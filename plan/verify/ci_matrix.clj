(ns ci-matrix
  "Reads .github/workflows/ci.yml and writes, for every ubuntu leg of the jobs
  jvm, jvm-macos, lint, clr and clr-full, the step scripts the workflow runs
  plus one row in legs.tsv.

  Usage: clojure -M ci_matrix.clj WORKFLOW-DIR OUT-DIR

  Every step script is the workflow step's own `run` text with the supported
  ${{ }} expressions substituted. An unsupported expression, a missing step or a
  duplicate leg name aborts with a message."
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def temp-dir "/tmp")

(defn fail [& parts]
  (binding [*out* *err*]
    (println (apply str "ci-matrix: " parts)))
  (System/exit 1))

(defn load-workflow [dir name]
  (yaml/parse-string (slurp (io/file dir name))))

(defn substitute [context text]
  (str/replace text #"\$\{\{\s*(.*?)\s*\}\}"
               (fn [[whole expr]]
                 (let [value (get context expr)]
                   (when (nil? value)
                     (fail "unsupported expression " whole))
                   (str value)))))

(defn step-named [job name]
  (or (some #(when (= name (:name %)) %) (:steps job))
      (fail "no step named " (pr-str name))))

(defn runnable-steps-after [job name]
  (->> (:steps job)
       (drop-while #(not= name (:name %)))
       rest
       (filter :run)))

(defn shell-quote [s]
  (str "'" (str/replace (str s) "'" "'\\''") "'"))

(defn substitute-env [context env]
  (into (sorted-map)
        (map (fn [[k v]] [(name k) (substitute context (str v))]))
        env))

(defn step-script [context env step]
  (let [env (substitute-env context (merge env (:env step)))]
    (str "echo " (shell-quote (str "==> " (:name step))) "\n"
         (str/join "" (for [[k v] env] (str "export " k "=" (shell-quote v) "\n")))
         (substitute context (:run step))
         "\n")))

(defn write-scripts! [out-dir leg phase steps context env]
  (let [dir (io/file out-dir leg)]
    (.mkdirs dir)
    (vec (for [[i step] (map-indexed vector steps)]
           (let [file-name (format "%s-%02d.sh" phase i)]
             (spit (io/file dir file-name) (step-script context env step))
             file-name)))))

(defn ubuntu? [entry]
  (str/starts-with? (:os entry) "ubuntu"))

(defn row [& kvs]
  (str/join "\t" (map (fn [[k v]] (str (name k) "=" v)) (partition 2 kvs))))

(defn cli-version [job]
  (or (some #(get-in % [:with :cli]) (:steps job))
      (fail "no setup-clojure cli version")))

(defn java-version [job]
  (or (some #(get-in % [:with :java-version]) (:steps job))
      (fail "no setup-java java-version")))

(defn jvm-legs [workflow job-key out-dir]
  (let [job (or (get-in workflow [:jobs job-key]) (fail "no job " (name job-key)))
        prepare (step-named job "Prepare dependencies")
        run (step-named job "Run the tests")
        cli (cli-version job)]
    (for [entry (filter ubuntu? (get-in job [:strategy :matrix :include]))]
      (let [leg (str (:clojure entry) "-temurin-" (:jdk entry))
            context {"matrix.clojure" (:clojure entry)
                     "matrix.jdk" (:jdk entry)
                     "matrix.aliases" (:aliases entry)
                     "matrix.os" (:os entry)
                     "runner.temp" temp-dir}
            env (merge (:env workflow) (:env job))]
        {:leg leg
         :phases {:prepare (write-scripts! out-dir leg "prepare" [prepare] context env)
                  :run (write-scripts! out-dir leg "run" [run] context env)}
         :row (row :kind "jvm" :leg leg :jdk (:jdk entry) :clojure (:clojure entry)
                   :aliases (:aliases entry) :cli cli :informational "false")}))))

(defn lint-leg [workflow out-dir]
  (let [job (get-in workflow [:jobs :lint])
        context {"runner.temp" temp-dir "matrix.os" "ubuntu-latest"}
        env (merge (:env workflow) (:env job))
        prepare (step-named job "Prepare dependencies")
        run (runnable-steps-after job "Prepare dependencies")]
    {:leg "lint"
     :phases {:prepare (write-scripts! out-dir "lint" "prepare" [prepare] context env)
              :run (write-scripts! out-dir "lint" "run" run context env)}
     :row (row :kind "lint" :leg "lint" :jdk (java-version job)
               :clojure (or (:CLOJURE_VERSION env) (fail "lint job sets no CLOJURE_VERSION"))
               :aliases "" :cli (cli-version job) :informational "false")}))

(defn tool-version [install-text tool]
  (let [pattern (re-pattern (str "dotnet tool install --global "
                                 (java.util.regex.Pattern/quote tool)
                                 " --version (\\S+)"))]
    (or (second (re-find pattern install-text))
        (fail "cannot find the " tool " version in the install step"))))

(defn clr-legs [workflow job-key out-dir]
  (let [job (or (get-in workflow [:jobs job-key]) (fail "no job " (name job-key)))
        install (:run (step-named job "Install ClojureCLR and cljr"))
        main-version (tool-version install "Clojure.Main")
        cljr-version (tool-version install "Clojure.Cljr")
        prepare (step-named job "Fetch dependencies")
        run (mapv #(step-named job %) ["Show the runtime ClojureCLR runs on"
                                       "Build the C# helpers"
                                       "Run the tests"])]
    (for [entry (filter ubuntu? (get-in job [:strategy :matrix :include]))]
      (let [leg (str "clr-net" (first (str/split (:dotnet entry) #"\.")))
            context {"matrix.dotnet" (:dotnet entry)
                     "matrix.os" (:os entry)
                     "matrix.role" (:role entry)
                     "runner.temp" temp-dir}
            env {:RUNNER_TEMP temp-dir :RUNNER_OS "Linux"}]
        {:leg leg
         :phases {:prepare (write-scripts! out-dir leg "prepare" [prepare] context env)
                  :run (write-scripts! out-dir leg "run" run context env)}
         :row (row :kind "clr" :leg leg :dotnet (:dotnet entry)
                   :clojure-main main-version :cljr cljr-version
                   :informational (:informational entry))}))))

(defn -main [workflow-dir out-dir]
  (let [ci (load-workflow workflow-dir "ci.yml")
        legs (concat (jvm-legs ci :jvm out-dir)
                     (jvm-legs ci :jvm-macos out-dir)
                     [(lint-leg ci out-dir)]
                     (clr-legs ci :clr out-dir)
                     (clr-legs ci :clr-full out-dir))
        names (map :leg legs)]
    (when-let [dupes (seq (for [[n c] (frequencies names) :when (> c 1)] n))]
      (fail "duplicate leg names: " (str/join ", " dupes)))
    (spit (io/file out-dir "legs.tsv")
          (str (str/join "\n" (map :row legs)) "\n"))
    (doseq [{:keys [leg phases]} legs]
      (spit (io/file out-dir leg "phases.tsv")
            (str (str/join "\n" (for [[phase files] phases, f files]
                                  (str (name phase) "\t" f)))
                 "\n")))))

(apply -main *command-line-args*)
