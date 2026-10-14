(ns loop-test
  "Runs one test namespace N times through kaocha inside this single JVM.

  Usage: clojure -M:1.12:dev plan/verify/loop_test.clj TEST-NS RUNS

  Prints progress, then a LOOP-RESULT line and, when a run failed, the output of
  the first failing run between LOOP-FIRST-FAILURE-BEGIN and
  LOOP-FIRST-FAILURE-END. Exits 1 when any run failed or errored."
  (:require [kaocha.api :as api]
            [kaocha.repl :as repl]
            [kaocha.result :as result]
            [clojure.string :as str])
  (:import (java.io StringWriter)))

(def reporting-plugins
  #{:kaocha.plugin/profiling :kaocha.plugin/junit-xml :kaocha.plugin/capture-output})

(def failure-events (atom []))

(defn failure-event? [{:keys [type actual]}]
  (or (#{:fail :error :kaocha.type.var/zero-assertions} type)
      (and (= :kaocha/begin-suite type) actual)))

(def current-var (atom nil))

(defn collect-failures [event]
  (when (= :begin-test-var (:type event))
    (reset! current-var (:var event)))
  (when (failure-event? event)
    (swap! failure-events conj (assoc event :in-var @current-var))))

(defn describe-failure [{:keys [type message expected actual in-var]}]
  (let [where (when in-var (str (:ns (meta in-var)) "/" (:name (meta in-var))))]
    (str (str/upper-case (name type)) " in " where "\n"
         (when (seq message) (str message "\n"))
         "expected: " (pr-str expected) "\n"
         "  actual: " (if (instance? Throwable actual)
                        (str/join "\n" (cons (str actual)
                                             (map str (take 15 (.getStackTrace ^Throwable actual)))))
                        (pr-str actual))
         "\n")))

(defn run-config [ns-sym]
  (-> (repl/config)
      (update :kaocha/plugins #(vec (remove reporting-plugins %)))
      (assoc :kaocha/color? false
             :kaocha/reporter [collect-failures])
      (update :kaocha.filter/focus (fnil conj []) (keyword (str ns-sym)))))

(defn run-once [config]
  (let [out (StringWriter.)
        _ (reset! failure-events [])
        outcome (try
                  (binding [*out* out
                            *err* out]
                    (-> (api/run config) :kaocha.result/tests result/totals))
                  (catch Throwable t
                    (.println System/err (str "run threw: " t))
                    {:kaocha.result/count 0
                     :kaocha.result/pass 0
                     :kaocha.result/fail 0
                     :kaocha.result/error 1
                     :thrown (str t)}))]
    {:totals outcome
     :output (str (str/join "\n" (map describe-failure @failure-events))
                  (when (seq (str out)) (str "--- test output\n" (str out))))
     :failed? (or (pos? (:kaocha.result/fail outcome 0))
                  (pos? (:kaocha.result/error outcome 0))
                  (zero? (:kaocha.result/count outcome 0)))}))

(defn -main [ns-name runs-text]
  (let [ns-sym (symbol ns-name)
        runs (Long/parseLong runs-text)
        config (run-config ns-sym)
        started (System/nanoTime)
        step (max 1 (quot runs 10))
        failing (volatile! 0)
        shape (volatile! nil)
        first-failure (volatile! nil)]
    (dotimes [i runs]
      (let [run-number (inc i)
            {:keys [failed? output totals]} (run-once config)]
        (when-not @shape
          (vreset! shape (select-keys totals [:kaocha.result/count :kaocha.result/pass])))
        (when failed?
          (vswap! failing inc)
          (when-not @first-failure
            (vreset! first-failure {:run run-number :output output :totals totals})))
        (when (or (zero? (mod run-number step)) (= run-number runs))
          (println (format "LOOP-PROGRESS run %d/%d, failing runs so far %d, %.1f s elapsed"
                           run-number runs @failing (/ (- (System/nanoTime) started) 1e9))))))
    (println (format "LOOP-RESULT ns=%s tests-per-run=%s assertions-per-run=%s runs=%d failing-runs=%d first-failing-run=%s seconds=%.1f"
                     ns-name (:kaocha.result/count @shape) (:kaocha.result/pass @shape)
                     runs @failing (or (:run @first-failure) "none")
                     (/ (- (System/nanoTime) started) 1e9)))
    (when-let [{:keys [output totals]} @first-failure]
      (println "LOOP-FIRST-FAILURE-BEGIN")
      (println (pr-str totals))
      (println output)
      (println "LOOP-FIRST-FAILURE-END"))
    (flush)
    (shutdown-agents)
    (System/exit (if (zero? @failing) 0 1))))

(apply -main *command-line-args*)
