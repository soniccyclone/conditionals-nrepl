(ns nrepl.util.runtime.jvm
  "internal host seam"
  {:no-doc true}
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [nrepl.util.io :as nio])
  (:import
   (clojure.lang Compiler)
   (java.io PrintWriter Reader Writer)
   (java.util Properties)))

(def host :jvm)

(defn parse-java-version
  []
  (try (let [s (System/getProperty "java.specification.version")
             [major minor _] (str/split s #"\.")
             major (Integer/parseInt major)]
         (if (> major 1)
           major
           (Integer/parseInt minor)))
       (catch Exception _ 8)))

(def java-version (parse-java-version))

(defn host-version
  []
  {:major java-version
   :version-string (System/getProperty "java.version")})

(defn attach-self-enabled?
  []
  (boolean (System/getProperty "jdk.attach.allowAttachSelf")))

(defn class-name-of
  [e]
  (str (class e)))

(defn print-stack-trace
  [e w]
  (.printStackTrace ^Throwable e (PrintWriter. ^Writer w)))

(defn load-file-bindings
  []
  {Compiler/METHOD nil
   Compiler/LOCAL_ENV nil
   Compiler/LOOP_LOCALS nil
   Compiler/NEXT_LOCAL_NUM 0})

(defn can-stop-threads?
  []
  (boolean (or (<= java-version 19)
               ((requiring-resolve 'nrepl.misc/jvmti-agent-enabled?)))))

(defn exit!
  [status]
  (System/exit status))

(defn block-forever!
  []
  (.join (Thread/currentThread)))

(defmacro ^:private install-sigint-handler
  [f]
  (if (try (Class/forName "sun.misc.Signal")
           (catch Throwable _e))
    `(try
       (sun.misc.Signal/handle
        (sun.misc.Signal. "INT")
        (proxy [sun.misc.SignalHandler] []
          (handle [signal#] (~f signal#))))
       (catch Throwable e#))
    `(println "Unable to set signal handlers.")))

(defn on-sigint!
  [f]
  (install-sigint-handler f))

(defn delete-on-exit!
  [path]
  (.deleteOnExit (io/file path)))

(defn home-dir
  []
  (System/getProperty "user.home"))

(defn getenv
  [k]
  (System/getenv k))

(defn path-join
  ([path]
   (io/file path))
  ([parent child]
   (io/file parent child)))

(defn path-exists?
  [path]
  (.exists (io/file path)))

(defn parent-dir
  [path]
  (.getParentFile (io/file path)))

(defn read-edn-file
  [path]
  (with-open [^Reader r (nio/text-reader path)]
    (edn/read (nio/pushback-reader r))))

(def ^:private properties-filename
  "META-INF/maven/nrepl/nrepl/pom.properties")

(defn- map-from-property-filepath
  [file]
  (try
    (with-open [rdr (io/reader (io/resource file))]
      (let [props (Properties.)]
        (.load props rdr)
        (into {} props)))
    (catch Exception _e nil)))

(defn version-string
  []
  (or (System/getProperty "nrepl.version")
      (get (map-from-property-filepath properties-filename) "version")
      ""))
