(ns nrepl.util.runtime-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [nrepl.misc :as misc]
   [nrepl.test-helpers.bounded :as bounded]
   [nrepl.util.io :as nio]
   [nrepl.util.runtime :as runtime]
   [nrepl.version :as version])
  #?@(:clj [(:require [clojure.java.io :as io])
            (:import (clojure.lang Compiler))]
      :cljr [(:import
              (clojure.lang Compiler)
              (System.Diagnostics Process ProcessStartInfo)
              (System.IO DirectoryInfo File FileInfo Path)
              (System.Reflection BindingFlags)
              (System.Runtime.InteropServices OSPlatform RuntimeInformation))]))

(defn- temp-file
  [content]
  #?(:clj (let [f (java.io.File/createTempFile "runtime-test" ".edn")]
            (spit f content)
            f)
     :cljr (let [path (Path/GetTempFileName)]
             (File/WriteAllText path content)
             path)))

(defn- delete-file
  [f]
  #?(:clj (.delete ^java.io.File f)
     :cljr (File/Delete (str f))))

(deftest host-identity
  #?(:clj (do
            (is (= :jvm runtime/host))
            (is (= (System/getProperty "java.version")
                   (:version-string (runtime/host-version))))
            (is (= misc/java-version (:major (runtime/host-version))))
            (is (integer? runtime/java-version))
            (is (>= runtime/java-version 8)))
     :cljr (do
             (is (= :clr runtime/host))
             (is (= (RuntimeInformation/FrameworkDescription)
                    (:version-string (runtime/host-version))))
             (is (= (.Major Environment/Version) (:major (runtime/host-version))))
             (is (>= (:major (runtime/host-version)) 8))
             (is (nil? runtime/java-version))
             (is (false? (runtime/attach-self-enabled?))))))

(deftest class-name-of-test
  (is (= #?(:clj "class clojure.lang.ExceptionInfo" :cljr "clojure.lang.ExceptionInfo")
         (runtime/class-name-of (ex-info "x" {}))))
  (is (= #?(:clj "class java.lang.IllegalStateException"
            :cljr "System.InvalidOperationException")
         (runtime/class-name-of #?(:clj (IllegalStateException. "y")
                                   :cljr (InvalidOperationException. "y"))))))

(defn- thrown
  []
  (try (throw #?(:clj (IllegalStateException. "kaboom") :cljr (InvalidOperationException. "kaboom")))
       (catch Exception e e)))

(deftest print-stack-trace-test
  (let [w (nio/string-writer)]
    (runtime/print-stack-trace (ex-info "kaboom" {}) w)
    (is (re-find #"clojure\.lang\.ExceptionInfo: kaboom" (nio/writer->string w))))
  (testing "a thrown exception prints its frames"
    (let [w (nio/string-writer)]
      (runtime/print-stack-trace (thrown) w)
      (is (re-find #"\sat\s" (nio/writer->string w))))))

#?(:cljr
   (defn- compiler-var
     [^String field-name]
     (.GetValue (.GetField Compiler field-name
                           (enum-or BindingFlags/NonPublic BindingFlags/Static))
                nil)))

(deftest load-file-bindings-test
  (let [m (runtime/load-file-bindings)]
    (is (= 4 (count m)))
    (is (every? var? (keys m)))
    #?(:clj (do
              (is (= 0 (get m Compiler/NEXT_LOCAL_NUM)))
              (is (contains? m Compiler/METHOD))
              (is (contains? m Compiler/LOCAL_ENV))
              (is (contains? m Compiler/LOOP_LOCALS)))
       :cljr (do
               (is (= 0 (get m (compiler-var "NextLocalNumVar"))))
               (is (contains? m (compiler-var "MethodVar")))
               (is (contains? m Compiler/LocalEnvVar))
               (is (contains? m (compiler-var "LoopLocalsVar")))
               (is (= 3 (count (filter nil? (vals m)))))))
    (testing "the map can be bound around an eval"
      (is (= 3 (with-bindings m (eval '(let [x 1 y 2] (+ x y)))))))))

(deftest can-stop-threads-test
  (is (boolean? (runtime/can-stop-threads?)))
  #?(:clj (is (= (boolean (or (<= runtime/java-version 19) (misc/jvmti-agent-enabled?)))
                 (runtime/can-stop-threads?)))
     :cljr (is (true? (runtime/can-stop-threads?)))))

(deftest block-forever-test
  (let [done (promise)
        t (bounded/start-thread
           (fn []
             (try (runtime/block-forever!)
                  (catch #?(:clj InterruptedException :cljr System.Threading.ThreadInterruptedException) _
                    (deliver done :interrupted)))))]
    (bounded/sleep-ms 300)
    (is (bounded/thread-alive? t))
    (is (not (realized? done)))
    (bounded/interrupt-thread! t)
    (is (= :interrupted (deref done bounded/wait-ms :timeout)))))

#?(:cljr
   (defn- run-clojure
     [form]
     (let [info (ProcessStartInfo. "Clojure.Main")]
       (.Add (.ArgumentList info) "-e")
       (.Add (.ArgumentList info) (str "(require 'nrepl.util.runtime) " form))
       (set! (.UseShellExecute info) false)
       (with-open [p (Process/Start info)]
         (when-not (.WaitForExit p 120000)
           (.Kill p))
         (.ExitCode p)))))

#?(:cljr
   (deftest process-hooks-test
     (testing "exit! ends the process with the status"
       (is (= 7 (run-clojure "(nrepl.util.runtime/exit! 7)"))))
     (testing "delete-on-exit! removes the file when the process ends"
       (let [path (temp-file "x")]
         (try
           (is (= 0 (run-clojure (str "(nrepl.util.runtime/delete-on-exit! " (pr-str path) ")"))))
           (is (not (File/Exists path)))
           (finally (File/Delete path)))))
     (testing "a file that is already gone does not fail the exit"
       (is (= 0 (run-clojure (str "(nrepl.util.runtime/delete-on-exit! "
                                  (pr-str (str (Path/GetTempPath) "no-such-nrepl-file"))
                                  ")")))))))

#?(:cljr
   (deftest on-sigint-test
     (when-not (RuntimeInformation/IsOSPlatform OSPlatform/Windows)
       (let [seen (promise)
             handler (runtime/on-sigint! (fn [args] (deliver seen (.Cancel ^System.ConsoleCancelEventArgs args))))]
         (is (instance? System.ConsoleCancelEventHandler handler))
         (try
           (let [info (ProcessStartInfo. "/bin/sh")]
             (.Add (.ArgumentList info) "-c")
             (.Add (.ArgumentList info) (str "kill -INT " (.Id (Process/GetCurrentProcess))))
             (set! (.UseShellExecute info) false)
             (with-open [p (Process/Start info)]
               (.WaitForExit p 10000)))
           (testing "f runs, and the event is cancelled so the process survives"
             (is (true? (deref seen bounded/wait-ms :timeout))))
           (finally
             (Console/remove_CancelKeyPress handler)))))))

(deftest environment-test
  #?(:clj (do
            (is (= (System/getProperty "user.home") (runtime/home-dir)))
            (is (nil? (runtime/getenv "NREPL_RUNTIME_TEST_NO_SUCH_VARIABLE")))
            (is (= (System/getenv "PATH") (runtime/getenv "PATH"))))
     :cljr (do
             (is (= (Environment/GetEnvironmentVariable "HOME") (runtime/home-dir)))
             (is (nil? (runtime/getenv "NREPL_RUNTIME_TEST_NO_SUCH_VARIABLE")))
             (try
               (Environment/SetEnvironmentVariable "NREPL_RUNTIME_TEST_VARIABLE" "value")
               (is (= "value" (runtime/getenv "NREPL_RUNTIME_TEST_VARIABLE")))
               (finally
                 (Environment/SetEnvironmentVariable "NREPL_RUNTIME_TEST_VARIABLE" nil)))
             (is (nil? (runtime/getenv "NREPL_RUNTIME_TEST_VARIABLE"))))))

(deftest paths-test
  #?(:clj (do
            (testing "path-join nests, parent-dir undoes it"
              (is (= (io/file "a" "b") (runtime/path-join "a" "b")))
              (is (= (io/file "a") (runtime/parent-dir (runtime/path-join "a" "b")))))
            (is (= (io/file "x.edn") (runtime/path-join "x.edn"))))
     :cljr (let [sep (str Path/DirectorySeparatorChar)
                 ab (runtime/path-join "a" "b")]
             (testing "path-join nests, parent-dir undoes it"
               (is (instance? FileInfo ab))
               (is (= (str "a" sep "b") (str ab)))
               (is (instance? DirectoryInfo (runtime/parent-dir ab)))
               (is (= "a" (str (runtime/parent-dir ab)))))
             (testing "a path-join result is a valid parent"
               (is (= (str "a" sep "b" sep "c") (str (runtime/path-join ab "c")))))
             (testing "a nil parent leaves the child alone"
               (is (= "x.edn" (str (runtime/path-join nil "x.edn")))))
             (is (= "x.edn" (str (runtime/path-join "x.edn"))))
             (is (nil? (runtime/parent-dir "/")))))
  (is (nil? (runtime/parent-dir (runtime/path-join "x.edn"))))
  (is (true? (runtime/path-exists? ".")))
  (is (true? (runtime/path-exists? (runtime/path-join "."))))
  (is (false? (runtime/path-exists? "no/such/path/anywhere")))
  (let [f (temp-file "x")]
    (try
      (is (true? (runtime/path-exists? f)))
      #?(:cljr (is (true? (runtime/path-exists? (str f)))))
      (finally (delete-file f)))))

(deftest read-edn-file-test
  (let [f (temp-file "{:a 1 :b [2 3]}")]
    (try
      (is (= {:a 1 :b [2 3]} (runtime/read-edn-file f)))
      #?(:cljr (is (= {:a 1 :b [2 3]} (runtime/read-edn-file (runtime/path-join (str f))))))
      (finally (delete-file f)))))

(deftest version-string-test
  (is (string? (runtime/version-string)))
  (is (= (runtime/version-string) (:version-string version/version)))
  #?(:cljr
     (try
       (AppContext/SetData "nrepl.version" "7.8.9")
       (is (= "7.8.9" (runtime/version-string)))
       (AppContext/SetData "nrepl.version" "")
       (is (= "" (runtime/version-string)))
       (finally
         (AppContext/SetData "nrepl.version" nil)))))
