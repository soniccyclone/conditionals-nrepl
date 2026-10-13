(ns nrepl.util.runtime-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [nrepl.misc :as misc]
   [nrepl.util.runtime :as runtime]
   [nrepl.version :as version])
  (:import
   (clojure.lang Compiler)
   (java.io StringWriter)))

(deftest host-identity
  (is (= :jvm runtime/host))
  (is (= (System/getProperty "java.version")
         (:version-string (runtime/host-version))))
  (is (= misc/java-version (:major (runtime/host-version))))
  (is (integer? runtime/java-version))
  (is (>= runtime/java-version 8)))

(deftest class-name-of-test
  (is (= "class clojure.lang.ExceptionInfo"
         (runtime/class-name-of (ex-info "x" {}))))
  (is (= "class java.lang.IllegalStateException"
         (runtime/class-name-of (IllegalStateException. "y")))))

(deftest print-stack-trace-test
  (let [w (StringWriter.)]
    (runtime/print-stack-trace (ex-info "kaboom" {}) w)
    (is (re-find #"clojure\.lang\.ExceptionInfo: kaboom" (str w)))))

(deftest load-file-bindings-test
  (let [m (runtime/load-file-bindings)]
    (is (= 4 (count m)))
    (is (= 0 (get m Compiler/NEXT_LOCAL_NUM)))
    (is (contains? m Compiler/METHOD))
    (is (contains? m Compiler/LOCAL_ENV))
    (is (contains? m Compiler/LOOP_LOCALS))))

(deftest can-stop-threads-test
  (is (boolean? (runtime/can-stop-threads?)))
  (is (= (boolean (or (<= runtime/java-version 19) (misc/jvmti-agent-enabled?)))
         (runtime/can-stop-threads?))))

(deftest environment-test
  (is (= (System/getProperty "user.home") (runtime/home-dir)))
  (is (nil? (runtime/getenv "NREPL_RUNTIME_TEST_NO_SUCH_VARIABLE")))
  (is (= (System/getenv "PATH") (runtime/getenv "PATH"))))

(deftest paths-test
  (testing "path-join nests, parent-dir undoes it"
    (is (= (io/file "a" "b") (runtime/path-join "a" "b")))
    (is (= (io/file "a") (runtime/parent-dir (runtime/path-join "a" "b")))))
  (is (= (io/file "x.edn") (runtime/path-join "x.edn")))
  (is (true? (runtime/path-exists? ".")))
  (is (false? (runtime/path-exists? "no/such/path/anywhere"))))

(deftest read-edn-file-test
  (let [f (java.io.File/createTempFile "runtime-test" ".edn")]
    (try
      (spit f "{:a 1 :b [2 3]}")
      (is (= {:a 1 :b [2 3]} (runtime/read-edn-file f)))
      (finally (.delete f)))))

(deftest version-string-test
  (is (string? (runtime/version-string)))
  (is (= (runtime/version-string) (:version-string version/version))))
