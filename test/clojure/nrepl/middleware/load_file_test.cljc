(ns nrepl.middleware.load-file-test
  {:author "Chas Emerick"}
  (:require
   [clojure.test :refer :all]
   [nrepl.core :as nrepl]
   [nrepl.core-test :refer [def-repl-test repl-server-fixture]]
   [nrepl.test-helpers :as th]))

(use-fixtures :each repl-server-fixture)

(def-repl-test load-code-with-debug-info
  (dorun (nrepl/message timeout-session
                        {:op "load-file" :file "\n\n\n(defn function [])"}))
  (is (contains?
       ;; different versions of Clojure use different default :file metadata
       #{[{:file "NO_SOURCE_PATH" :line 4}]
         [{:file "NO_SOURCE_FILE" :line 4}]}
       (repl-values timeout-session
                    (nrepl/code
                     (-> #'function
                         meta
                         (select-keys [:file :line]))))))
  (dorun (nrepl/message timeout-session {:op "load-file"
                                         :file "\n\n\n\n\n\n\n\n\n(defn afunction [])"
                                         :file-path "path/from/source/root.clj"
                                         :file-name "root.clj"}))
  (is (= [{:file "path/from/source/root.clj" :line 10}]
         (repl-values timeout-session
                      (nrepl/code
                       (-> #'afunction
                           meta
                           (select-keys [:file :line])))))))

(def-repl-test load-file-with-debug-info
  (dorun
   (nrepl/message timeout-session
                  {:op "load-file"
                   :file (slurp (th/project-file "load-file-test/nrepl/load_file_sample.clj"))
                   :file-path "nrepl/load_file_sample.clj"
                   :file-name "load_file_sample.clj"}))
  (is (= [{:file "nrepl/load_file_sample.clj" :line 5}]
         (repl-values timeout-session
                      (nrepl/code
                       (-> #'nrepl.load-file-sample/dfunction
                           meta
                           (select-keys [:file :line])))))))

(def-repl-test load-file-with-print-vars
  (set! *print-length* 3)
  (set! *print-level* 3)
  (dorun
   (nrepl/message session {:op "load-file"
                           :file "(def a (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6))))))
                                   (def b 2) (def c 3) (def ^{:internal true} d 4)"
                           :file-path "path/from/source/root.clj"
                           :file-name "root.clj"}))
  (is (= [4]
         (repl-values session (nrepl/code d)))))

(def-repl-test load-file-response-no-ns
  (is (not (contains? (nrepl/combine-responses
                       (nrepl/message session
                                      {:op "load-file"
                                       :file "(ns foo) (def x 5)"
                                       :file-path "/path/to/source.clj"
                                       :file-name "source.clj"}))
                      :ns))))

(def-repl-test load-file-huge-file
  (let [very-long-code (clojure.string/join "\n" (repeat 3000 "(+ 1 2 3 4 5 6 7 8 9 0)"))]
    (is (< 65536 (count very-long-code)))
    (is (= [45] (nrepl/response-values
                 (nrepl/message session
                                {:op "load-file"
                                 :file very-long-code
                                 :file-path "/path/to/source.clj"
                                 :file-name "source.clj"}))))))

(def-repl-test load-file-empty-file
  (let [resp (nrepl/combine-responses
              (nrepl/message session {:op "load-file"
                                      :file ""
                                      :file-path "/path/to/empty.clj"
                                      :file-name "empty.clj"}))]
    (testing "empty file completes without error"
      (is (some #{:done "done"} (:status resp)))
      (is (not (contains? resp :ex))))
    (testing "empty file produces no value"
      (is (not (contains? resp :value))))))

(def-repl-test load-file-only-comments
  (let [resp (nrepl/combine-responses
              (nrepl/message session {:op "load-file"
                                      :file ";; just a comment\n;; another comment\n"
                                      :file-path "/path/to/comments.clj"
                                      :file-name "comments.clj"}))]
    (testing "comments-only file completes without error"
      (is (some #{:done "done"} (:status resp)))
      (is (not (contains? resp :ex))))
    (testing "comments-only file produces no value"
      (is (not (contains? resp :value))))))

(def-repl-test load-file-lines-of-later-forms
  (dorun
   (nrepl/message session
                  {:op "load-file"
                   :file "(def first-form 1)\n\n(def second-form\n  2)\n(def third-form 3)"
                   :file-path "path/from/source/root.clj"
                   :file-name "root.clj"}))
  (is (= [[{:file "path/from/source/root.clj" :line 1}
           {:file "path/from/source/root.clj" :line 3}
           {:file "path/from/source/root.clj" :line 5}]]
         (repl-values session
                      (nrepl/code
                       (mapv #(select-keys (meta %) [:file :line])
                             [#'first-form #'second-form #'third-form]))))))

(def-repl-test load-file-reader-conditionals
  (let [resp (nrepl/combine-responses
              (nrepl/message session
                             {:op "load-file"
                              :file (str "(def shared-var #?(:clj :jvm :cljr :clr))\n"
                                         "#?(:cljr (def cljr-only-var (str (System.Math/Abs -3))))\n"
                                         "#?(:clj (def clj-only-var (str (Math/abs -3))))")
                              :file-path "path/from/source/root.cljc"
                              :file-name "root.cljc"}))]
    (is (not (contains? resp :ex)))
    (is (= [[#?(:clj :jvm :cljr :clr)
             #?(:clj nil :cljr "3")
             #?(:clj "3" :cljr nil)]]
           (repl-values session
                        (nrepl/code
                         [shared-var
                          (some-> (ns-resolve *ns* 'cljr-only-var) deref)
                          (some-> (ns-resolve *ns* 'clj-only-var) deref)]))))))

(def-repl-test load-file-compile-error
  (let [resp (nrepl/combine-responses
              (nrepl/message session
                             {:op "load-file"
                              :file "(def before-error 1)\n(defn bad [] (no-such-fn-xyz 1))\n(def after-error 2)"
                              :file-path "path/from/source/root.clj"
                              :file-name "root.clj"}))]
    (is (contains? (set (map keyword (:status resp))) :eval-error))
    (is (re-find #?(:clj #"^class clojure\.lang\.Compiler\$CompilerException$"
                    :cljr #"^clojure\.lang\.Compiler\+CompilerException$")
                 (:ex resp)))
    (is (= [[1 nil]]
           (repl-values session
                        (nrepl/code
                         [(some-> (ns-resolve *ns* 'before-error) deref)
                          (some-> (ns-resolve *ns* 'after-error) deref)]))))))

(def-repl-test load-file-bindings-do-not-leak-between-loads
  (let [source (fn [suffix]
                 (str "(defn nested" suffix " [a]\n"
                      "  (let [b (inc a)]\n"
                      "    (loop [i 0 acc b]\n"
                      "      (if (< i 3)\n"
                      "        (recur (inc i) (+ acc i))\n"
                      "        (fn [] (let [c (* 2 acc)] [a b c i acc]))))))\n"
                      "(def result" suffix " ((nested" suffix " 1)))\n"
                      "result" suffix))]
    (doseq [suffix ["-one" "-two"]]
      (let [resp (nrepl/combine-responses
                  (nrepl/message session
                                 {:op "load-file"
                                  :file (source suffix)
                                  :file-path "path/from/source/root.clj"
                                  :file-name "root.clj"}))]
        (is (not (contains? resp :ex)))
        (is (= ["[1 2 10 3 5]"] (:value resp)))))
    (is (= [[[1 2 10 3 5] [1 2 10 3 5]]]
           (repl-values session (nrepl/code [result-one result-two]))))
    (is (= [[7 2]]
           (repl-values session
                        (nrepl/code
                         (let [x 7]
                           (loop [i 0] (if (< i 2) (recur (inc i)) [x i])))))))))
