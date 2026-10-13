(ns nrepl.cmdline-options-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [nrepl.cmdline :as cmd]
   [nrepl.cmdline.options :as options]))

(deftest exit-data
  (let [e (try (options/exit 3) (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:nrepl/kind :nrepl.cmdline/exit :nrepl.cmdline/status 3}
           (ex-data e)))))

(deftest default-repl-fn
  (testing "the options half has no REPL of its own"
    (is (nil? (:repl-fn (options/connection-opts {}))))
    (is (nil? (:repl-fn (options/server-opts {})))))
  (testing "an entry point passes its default"
    (is (= #'identity (:repl-fn (options/connection-opts {} #'identity))))
    (is (= #'identity (:repl-fn (options/server-opts {} #'identity)))))
  (testing "an explicit :repl-fn wins"
    (is (= #'inc (:repl-fn (options/connection-opts {:repl-fn 'clojure.core/inc} #'identity)))))
  (testing "nrepl.cmdline supplies run-repl"
    (is (= #'nrepl.cmdline/run-repl (:repl-fn (cmd/connection-opts {}))))
    (is (= #'nrepl.cmdline/run-repl (:repl-fn (cmd/server-opts {}))))))

(deftest reexports
  (is (= (options/help) (cmd/help)))
  (is (identical? options/exit cmd/exit)))
