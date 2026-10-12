(ns nrepl.transport-test
  (:require [nrepl.transport :as sut]
            [clojure.test :refer [deftest testing is]])
  (:import (java.io BufferedInputStream BufferedOutputStream
                    ByteArrayInputStream ByteArrayOutputStream)))

(deftest bencode-safe-write-test
  (testing "safe-write-bencode only writes if the whole message is writable"
    (let [out (ByteArrayOutputStream.)]
      (is (thrown? IllegalArgumentException
                   (#'sut/safe-write-bencode out {"obj" (Object.)})))
      (is (empty? (.toByteArray out))))))

(deftest malformed-bencode-input-test
  (testing "if non-bencode input is passed, throw an informative error"
    (let [in (-> (.getBytes "123456789123456789123456789")
                 ByteArrayInputStream.
                 BufferedInputStream.)
          out (BufferedOutputStream. (ByteArrayOutputStream.))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^nREPL message must be a map."
                            (sut/recv (sut/bencode in out))))))

  (testing "if top-level message is not a map, throw an informative error"
    (let [in (-> (.getBytes "li1ei2ei3ee")
                 ByteArrayInputStream.
                 BufferedInputStream.)
          out (BufferedOutputStream. (ByteArrayOutputStream.))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^nREPL message must be a map."
                            (sut/recv (sut/bencode in out)))))))

(deftest safe-handle-error-reply-test
  (testing "a handler throwing ex-info is answered with its ex-message and an -error status"
    (let [sent (atom [])
          transport (sut/->FnTransport (fn [_timeout]) #(swap! sent conj %) (fn []))
          e (ex-info "boom" {:cause :test})]
      (sut/safe-handle {:op "explode" :id "1" :transport transport}
        "explode" (fn [_msg] (throw e)))
      (is (= [{:id "1"
               :status #{:explode-error :error :done}
               :message (ex-message e)}]
             @sent)))))
