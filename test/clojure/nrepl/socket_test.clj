(ns nrepl.socket-test
  (:require [clojure.test :refer [deftest testing is]]
            [nrepl.socket :as sut])
  (:import (java.io IOException)
           (java.net SocketException)
           (java.nio.channels ClosedChannelException)))

(deftest disconnected-exception-test
  (testing "disconnected-exception builds an exception that disconnected? recognises"
    (let [e (sut/disconnected-exception "gone")]
      (is (sut/disconnected? e))
      (is (= "gone" (ex-message e)))))

  (testing "disconnected? is false for other exceptions"
    (is (not (sut/disconnected? (IOException. "gone"))))
    (is (not (sut/disconnected? (ClosedChannelException.))))
    (is (not (sut/disconnected? nil)))))

(deftest closed-exception-test
  (testing "closed-exception? recognises the exception accept throws on a closed listener"
    (is (sut/closed-exception? (ClosedChannelException.))))

  (testing "closed-exception? is false for other exceptions"
    (is (not (sut/closed-exception? (SocketException. "gone"))))
    (is (not (sut/closed-exception? (sut/disconnected-exception "gone"))))
    (is (not (sut/closed-exception? nil)))))
