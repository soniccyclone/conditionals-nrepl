(ns nrepl.transport-test
  (:require [nrepl.socket :as socket]
            [nrepl.transport :as sut]
            [clojure.test :refer [deftest testing is]])
  (:import (java.io BufferedInputStream BufferedOutputStream
                    ByteArrayInputStream ByteArrayOutputStream
                    IOException InputStream)
           (java.net InetAddress ServerSocket Socket)
           (java.nio.channels ClosedChannelException)))

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

(defn- recv-error
  ([make-transport input]
   (recv-error make-transport input nil))
  ([make-transport input s]
   (let [in (BufferedInputStream.
             (if (string? input)
               (ByteArrayInputStream. (.getBytes ^String input "UTF-8"))
               input))
         out (BufferedOutputStream. (ByteArrayOutputStream.))]
     (try
       (sut/recv (make-transport in out s))
       nil
       (catch Throwable e e)))))

(defn- failing-input-stream
  ^InputStream [^Throwable e]
  (proxy [InputStream] []
    (read
      ([] (throw e))
      ([_buf] (throw e))
      ([_buf _off _len] (throw e)))))

(deftest truncated-input-is-a-disconnection-test
  (testing "bencode input that ends at the first byte is reported as a lost connection"
    (let [e (recv-error sut/bencode "")]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "bencode input that ends inside a map is reported as a lost connection"
    (let [e (recv-error sut/bencode "d3:foo")]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "EDN input that ends mid-form is reported as a lost connection"
    (let [e (recv-error sut/edn "{:op ")]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e))))))

(deftest failing-read-is-a-disconnection-test
  (testing "a read on a closed channel is reported as a lost connection"
    (let [e (recv-error sut/bencode (failing-input-stream (ClosedChannelException.)))]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "another read failure is a lost connection when the socket is not connected"
    (let [e (recv-error sut/bencode (failing-input-stream (IOException. "boom")) (Socket.))]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "another read failure is rethrown unchanged when the socket is still connected"
    (let [cause (IOException. "boom")]
      (with-open [server (ServerSocket. 0 1 (InetAddress/getLoopbackAddress))
                  client (Socket. (InetAddress/getLoopbackAddress) (.getLocalPort server))]
        (is (identical? cause (recv-error sut/bencode (failing-input-stream cause) client))))))

  (testing "another read failure is rethrown unchanged when there is no socket to blame"
    (let [cause (IOException. "boom")]
      (is (identical? cause (recv-error sut/bencode (failing-input-stream cause)))))))

(deftest non-map-bencode-message-is-not-a-disconnection-test
  (testing "a well-formed bencode list is rejected as not a message"
    (let [e (recv-error sut/bencode "li1ee")]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (re-find #"^nREPL message must be a map" (ex-message e)))))

  (testing "a netstring at the top level is rejected as not a message, not as a lost connection"
    (let [e (recv-error sut/bencode "5:abc")]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (re-find #"^nREPL message must be a map" (ex-message e))))))

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
