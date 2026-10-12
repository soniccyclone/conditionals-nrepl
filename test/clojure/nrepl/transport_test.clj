(ns nrepl.transport-test
  (:require [nrepl.socket :as socket]
            [nrepl.transport :as sut]
            [nrepl.util.threading :as threading]
            [clojure.test :refer [deftest testing is]])
  (:import (java.io BufferedInputStream BufferedOutputStream
                    ByteArrayInputStream ByteArrayOutputStream
                    Closeable IOException InputStream)
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

  (testing "bencode input that ends inside a netstring is reported as a lost connection"
    (let [e (recv-error sut/bencode "d3:foo3:ba")]
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

  (testing "a RuntimeException is a lost connection when the socket is not connected"
    (let [e (recv-error sut/bencode (failing-input-stream (RuntimeException. "boom")) (Socket.))]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "a RuntimeException is rethrown unchanged when there is no socket to blame"
    (let [cause (RuntimeException. "boom")]
      (is (identical? cause (recv-error sut/bencode (failing-input-stream cause))))))

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

(defn- fn-transport-of
  [transport-read]
  (sut/fn-transport transport-read (fn [_]) (fn [])))

(deftest rendezvous-backpressure
  (testing "the reader loop reads ahead by nothing: one read per received message"
    (let [calls (atom 0)
          transport (fn-transport-of #(hash-map :n (swap! calls inc)))]
      (try
        (Thread/sleep 200)
        (is (= 1 @calls))
        (is (= {:n 1} (sut/recv transport 2000)))
        (Thread/sleep 200)
        (is (= 2 @calls))
        (finally
          (.close ^Closeable transport))))))

(deftest recv-timeout-keeps-the-message
  (testing "a recv that times out does not lose the message read in the meantime"
    (let [calls (atom 0)
          transport (fn-transport-of (fn []
                                       (if (= 1 (swap! calls inc))
                                         (do (Thread/sleep 300) {:n 1})
                                         (Thread/sleep 60000))))]
      (try
        (is (nil? (sut/recv transport 50)))
        (is (= {:n 1} (sut/recv transport 5000)))
        (finally
          (.close ^Closeable transport))))))

(deftest reader-failure-is-sticky
  (testing "a failing read is rethrown by every recv"
    (let [failure (ex-info "boom" {})
          transport (fn-transport-of #(throw failure))]
      (try
        (is (identical? failure (try (sut/recv transport 2000) (catch Throwable e e))))
        (is (identical? failure (try (sut/recv transport 2000) (catch Throwable e e))))
        (finally
          (.close ^Closeable transport)))))

  (testing "the reader loop does not read again after a failure"
    (let [counters (repeatedly 200 #(atom 0))
          transports (mapv (fn [calls]
                             (fn-transport-of #(do (swap! calls inc)
                                                   (throw (ex-info "boom" {})))))
                           counters)]
      (try
        (run! #(try (sut/recv % 2000) (catch Throwable _)) transports)
        (Thread/sleep 200)
        (is (= (repeat 200 1) (map deref counters)))
        (finally
          (run! #(.close ^Closeable %) transports))))))

(deftest close-stops-the-reader-loop
  (testing "closing drops a message the reader loop is blocked on handing over"
    (let [calls (atom 0)
          transport (fn-transport-of #(hash-map :n (swap! calls inc)))]
      (is (= {:n 1} (sut/recv transport 2000)))
      (Thread/sleep 200)
      (.close ^Closeable transport)
      (Thread/sleep 200)
      (is (nil? (sut/recv transport 300)))
      (is (= 2 @calls))))

  (testing "closing interrupts a reader loop that is blocked in the read"
    (let [interrupted (promise)
          calls (atom 0)
          transport (fn-transport-of (fn []
                                       (swap! calls inc)
                                       (try (Thread/sleep 60000)
                                            (catch InterruptedException e
                                              (deliver interrupted true)
                                              (throw e)))))]
      (Thread/sleep 100)
      (.close ^Closeable transport)
      (is (true? (deref interrupted 2000 false)))
      (Thread/sleep 300)
      (is (= 1 @calls)))))

(deftest infinite-timeout-is-accepted
  (testing "recv accepts the infinite timeout, with and without an argument"
    (let [transport (fn-transport-of #(do (Thread/sleep 100) {:n 1}))]
      (try
        (is (= {:n 1} (sut/recv transport threading/infinite-ms)))
        (is (= {:n 1} (sut/recv transport)))
        (finally
          (.close ^Closeable transport))))))
