(ns nrepl.transport-test
  (:require [clojure.test :refer [deftest testing is]]
            [nrepl.core :as nrepl]
            [nrepl.server :as server]
            [nrepl.socket :as socket]
            [nrepl.test-helpers :as th]
            [nrepl.transport :as sut]
            [nrepl.util.io :as io]
            [nrepl.util.threading :as threading])
  #?(:clj (:import (java.io BufferedInputStream BufferedOutputStream
                            ByteArrayInputStream ByteArrayOutputStream
                            IOException InputStream)
                   (java.net Socket)
                   (java.nio.channels ClosedChannelException))
     :cljr (:import (System.IO BufferedStream IOException MemoryStream Stream)
                    (System.Net.Sockets ProtocolType Socket SocketType))))

(defn- input-stream-of
  [^bytes bs]
  #?(:clj (BufferedInputStream. (ByteArrayInputStream. bs))
     :cljr (BufferedStream. (MemoryStream. bs))))

(defn- buffered-input-of
  [in]
  #?(:clj (BufferedInputStream. ^InputStream in)
     :cljr (BufferedStream. ^Stream in)))

(defn- output-stream []
  #?(:clj (BufferedOutputStream. (ByteArrayOutputStream.))
     :cljr (BufferedStream. (MemoryStream.))))

(defn- closed-channel-exception []
  #?(:clj (ClosedChannelException.)
     :cljr (ObjectDisposedException. "closed")))

(defn- runtime-exception [msg]
  (new #?(:clj RuntimeException :cljr Exception) ^String msg))

(defn- unconnected-socket []
  #?(:clj (Socket.)
     :cljr (Socket. SocketType/Stream ProtocolType/Tcp)))

(deftest bencode-safe-write-test
  (testing "safe-write-bencode only writes if the whole message is writable"
    (let [out (io/byte-sink)]
      (is (thrown? #?(:clj IllegalArgumentException :cljr ArgumentException)
                   (#'sut/safe-write-bencode out {"obj" (Object.)})))
      (is (empty? (io/sink->bytes out))))))

(deftest malformed-bencode-input-test
  (testing "if non-bencode input is passed, throw an informative error"
    (let [in (input-stream-of (io/utf8-bytes "123456789123456789123456789"))
          out (output-stream)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^nREPL message must be a map."
                            (sut/recv (sut/bencode in out))))))

  (testing "if top-level message is not a map, throw an informative error"
    (let [in (input-stream-of (io/utf8-bytes "li1ei2ei3ee"))
          out (output-stream)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"^nREPL message must be a map."
                            (sut/recv (sut/bencode in out)))))))

(defn- recv-error
  ([make-transport input]
   (recv-error make-transport input nil))
  ([make-transport input s]
   (let [in (if (string? input)
              (input-stream-of (io/utf8-bytes input))
              (buffered-input-of input))
         out (output-stream)]
     (try
       (sut/recv (make-transport in out s))
       nil
       (catch #?(:clj Throwable :cljr Exception) e e)))))

#?(:clj
   (defn- failing-input-stream
     ^InputStream [^Throwable e]
     (proxy [InputStream] []
       (read
         ([] (throw e))
         ([_buf] (throw e))
         ([_buf _off _len] (throw e)))))
   :cljr
   (defn- failing-input-stream
     ^Stream [^Exception e]
     (proxy [Stream] []
       (get_CanRead [] true)
       (get_CanSeek [] false)
       (get_CanWrite [] false)
       (Flush [] nil)
       (Read [& _] (throw e)))))

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
    (let [e (recv-error sut/bencode (failing-input-stream (closed-channel-exception)))]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "another read failure is a lost connection when the socket is not connected"
    (let [e (recv-error sut/bencode (failing-input-stream (IOException. "boom")) (unconnected-socket))]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "a RuntimeException is a lost connection when the socket is not connected"
    (let [e (recv-error sut/bencode (failing-input-stream (runtime-exception "boom")) (unconnected-socket))]
      (is (socket/disconnected? e))
      (is (re-find #"lost its connection" (ex-message e)))))

  (testing "a RuntimeException is rethrown unchanged when there is no socket to blame"
    (let [cause (runtime-exception "boom")]
      (is (identical? cause (recv-error sut/bencode (failing-input-stream cause))))))

  (testing "another read failure is rethrown unchanged when the socket is still connected"
    (let [cause (IOException. "boom")]
      (let [server (socket/inet-socket "127.0.0.1" 0)
            client (socket/connect "127.0.0.1" (socket/local-port server))]
        (try
          (is (identical? cause (recv-error sut/bencode (failing-input-stream cause) client)))
          (finally
            (socket/close-connection! client)
            (socket/close-listener! server))))))

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
        (th/sleep 200)
        (is (= 1 @calls))
        (is (= {:n 1} (sut/recv transport 2000)))
        (th/sleep 200)
        (is (= 2 @calls))
        (finally
          (io/close! transport))))))

(deftest recv-timeout-keeps-the-message
  (testing "a recv that times out does not lose the message read in the meantime"
    (let [calls (atom 0)
          transport (fn-transport-of (fn []
                                       (if (= 1 (swap! calls inc))
                                         (do (th/sleep 300) {:n 1})
                                         (th/sleep 60000))))]
      (try
        (is (nil? (sut/recv transport 50)))
        (is (= {:n 1} (sut/recv transport 5000)))
        (finally
          (io/close! transport))))))

(deftest reader-failure-is-sticky
  (testing "a failing read is rethrown by every recv"
    (let [failure (ex-info "boom" {})
          transport (fn-transport-of #(throw failure))]
      (try
        (is (identical? failure (try (sut/recv transport 2000) (catch #?(:clj Throwable :cljr Exception) e e))))
        (is (identical? failure (try (sut/recv transport 2000) (catch #?(:clj Throwable :cljr Exception) e e))))
        (finally
          (io/close! transport)))))

  (testing "the reader loop does not read again after a failure"
    (let [counters (repeatedly 200 #(atom 0))
          transports (mapv (fn [calls]
                             (fn-transport-of #(do (swap! calls inc)
                                                   (throw (ex-info "boom" {})))))
                           counters)]
      (try
        (run! #(try (sut/recv % 2000) (catch #?(:clj Throwable :cljr Exception) _)) transports)
        (th/sleep 200)
        (is (= (repeat 200 1) (map deref counters)))
        (finally
          (run! io/close! transports))))))

(deftest close-stops-the-reader-loop
  (testing "closing drops a message the reader loop is blocked on handing over"
    (let [calls (atom 0)
          transport (fn-transport-of #(hash-map :n (swap! calls inc)))]
      (is (= {:n 1} (sut/recv transport 2000)))
      (th/sleep 200)
      (io/close! transport)
      (th/sleep 200)
      (is (nil? (sut/recv transport 300)))
      (is (= 2 @calls))))

  (testing "closing interrupts a reader loop that is blocked in the read"
    (let [interrupted (promise)
          calls (atom 0)
          transport (fn-transport-of (fn []
                                       (swap! calls inc)
                                       (try (th/sleep 60000)
                                            (catch #?(:clj InterruptedException :cljr System.Threading.ThreadInterruptedException) e
                                              (deliver interrupted true)
                                              (throw e)))))]
      (th/sleep 100)
      (io/close! transport)
      (is (true? (deref interrupted 2000 false)))
      (th/sleep 300)
      (is (= 1 @calls)))))

(deftest infinite-timeout-is-accepted
  (testing "recv accepts the infinite timeout, with and without an argument"
    (let [transport (fn-transport-of #(do (th/sleep 100) {:n 1}))]
      (try
        (is (= {:n 1} (sut/recv transport threading/infinite-ms)))
        (is (= {:n 1} (sut/recv transport)))
        (finally
          (io/close! transport))))))

(deftest tcp-server-smoke
  (testing "a TCP server evaluates, forwards output, and stops accepting when closed"
    (let [accept-failure (promise)
          server (server/start-server :consume-exception #(deliver accept-failure %))
          port (:port server)]
      (try
        (is (pos? port))
        (with-open [conn (nrepl/connect :port port)]
          (let [client (nrepl/client conn 5000)]
            (is (= [3] (nrepl/response-values
                        (nrepl/message client {:op "eval" :code "(+ 1 2)"}))))
            (let [combined (nrepl/combine-responses
                            (nrepl/message client {:op "eval" :code "(println :hi)"}))]
              (is (= (th/newline->sys ":hi\n") (:out combined)))
              (is (= ["nil"] (:value combined)))
              (is (contains? (:status combined) "done")))))
        (finally
          (server/stop-server server)))
      (is (socket/listener-closed? (:server-socket server)))
      (is (some? (deref accept-failure 5000 nil)))
      (is (th/thrown-with? th/connection-refused? (socket/connect "127.0.0.1" port))))))
