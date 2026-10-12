(ns nrepl.socket-test
  (:require [clojure.test :refer [deftest testing is]]
            [nrepl.socket :as sut])
  (:import (java.io IOException)
           (java.net ServerSocket Socket SocketException URI)
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

(deftest parse-uri-test
  (testing "parse-uri returns scheme, host and port of a string"
    (is (= {:scheme "nrepl" :host "example.org" :port 7888}
           (sut/parse-uri "nrepl://example.org:7888"))))

  (testing "parse-uri reports the port as -1 when the url has none"
    (is (= -1 (:port (sut/parse-uri "telnet://example.org")))))

  (testing "parse-uri keeps the scheme as written"
    (is (= "NREPL" (:scheme (sut/parse-uri "NREPL://localhost:1")))))

  (testing "parse-uri accepts a java.net.URI"
    (is (= {:scheme "nrepl+edn" :host "h" :port 5}
           (sut/parse-uri (URI. "nrepl+edn://h:5")))))

  (testing "parse-uri throws for a malformed url"
    (is (thrown? java.net.URISyntaxException (sut/parse-uri "nrepl://exa mple.org")))))

(deftest tcp-listener-test
  (let [ss (sut/inet-socket "127.0.0.1" 0)]
    (try
      (testing "local-port is the bound port"
        (is (pos? (sut/local-port ss)))
        (is (= (.getLocalPort ^ServerSocket ss) (sut/local-port ss))))

      (testing "as-nrepl-uri-string is the ASCII uri text of the listener"
        (is (= (str "nrepl://127.0.0.1:" (sut/local-port ss))
               (sut/as-nrepl-uri-string {:host "127.0.0.1" :server-socket ss} "nrepl"))))

      (testing "connect opens a client connection that close-connection! closes"
        (let [^Socket client (sut/connect "127.0.0.1" (sut/local-port ss))]
          (is (.isConnected client))
          (is (not (.isClosed client)))
          (sut/close-connection! client)
          (is (.isClosed client))))

      (testing "listener-closed? follows close-listener!"
        (is (not (sut/listener-closed? ss)))
        (sut/close-listener! ss)
        (is (sut/listener-closed? ss)))
      (finally
        (.close ^ServerSocket ss)))))
