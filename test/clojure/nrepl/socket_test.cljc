(ns nrepl.socket-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [nrepl.core :as nrepl]
            [nrepl.server :as server]
            [nrepl.socket :as sut]
            [nrepl.test-helpers :as th]
            [nrepl.test-helpers.bounded :as bounded]
            [nrepl.transport :as transport])
  #?(:clj (:import (java.io IOException)
                   (java.net Socket URI URISyntaxException)
                   (java.nio.channels ClosedChannelException))
     :cljr (:import (System.IO IOException))))

(def ^:private closed-exception
  #?(:clj (ClosedChannelException.)
     :cljr (ObjectDisposedException. "closed")))

(def ^:private port-token "PORT")

(defn- tcp-uri-text
  [host scheme]
  (let [ss (sut/inet-socket "127.0.0.1" 0)]
    (try
      (let [text (sut/as-nrepl-uri-string {:host host :server-socket ss} scheme)]
        (str (subs text 0 (- (count text) (count (str (sut/local-port ss))))) port-token))
      (finally
        (sut/close-listener! ss)))))

(defn- unix-uri-text
  [name]
  (let [dir (th/make-temp-dir "nrepl-uri-")
        path (th/path-join dir name)
        ss (sut/unix-server-socket path)]
    (try
      [path (sut/as-nrepl-uri-string {:host nil :server-socket ss} "nrepl")]
      (finally
        (sut/close-listener! ss)
        (th/delete-recursively! dir)))))

(defn- quoted-dir
  [path name]
  (let [dir (subs path 0 (- (count path) (count name)))]
    (str/replace dir "\\" "%5C")))

(deftest disconnected-exception-test
  (testing "disconnected-exception builds an exception that disconnected? recognises"
    (let [e (sut/disconnected-exception "gone")]
      (is (sut/disconnected? e))
      (is (= "gone" (ex-message e)))))

  (testing "disconnected? is false for other exceptions"
    (is (not (sut/disconnected? (IOException. "gone"))))
    (is (not (sut/disconnected? closed-exception)))
    (is (not (sut/disconnected? nil)))))

(deftest closed-exception-test
  (testing "closed-exception? recognises the exception accept throws on a closed listener"
    (is (sut/closed-exception? closed-exception)))

  (testing "closed-exception? is false for other exceptions"
    (is (not (sut/closed-exception? (IOException. "gone"))))
    (is (not (sut/closed-exception? (sut/disconnected-exception "gone"))))
    (is (not (sut/closed-exception? nil)))))

(deftest parse-uri-test
  (testing "parse-uri returns scheme, host and port of a string"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "nrepl://example.org:7888"   {:scheme "nrepl" :host "example.org" :port 7888}
      "nrepl+edn://h:5"            {:scheme "nrepl+edn" :host "h" :port 5}
      "nrepl://127.0.0.1:0"        {:scheme "nrepl" :host "127.0.0.1" :port 0}
      "nrepl://127.0.0.1:65536"    {:scheme "nrepl" :host "127.0.0.1" :port 65536}
      "nrepl://u@h:5/p"            {:scheme "nrepl" :host "h" :port 5}
      "nrepl://h:5/p?q#f"          {:scheme "nrepl" :host "h" :port 5}))

  (testing "parse-uri reports the port as -1 when the url has none, even for a scheme with a default port"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "nrepl://example.org"        {:scheme "nrepl" :host "example.org" :port -1}
      "telnet://example.org"       {:scheme "telnet" :host "example.org" :port -1}
      "nrepl://h:"                 {:scheme "nrepl" :host "h" :port -1}))

  (testing "parse-uri reports an explicit default port"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "telnet://example.org:23"    {:scheme "telnet" :host "example.org" :port 23}
      "http://h:80"                {:scheme "http" :host "h" :port 80}))

  (testing "parse-uri keeps the scheme and the host as written"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "NREPL://LocalHost:1"        {:scheme "NREPL" :host "LocalHost" :port 1}
      "nrepl://H:7"                {:scheme "nrepl" :host "H" :port 7}))

  (testing "parse-uri keeps the brackets of an IPv6 host"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "nrepl://[::1]:7888"         {:scheme "nrepl" :host "[::1]" :port 7888}
      "nrepl://[::1]"              {:scheme "nrepl" :host "[::1]" :port -1}))

  (testing "parse-uri has no host and no port for a unix socket url"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "nrepl+unix:/tmp/n.sock"     {:scheme "nrepl+unix" :host nil :port -1}
      "nrepl+unix:/tmp/a%20b"      {:scheme "nrepl+unix" :host nil :port -1}))

  (testing "parse-uri has no host for an authority that is not a host and port"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "nrepl://my_host:7888"       {:scheme "nrepl" :host nil :port -1}
      "nrepl://h:abc"              {:scheme "nrepl" :host nil :port -1}
      "nrepl:///x"                 {:scheme "nrepl" :host nil :port -1}
      "nrepl:h:5"                  {:scheme "nrepl" :host nil :port -1}))

  (testing "parse-uri accepts a string without a scheme"
    (are [url parsed] (= parsed (sut/parse-uri url))
      "foo"                        {:scheme nil :host nil :port -1}
      ""                           {:scheme nil :host nil :port -1}))

  (testing "parse-uri accepts the host's own uri object"
    (is (= {:scheme "nrepl+edn" :host "h" :port 5}
           (sut/parse-uri #?(:clj (URI. "nrepl+edn://h:5")
                             :cljr (System.Uri. "nrepl+edn://h:5"))))))

  (testing "parse-uri throws for a malformed url"
    (is (thrown? #?(:clj URISyntaxException :cljr System.UriFormatException)
                 (sut/parse-uri "nrepl://exa mple.org")))
    (is (thrown? #?(:clj URISyntaxException :cljr System.UriFormatException)
                 (sut/parse-uri "nrepl+unix:")))))

(deftest as-nrepl-uri-string-test
  (testing "a TCP listener is nrepl://host:port, an IPv6 host in brackets"
    (are [host text] (= text (tcp-uri-text host "nrepl"))
      "127.0.0.1"    "nrepl://127.0.0.1:PORT"
      "0.0.0.0"      "nrepl://0.0.0.0:PORT"
      "example.org"  "nrepl://example.org:PORT"
      "localhost"    "nrepl://localhost:PORT"
      "LocalHost"    "nrepl://LocalHost:PORT"
      "::1"          "nrepl://[::1]:PORT"
      "[::1]"        "nrepl://[::1]:PORT"
      "fe80::1"      "nrepl://[fe80::1]:PORT"
      "fe80::1%eth0" "nrepl://[fe80::1%eth0]:PORT"))

  (testing "the transport's scheme is used as given"
    (is (= "nrepl+edn://127.0.0.1:PORT" (tcp-uri-text "127.0.0.1" "nrepl+edn")))
    (is (= "nrepl+edn://[::1]:PORT" (tcp-uri-text "::1" "nrepl+edn"))))

  (testing "a host that cannot be part of a URI is an error"
    (doseq [host ["my_host" "" "a b" nil]]
      (is (th/thrown-with? (constantly true) (tcp-uri-text host "nrepl"))
          (pr-str host))))

  (testing "a unix socket is nrepl+unix: followed by the absolute path"
    (let [[path text] (unix-uri-text "sock")]
      (is (= (str "nrepl+unix:" (str/replace path "\\" "%5C")) text))))

  (testing "a unix socket path is percent-encoded like java.net.URI does"
    (doseq [[name quoted] [["a b" "a%20b"]
                           ["a%20b" "a%2520b"]
                           ["50%" "50%25"]
                           ["a#b" "a%23b"]
                           ["a{b}" "a%7Bb%7D"]
                           ["a^b" "a%5Eb"]
                           ["a`b" "a%60b"]
                           ["a;b" "a;b"]
                           ["a@b" "a@b"]
                           ["a&b=c+d$e,f" "a&b=c+d$e,f"]
                           ["a!~'()" "a!~'()"]]]
      (let [[path text] (unix-uri-text name)]
        (is (= (str "nrepl+unix:" (quoted-dir path name) quoted) text) (pr-str name))))))

(deftest tcp-listener-test
  (let [ss (sut/inet-socket "127.0.0.1" 0)]
    (try
      (testing "local-port is the bound port"
        (is (pos? (sut/local-port ss))))

      (testing "as-nrepl-uri-string is the ASCII uri text of the listener"
        (is (= (str "nrepl://127.0.0.1:" (sut/local-port ss))
               (sut/as-nrepl-uri-string {:host "127.0.0.1" :server-socket ss} "nrepl"))))

      (testing "connect opens a client connection that close-connection! closes"
        (let [client (sut/connect "127.0.0.1" (sut/local-port ss))]
          (is (sut/is-connected? client))
          (sut/close-connection! client)
          #?(:clj (is (.isClosed ^Socket client))
             :cljr (is (not (sut/is-connected? client))))))

      (testing "listener-closed? follows close-listener!"
        (is (not (sut/listener-closed? ss)))
        (sut/close-listener! ss)
        (is (sut/listener-closed? ss)))
      (finally
        (sut/close-listener! ss)))))

(deftest accept-test
  (testing "accept returns the server side of a client connection"
    (let [ss (sut/inet-socket "127.0.0.1" 0)
          client (sut/connect "127.0.0.1" (sut/local-port ss))
          accepted (sut/accept ss)]
      (try
        (is (sut/is-connected? accepted))
        (finally
          (sut/close-connection! client)
          (sut/close-connection! accepted)
          (sut/close-listener! ss)))))

  (testing "accept on a closed listener throws something closed-exception? recognises"
    (let [ss (sut/inet-socket "127.0.0.1" 0)]
      (sut/close-listener! ss)
      (let [e (try (sut/accept ss) nil (catch #?(:clj Throwable :cljr Exception) e e))]
        (is (some? e))
        (is (sut/closed-exception? e)))))

  (testing "closing the listener under a blocked accept ends the accept"
    (let [ss (sut/inet-socket "127.0.0.1" 0)
          blocked (bounded/spawn #(try (sut/accept ss) nil
                                       (catch #?(:clj Throwable :cljr Exception) e e)))]
      (bounded/sleep-ms 200)
      (sut/close-listener! ss)
      (let [e (bounded/await-result blocked)]
        (is (not= ::bounded/timeout e))
        (is (some? e))
        (is (sut/listener-closed? ss))))))

(deftest unix-socket-listener-test
  (when sut/unix-domain-flavor
    (let [dir (th/make-temp-dir "nrepl-sock-")
          path (th/path-join dir "listener")]
      (try
        (let [ss (sut/unix-server-socket path)]
          (try
            (testing "a client connects to the path and the listener accepts it"
              (let [client (sut/unix-client-socket path)
                    accepted (sut/accept ss)]
                (try
                  (is (sut/is-connected? client))
                  (is (sut/is-connected? accepted))
                  (finally
                    (sut/close-connection! client)
                    (sut/close-connection! accepted)))))

            (testing "the path of a listener in use cannot be bound again"
              (is (th/thrown-with? (constantly true) (sut/unix-server-socket path))))

            (testing "listener-closed? follows close-listener!"
              (is (not (sut/listener-closed? ss)))
              (sut/close-listener! ss)
              (is (sut/listener-closed? ss)))

            (testing "accept on a closed listener throws something closed-exception? recognises"
              (let [e (try (sut/accept ss) nil (catch #?(:clj Throwable :cljr Exception) e e))]
                (is (some? e))
                (is (sut/closed-exception? e))))
            (finally
              (sut/close-listener! ss))))
        (finally
          (th/delete-recursively! dir))))))

(deftest unix-socket-stale-file-test
  (when sut/unix-domain-flavor
    (let [dir (th/make-temp-dir "nrepl-sock-")
          path (th/path-join dir "stale")]
      (try
        (spit path "left behind")
        (testing "a file already at the path blocks the bind and is left alone"
          (is (th/thrown-with? (constantly true) (sut/unix-server-socket path)))
          (is (= "left behind" (slurp path))))
        (finally
          (th/delete-recursively! dir))))))

(deftest unix-socket-connect-failure-test
  (when sut/unix-domain-flavor
    (let [dir (th/make-temp-dir "nrepl-sock-")]
      (try
        (testing "connecting to a path nobody listens on throws"
          (is (th/thrown-with? (constantly true)
                               (sut/unix-client-socket (th/path-join dir "nobody")))))
        (finally
          (th/delete-recursively! dir))))))

(defn- eval-over
  [connect-opts transport-fn]
  (with-open [conn (apply nrepl/connect :transport-fn transport-fn connect-opts)]
    (th/eval-value1 (nrepl/client conn 10000) '(+ 1 1))))

(deftest unix-socket-server-test
  (when sut/unix-domain-flavor
    (doseq [transport-fn [#'transport/bencode #'transport/edn]]
      (testing (str "a server on a unix socket serves " (transport/uri-scheme transport-fn))
        (let [dir (th/make-temp-dir "nrepl-sock-")
              path (th/path-join dir "server")]
          (try
            (let [server (server/start-server :socket path :transport-fn transport-fn)]
              (try
                (is (= path (:socket server)))
                (is (nil? (:port server)))
                (is (= (str (transport/uri-scheme transport-fn) "+unix:" (str/replace path "\\" "%5C"))
                       (sut/as-nrepl-uri-string server (transport/uri-scheme transport-fn))))
                (is (= 2 (eval-over [:socket path] transport-fn)))
                (is (= 2 (eval-over [:socket path] transport-fn)))
                (finally
                  (server/stop-server server))))
            #?(:cljr (testing "closing the server removes the socket file"
                       (is (not (th/path-exists? path)))))
            (testing "a closed server refuses connections"
              (is (th/thrown-with? (constantly true) (eval-over [:socket path] transport-fn))))
            (finally
              (th/delete-recursively! dir))))))))

(deftest stop-server-serves-no-connection-accepted-during-close-test
  (doseq [kind (cond-> [:tcp] sut/unix-domain-flavor (conj :unix))]
    (testing (str "a connection accepted while the listener closes on a " (name kind) " server is closed, not served")
      (let [dir (th/make-temp-dir "nrepl-sock-")
            path (th/path-join dir "server")
            real-accept sut/accept
            real-close-listener! sut/close-listener!
            first-accept? (atom :pending)
            events (atom [])
            record! (fn [event] (swap! events conj event))
            accepted (promise)
            listener-closed (promise)
            stop-returned (promise)
            transports-made (atom 0)
            transport-fn (fn [sock]
                           (swap! transports-made inc)
                           (transport/bencode sock))]
        (try
          (with-redefs [sut/accept (fn [listener]
                                     (if (compare-and-set! first-accept? :pending :taken)
                                       (let [conn (real-accept listener)]
                                         (record! :accepted)
                                         (deliver accepted true)
                                         @listener-closed
                                         (deref stop-returned 500 nil)
                                         conn)
                                       (try
                                         (real-accept listener)
                                         (finally (record! :final-accept)))))
                        sut/close-listener! (fn [listener]
                                              (real-close-listener! listener)
                                              (record! :listener-closed)
                                              (deliver listener-closed true))]
            (let [server (if (= :unix kind)
                           (server/start-server :socket path :transport-fn transport-fn)
                           (server/start-server :transport-fn transport-fn))
                  client (if (= :unix kind)
                           (nrepl/connect :socket path)
                           (nrepl/connect :port (:port server)))]
              (try
                (transport/send client {:op "eval" :code "(+ 1 1)" :id "during-close"})
                (deref accepted 5000 nil)
                (is (not= ::bounded/timeout
                          (bounded/bounded #(do (server/stop-server server)
                                                (record! :stop-returned)
                                                (deliver stop-returned true)))))
                (is (= [:accepted :listener-closed :final-accept :stop-returned] @events))
                (is (zero? @transports-made))
                (is (th/thrown-with? th/disconnected? (transport/recv client 5000)))
                (finally
                  (th/close! client)
                  (bounded/bounded #(server/stop-server server))))))
          (finally
            (th/delete-recursively! dir)))))))

(deftest tcp-server-url-test
  (doseq [transport-fn [#'transport/bencode #'transport/edn]]
    (let [server (server/start-server :transport-fn transport-fn)]
      (try
        (let [url (sut/as-nrepl-uri-string server (transport/uri-scheme transport-fn))]
          (is (= (str (transport/uri-scheme transport-fn) "://127.0.0.1:" (:port server)) url))
          (with-open [conn (nrepl/url-connect url)]
            (is (= 2 (th/eval-value1 (nrepl/client conn 10000) '(+ 1 1))))))
        (finally
          (server/stop-server server))))))
