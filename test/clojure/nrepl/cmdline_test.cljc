(ns nrepl.cmdline-test
  {:author "Chas Emerick"}
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   #?(:clj [clojure.java.io :refer [as-file]])
   #?(:clj [com.github.ivarref.locksmith :as locksmith])
   #?(:clj [matcher-combinators.matchers :as m]
      :cljr [nrepl.test-helpers.match :as m])
   [nrepl.ack :as ack]
   [nrepl.cmdline :as cmd]
   [nrepl.core :as nrepl]
   [nrepl.core-test :refer [*server* *transport-fn* transport-fns]]
   [nrepl.middleware :as middleware]
   [nrepl.server :as server]
   [nrepl.socket :refer [unix-domain-flavor]]
   [nrepl.test-helpers :as th :refer [eval-value1 free-port is+ win? with-process with-timeout]]
   [nrepl.transport :as transport])
  #?@(:clj [(:import
             (clojure.lang LineNumberingPushbackReader)
             (java.nio.file Files)
             (java.util.concurrent TimeUnit)
             (nrepl.server Server))]
      :cljr [(:import
              (System.Diagnostics DataReceivedEventArgs DataReceivedEventHandler Process ProcessStartInfo)
              (System.IO Path))]))

(defn wrap-dummy [handler]
  (fn [msg] (handler msg)))

(middleware/set-descriptor! #'wrap-dummy
                            {:requires #{}
                             :expects #{}})

(defn- run-f-with-server-and-every-transport [server-opts-fn f]
  (doseq [transport-fn transport-fns]
    (with-open [^nrepl.server.Server server
                (apply server/start-server
                       (apply concat (assoc (if server-opts-fn (server-opts-fn) {})
                                            :transport-fn transport-fn
                                            :handler (ack/handle-ack (server/default-handler)))))]
      (binding [*transport-fn* transport-fn
                *server* server
                *print-level* *print-level*
                *print-length* *print-length*]
        (ack/reset-ack-port!)
        (f)))))

(defmacro with-server-every-transport
  {:style/indent 1}
  [server-opts-fn & body]
  `(run-f-with-server-and-every-transport ~server-opts-fn (fn [] ~@body)))

(defn- var->str
  [sym]
  (subs (str sym) 2))

#?(:cljr
   (defn- clr-process-info
     ^ProcessStartInfo [args dir]
     (let [info (ProcessStartInfo. (if win? "Clojure.Main.exe" "Clojure.Main"))
           env (.Environment info)]
       (doseq [a (concat ["-m" "nrepl.cmdline"] args)]
         (.Add (.ArgumentList info) (str a)))
       (set! (.UseShellExecute info) false)
       (set! (.RedirectStandardOutput info) true)
       (set! (.RedirectStandardError info) true)
       (when dir
         (set! (.WorkingDirectory info) ^String dir))
       (.set_Item env "CLOJURE_LOAD_PATH" (Path/Combine Environment/CurrentDirectory "src" "clojure"))
       (.set_Item env "NREPLACKTEST" "y")
       info)))

#?(:cljr
   (defn- start-clr-process
     [args dir]
     (let [out (atom [])
           err (atom [])
           p (Process.)]
       (set! (.StartInfo p) (clr-process-info args dir))
       (.add_OutputDataReceived p (gen-delegate DataReceivedEventHandler [sender ^DataReceivedEventArgs e]
                                                (when-some [line (.Data e)]
                                                  (swap! out conj line))))
       (.add_ErrorDataReceived p (gen-delegate DataReceivedEventHandler [sender ^DataReceivedEventArgs e]
                                               (when-some [line (.Data e)]
                                                 (swap! err conj line))))
       (.Start p)
       (.BeginOutputReadLine p)
       (.BeginErrorReadLine p)
       {:process p :out out :err err})))

#?(:cljr
   (defn- stop-clr-process!
     [{:keys [^Process process]}]
     (try
       (when-not (.HasExited process)
         (.Kill process true))
       (catch Exception _))
     (.Dispose process)))

(defmacro ^:private with-nrepl-process
  {:style/indent 1}
  [[binding args] & body]
  #?(:clj `(with-process [~binding (into ["java" "-Dnreplacktest=y"
                                          "-cp" (System/getProperty "java.class.path")
                                          "nrepl.main"]
                                         ~args)]
             ~@body)
     :cljr `(let [~binding (start-clr-process ~args nil)]
              (try ~@body
                   (finally (stop-clr-process! ~binding))))))

(defn- await-exit-code
  [proc ms]
  #?(:clj (when (.waitFor ^Process proc (long ms) TimeUnit/MILLISECONDS)
            (.exitValue ^Process proc))
     :cljr (let [^Process p (:process proc)]
             (when (.WaitForExit p (int ms))
               (.WaitForExit p)
               (.ExitCode p)))))

(def ^:private exit-42-code
  #?(:clj "(System/exit 42)"
     :cljr "(System.Environment/Exit 42)"))

(def ^:private ack-test-form
  #?(:clj '(System/getProperty "nreplacktest")
     :cljr '(System.Environment/GetEnvironmentVariable "NREPLACKTEST")))

(deftest repl-intro
  (is+ #"nREPL" (cmd/repl-intro)))

(deftest help
  (is+ #"Usage:" (cmd/help)))

(deftest parse-cli-values
  (is+ {:other "string"
        :middleware :middleware
        :handler :handler
        :transport :transport}
       (cmd/parse-cli-values {:other "string"
                              :middleware ":middleware"
                              :handler ":handler"
                              :transport ":transport"})))

(deftest args->cli-options
  (is+ [{:middleware :middleware :repl "true"} ["extra" "args"]]
       (cmd/args->cli-options ["-m" ":middleware" "-r" "true" "extra" "args"]))

  (testing "having ^:concat metadata groups together collection values coming from CLI and config"
    (with-redefs [nrepl.config/config {:middleware (with-meta '[foo/middleware bar/middleware] {:concat true})}]
      (is+ [{:middleware '[foo/middleware bar/middleware baz/middleware]} []]
           (cmd/args->cli-options ["-m" "[baz/middleware]"])))
    (with-redefs [nrepl.config/config {:middleware (with-meta '[foo/middleware] {:concat true})}]
      (is+ [{:middleware '[foo/middleware]} []]
           (cmd/args->cli-options [])))
    (with-redefs [nrepl.config/config {:middleware '[foo/middleware bar/middleware]}]
      (is+ [{:middleware '[foo/middleware bar/middleware baz/middleware]} []]
           (cmd/args->cli-options ["-m" "^:concat [baz/middleware]"])))
    (is+ [{:middleware '[baz/middleware]} []]
         (cmd/args->cli-options ["-m" "^:concat [baz/middleware]"]))
    (with-redefs [nrepl.config/config {:middleware (with-meta '[foo/middleware bar/middleware] {:concat true})}]
      (is+ [{:middleware '[foo/middleware bar/middleware baz/middleware]} []]
           (cmd/args->cli-options ["-m" "^:concat [baz/middleware]"]))))

  (testing "no ^:concat metadata - CLI overrides config"
    (with-redefs [nrepl.config/config {:middleware '[old/middleware]}]
      (is+ [{:middleware '[new/middleware]} []]
           (cmd/args->cli-options ["-m" "[new/middleware]"])))))

(deftest connection-opts
  (is+ {:port 5000
        :host "0.0.0.0"
        :socket nil
        :transport #'transport/bencode
        :repl-fn #'nrepl.cmdline/run-repl
        :tls-keys-str nil
        :tls-keys-file nil}
       (cmd/connection-opts {:port "5000"
                             :host "0.0.0.0"
                             :transport nil})))

(deftest server-opts
  (is+ {:bind "0.0.0.0"
        :port 5000
        :transport #'transport/bencode
        :handler #'clojure.core/identity
        :repl-fn #'clojure.core/identity
        :greeting nil
        :ack-port 2000}
       (cmd/server-opts {:bind "0.0.0.0"
                         :port 5000
                         :ack 2000
                         :handler 'clojure.core/identity
                         :repl-fn 'clojure.core/identity}))
  (testing  "middleware resolution"
    (testing "builds handler with optional middleware present"
      (let [result (cmd/server-opts {:middleware [(with-meta 'nrepl.cmdline-test/wrap-dummy {:optional true})]})]
        (is+ {:stack (m/embeds [#'nrepl.cmdline-test/wrap-dummy])} (meta (:handler result)))))

    (testing "builds handler with optional middleware missing"
      (let [base-middleware-stack (meta (:handler (cmd/server-opts {})))
            result (cmd/server-opts {:middleware [(with-meta 'nonexistent/middleware {:optional true})]})]
        (is+ base-middleware-stack (meta (:handler result)))))))

(deftest server-started-message
  (testing "default bind uses 127.0.0.1 in message and URI"
    (with-open [server (server/start-server
                        :transport-fn #'transport/bencode
                        :handler server/default-handler)]
      (is+ #"nREPL server started on port \d+ on host 127\.0\.0\.1 - .*//127\.0\.0\.1:\d+"
           (cmd/server-started-message server {:transport #'transport/bencode}))))
  (testing "explicit localhost bind is preserved"
    (with-open [server (server/start-server
                        :bind "localhost"
                        :transport-fn #'transport/bencode
                        :handler server/default-handler)]
      (is+ #"nREPL server started on port \d+ on host localhost - .*//localhost:\d+"
           (cmd/server-started-message server {:transport #'transport/bencode})))))

(defn- wait-for-ack
  [ms]
  (try
    (ack/wait-for-ack ms)
    (finally
      (deliver @@#'ack/ack-port-promise nil))))

(deftest ^:slow ack
  (with-server-every-transport nil
    (with-nrepl-process [_ ["--ack" (str (:port *server*))
                            "--transport" (var->str *transport-fn*)]]
      (let [acked-port (wait-for-ack 100000)]
        (when (is acked-port "Timed out waiting for ack")
          (with-open [transport-2 (nrepl/connect :port acked-port
                                                 :transport-fn *transport-fn*)]
            (let [client (nrepl/client transport-2 1000)]
              ;; just a sanity check
              (is (= "y" (eval-value1 client ack-test-form))))))))))

(deftest ^:slow explicit-port-argument
  (with-server-every-transport nil
    (let [ack-port (:port *server*)
          bind-port (free-port)]
      (with-nrepl-process [_ ["--port" (str bind-port)
                              "--ack" (str ack-port)
                              "--transport" (var->str *transport-fn*)]]
        (is (= bind-port (wait-for-ack 100000)))))))

(deftest connect-url-scheme-test
  (are [in out] (= out (#'cmd/connect-url-scheme in))
    "nrepl://localhost:7888" "nrepl"
    "NREPL://localhost"      "nrepl"
    "nrepl+edn://host:1"     "nrepl+edn"
    "nrepls://host:7888"     "nrepls"
    "http://host/repl"       "http"
    "https://host/repl"      "https"
    "localhost"              nil
    "127.0.0.1:7888"         nil
    ""                       nil
    nil                      nil))

(deftest ensure-url-scheme-support-test
  (testing "schemes handled by nREPL itself need no extra setup"
    (is (nil? (#'cmd/ensure-url-scheme-support! "nrepl")))
    (is (nil? (#'cmd/ensure-url-scheme-support! "nrepl+edn"))))
  (testing "telnet is rejected, since the built-in client can't drive a tty transport"
    (let [died (fn [& msg] (throw (ex-info (apply str msg) {})))]
      (with-redefs [cmd/die died]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not support the tty transport"
                              (#'cmd/ensure-url-scheme-support! "telnet"))))))
  (testing "http(s) needs drawbridge, which isn't on the test classpath"
    (let [died (fn [& msg] (throw (ex-info (apply str msg) {})))]
      (with-redefs [cmd/die died]
        (doseq [scheme ["http" "https"]]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires the nrepl/drawbridge library"
                                (#'cmd/ensure-url-scheme-support! scheme))))))))

(deftest unix-socket-url-test
  (are [in out] (= out (#'cmd/unix-socket-url in))
    "nrepl+unix:/tmp/n.sock"     [#'transport/bencode "/tmp/n.sock"]
    "nrepl+edn+unix:/tmp/n.sock" [#'transport/edn "/tmp/n.sock"]
    "NREPL+UNIX:/tmp/n.sock"     [#'transport/bencode "/tmp/n.sock"]
    "nrepl://host:7888"          nil
    "nrepl+unix:"                nil
    "/tmp/n.sock"                nil
    nil                          nil))

(deftest url-conflicting-options-test
  ;; A URL already says where to connect and over which transport, so options
  ;; that decide the same things are refused rather than silently dropped.
  (let [died (fn [& msg] (throw (ex-info (apply str msg) {})))]
    (with-redefs [cmd/die died]
      (are [opts pattern] (thrown-with-msg?
                           clojure.lang.ExceptionInfo pattern
                           (#'cmd/reject-conflicting-url-options! "nrepl://h:1" opts))
        {:socket "/tmp/s"}            #"--socket can't be combined"
        {:port 12345}                 #"--port can't be combined"
        {:transport #'transport/edn}  #"--transport can't be combined")
      (testing "the defaults alone are not a conflict"
        (is (nil? (#'cmd/reject-conflicting-url-options!
                   "nrepl://h:1" {:transport #'transport/bencode})))))))

;;; Unix domain socket tests

(defn send-unix-socket-message [message path]
  (with-open [sock (nrepl.socket/unix-client-socket path)]
    ;; Assume it's safe to use Channels input/output streams here since
    ;; we're never reading and writing at the same time.
    ;; (cf. https://bugs.openjdk.java.net/browse/JDK-4509080).
    (let [out (nrepl.socket/buffered-output sock)]
      (#'transport/safe-write-bencode out message)
      #?(:clj (.flush ^java.io.Flushable out)
         :cljr (.Flush ^System.IO.Stream out)))))

(defn socket-file-connectable? [sock-path]
  (try (with-open [#?(:clj ^java.io.Closeable _ :cljr _) (nrepl.socket/unix-client-socket sock-path)]
         true)
       (catch Exception _ false)))

(defn- await-connectable
  [sock-path ms]
  (let [deadline (+ (th/now-ms) ms)]
    (loop []
      (or (socket-file-connectable? sock-path)
          (when (< (th/now-ms) deadline)
            (th/sleep 100)
            (recur))))))

(deftest ^:slow basic-fs-socket-behavior
  (when (is unix-domain-flavor)
    (let [tmpdir (th/make-temp-dir "socket-test-")
          sock-path (th/path-join tmpdir "socket")]
      (try
        (with-nrepl-process [server ["-s" sock-path]]
          (is (await-connectable sock-path 60000) "the socket never became connectable")
          (th/sleep 1000) ;; Extra wait to ensure the server is up.
          (send-unix-socket-message {:code exit-42-code :op :eval}
                                    sock-path)
          (is (= 42 (await-exit-code server 60000))))
        (finally
          (th/delete-recursively! tmpdir))))))

#?(:cljr
   (defn- await-line
     [{:keys [out]} re]
     (let [deadline (+ (th/now-ms) 60000)]
       (loop []
         (or (some #(when (re-find re %) %) @out)
             (when (< (th/now-ms) deadline)
               (th/sleep 50)
               (recur)))))))

#?(:cljr
   (def ^:private started-line-re
     #"^nREPL server started on port (\d+) on host 127\.0\.0\.1 - nrepl://127\.0\.0\.1:(\d+)$"))

#?(:cljr
   (defn- send-exit!
     [port code]
     (with-open [transport (nrepl/connect :port port)]
       (transport/send transport {:op "eval" :code (str "(System.Environment/Exit " code ")")}))))

#?(:cljr
   (deftest ^:slow clr-started-line-and-exit-status
     (let [proc (start-clr-process ["--port" "0"] nil)]
       (try
         (let [line (await-line proc started-line-re)
               [_ port-text uri-port-text] (re-find started-line-re (str line))]
           (is line "no started line on stdout")
           (is (= line (first @(:out proc))) "the started line is the first thing printed")
           (is (= port-text uri-port-text))
           (when line
             (let [port (Int32/Parse ^String port-text)]
               (with-open [transport (nrepl/connect :port port)]
                 (is (= 3 (eval-value1 (nrepl/client transport 5000) '(+ 1 2)))))
               (send-exit! port 42)
               (is (= 42 (await-exit-code proc 30000))))))
         (finally
           (stop-clr-process! proc))))))

#?(:cljr
   (deftest ^:slow clr-port-file
     (let [dir (th/make-temp-dir "port-file-test-")
           port-file (th/path-join dir ".nrepl-port")
           proc (start-clr-process ["--port" "0"] dir)]
       (try
         (let [line (await-line proc started-line-re)
               port-text (second (re-find started-line-re (str line)))]
           (is line "no started line on stdout")
           (when line
             (is (= port-text (slurp port-file)))
             (send-exit! (Int32/Parse ^String port-text) 0)
             (is (= 0 (await-exit-code proc 30000)))
             (is (not (th/path-exists? port-file)) "the port file survived the exit")))
         (finally
           (stop-clr-process! proc)
           (th/delete-recursively! dir))))))

#?(:cljr
   (defn- interrupt-process!
     [{:keys [^Process process]}]
     (let [info (ProcessStartInfo. "/bin/sh")]
       (.Add (.ArgumentList info) "-c")
       (.Add (.ArgumentList info) (str "kill -INT " (.Id process)))
       (set! (.UseShellExecute info) false)
       (with-open [killer (Process/Start info)]
         (.WaitForExit killer 10000)))))

#?(:cljr
   (deftest ^:slow clr-sigint-stops-the-server
     (if win?
       (is win? "SIGINT delivery is not exercised on Windows")
       (let [dir (th/make-temp-dir "sigint-test-")
             port-file (th/path-join dir ".nrepl-port")
             proc (start-clr-process ["--port" "0"] dir)]
         (try
           (let [line (await-line proc started-line-re)]
             (is line "no started line on stdout")
             (when line
               (interrupt-process! proc)
               (is (= 0 (await-exit-code proc 30000)))
               (is (not (th/path-exists? port-file)))))
           (finally
             (stop-clr-process! proc)
             (th/delete-recursively! dir)))))))

#?(:cljr
   (deftest ^:slow clr-unsupported-options-exit-with-status-2
     (doseq [[args option] [[["--connect"] "--connect"]
                            [["-i"] "--interactive"]
                            [["--color"] "--color"]]]
       (let [proc (start-clr-process args nil)]
         (try
           (is (= 2 (await-exit-code proc 60000)) (str args))
           (is (= [(str option " is not supported on ClojureCLR")] @(:err proc)) (str args))
           (is (empty? @(:out proc)) (str args))
           (finally
             (stop-clr-process! proc)))))))

#?(:cljr
   (deftest unsupported-options
     (doseq [[options option] [[{:connect true} "--connect"]
                               [{:interactive true} "--interactive"]
                               [{:color true} "--color"]]]
       (let [err (System.IO.StringWriter.)
             e (binding [*err* err]
                 (try (cmd/dispatch-commands options)
                      (catch clojure.lang.ExceptionInfo e e)))]
         (is (= {:nrepl/kind :nrepl.cmdline/exit :nrepl.cmdline/status 2} (ex-data e)))
         (is (= (str option " is not supported on ClojureCLR\n") (str err)))))))

#?(:clj
   (defn create-tmpdir
     "Creates a temporary directory in parent (something clojure.java.io/as-path
     can handle) and returns its Path."
     [parent prefix]
     (let [nio-path (.toPath (as-file parent))]
       (Files/createTempDirectory nio-path prefix (into-array java.nio.file.attribute.FileAttribute [])))))

#?(:clj
   (deftest can-define-dynamic-vars
     (with-server-every-transport nil
       (testing "when a dynamic var is defined from CLI client, it stays dynamic (#271)"
         (let [test-input      (str/join "\n" ["(def ^:dynamic *dyn* 1)"
                                               "(binding [*dyn* 2] *dyn*)"])
               expected-output ["#'user/*dyn*" "2"]
               >devnull        (fn [& _] nil)]
           (binding [*in* (java.io.PushbackReader. (java.io.StringReader. test-input))]
             (with-redefs [cmd/clean-up-and-exit >devnull]
               (let [results (atom [])]
                 (with-timeout 60000
                   (#'cmd/run-repl (:host *server*) (:port *server*)
                                   {:transport *transport-fn*
                                    :prompt >devnull
                                    :err >devnull
                                    :out >devnull
                                    :value #(swap! results conj %)}))
                 (is (= expected-output @results))))))))))

#?(:clj
   (deftest raw-input-handling
     ;; The client reads input only to find form boundaries and sends the raw
     ;; text to the server, so reader context that exists only in the server
     ;; session (aliases, data readers) doesn't matter on the client, and broken
     ;; input can't kill the REPL.
     (with-open [^Server server (server/start-server :handler (server/default-handler))]
       (let [drive (fn [input]
                     (let [>devnull (fn [& _] nil)
                           values (atom [])
                           errors (atom "")]
                       (binding [*in* (java.io.StringReader. input)]
                         (with-redefs [cmd/clean-up-and-exit >devnull]
                           (with-timeout 60000
                             (#'cmd/run-repl "127.0.0.1" (:port server)
                                             {:prompt >devnull
                                              :err #(swap! errors str %)
                                              :out >devnull
                                              :value #(swap! values conj %)}))))
                       {:values @values :errors @errors}))]
         (testing "a reader typo doesn't kill the session"
           (let [{:keys [values errors]} (drive ")\n(+ 1 2)")]
             (is (= ["3"] values))
             (is (str/includes? errors "Unmatched delimiter"))))
         (testing "tagged literals unknown to the client are read by the server"
           (let [{:keys [values errors]} (drive "#nosuch/tag 42\n(+ 1 2)")]
             (is (= ["3"] values))
             (is (str/includes? errors "No reader function for tag nosuch/tag"))))
         (testing "auto-resolved keywords use the server session's aliases"
           (let [{:keys [values]} (drive "(require '[clojure.string :as zzz])\n::zzz/foo")]
             (is (= ["nil" ":clojure.string/foo"] values))))
         (testing "multi-line forms work"
           (is (= ["3"] (:values (drive "(+ 1\n2)")))))
         (testing "reader conditionals are passed through to the server"
           (is (= ["1"] (:values (drive "#?(:clj 1 :cljs 2)")))))
         (testing "#_ discard is sent along with the following form"
           (is (= ["42"] (:values (drive "#_(/ 1 0) 42")))))
         (testing "metadata reaches the server without client-side re-printing"
           (is (= ["true"] (:values (drive "(:foo (meta (quote ^:foo bar)))")))))
         (testing "the exit command stops the loop before remaining input"
           (is (= ["3"] (:values (drive "(+ 1 2)\nexit\n(+ 7 8)")))))
         (testing "after a reader error the rest of the line is discarded (not re-read)"
           ;; `)foo` must not sneak `foo` through to the server as a real eval.
           (let [{:keys [values errors]} (drive ")foo\n(+ 1 2)")]
             (is (str/includes? errors "Unmatched delimiter"))
             (is (= ["3"] values))))
         (testing "EOF in the middle of a form ends the loop instead of hanging"
           (is (= ["3"] (:values (drive "(+ 1 2)\n(+ 4")))))))))

#?(:clj
   (deftest read-form-for-server-passthrough
     ;; The client reads only to find the form boundary; literals that would
     ;; throw under *read-eval* false (records, #=) or that the client can't
     ;; resolve (aliased keywords, unknown tags) are read inertly and their
     ;; exact text is handed to the server verbatim.
     (let [read-one (fn [s]
                      (let [rdr (LineNumberingPushbackReader.
                                 (java.io.StringReader. s))]
                        (#'cmd/read-form-for-server rdr)))]
       (are [input expected-code] (= expected-code (:code (read-one input)))
         "#clojure.lang.PersistentQueue[]" "#clojure.lang.PersistentQueue[]"
         "#nosuch/tag 42"                  "#nosuch/tag 42"
         "::zzz/foo"                       "::zzz/foo"
         "(+ 1\n  2)"                      "(+ 1\n  2)")
       (testing "tagged literals are held inert rather than constructed"
         (are [input] (instance? clojure.lang.TaggedLiteral (:form (read-one input)))
           "#clojure.lang.PersistentQueue[]"
           "#nosuch/tag 42"))
       (testing "#= is rejected instead of being evaluated in the client"
         ;; *suppress-read* doesn't cover #=, so without *read-eval* false the
         ;; reader runs it here and the server then runs the same text again.
         (let [ran (atom false)]
           (intern 'nrepl.cmdline-test 'mark-ran (fn [] (reset! ran true)))
           (is (thrown-with-msg? RuntimeException #"EvalReader not allowed"
                                 (read-one "#=(nrepl.cmdline-test/mark-ran)")))
           (is (false? @ran))))
       (testing "only the first form is consumed, leaving the rest for the next read"
         (let [rdr (LineNumberingPushbackReader.
                    (java.io.StringReader. "(+ 1 2) (+ 3 4)"))]
           (is (= "(+ 1 2)" (:code (#'cmd/read-form-for-server rdr))))
           (is (= "(+ 3 4)" (:code (#'cmd/read-form-for-server rdr))))
           (is (= :nrepl.cmdline/eof (#'cmd/read-form-for-server rdr))))))))

#?(:clj
   (deftest tls-url-option-validation
     (let [died (fn [& msg] (throw (ex-info (apply str msg) {})))]
       (with-redefs [cmd/die died]
         (are [url opts re] (thrown-with-msg? clojure.lang.ExceptionInfo re
                                              (#'cmd/run-repl url nil opts))
           ;; TLS URLs demand the TLS keys options
           "nrepls://localhost:12345"     {}                          #"require --tls-keys-file or --tls-keys-str"
           "nrepl+edns://localhost:12345" {}                          #"require --tls-keys-file or --tls-keys-str"
           ;; TLS keys options are rejected for non-TLS URL schemes
           "nrepl://localhost:12345"      {:tls-keys-str "irrelevant"} #"only supported for nrepls:// or nrepl\+edns://"
           ;; TLS URLs without a usable host are rejected
           "nrepls://"                    {:tls-keys-str "irrelevant"} #"Invalid URL"
           "nrepls:///nohost"             {:tls-keys-str "irrelevant"} #"Can't extract a host"
           ;; ... as are out-of-range ports
           "nrepls://localhost:444401"    {:tls-keys-str "irrelevant"} #"Invalid port")))))

#?(:clj
   (deftest can-connect-via-tls-url
     (if win?
       ;; TLS is not exercised on Windows (see nrepl.tls-test); assert something
       ;; so the test isn't reported as running without assertions.
       (is win? "TLS tests are skipped on Windows")
       (let [{:keys [ca-cert server-cert server-key client-cert client-key]} (locksmith/gen-certs {:duration-days 1})
             server-keys (str ca-cert server-cert server-key)
             client-keys (str ca-cert client-cert client-key)]
         (doseq [transport-fn [#'transport/bencode #'transport/edn]]
           (with-open [^Server server (server/start-server :tls? true
                                                           :tls-keys-str server-keys
                                                           :transport-fn transport-fn)]
             ;; Connect via the exact URL the server advertises in its startup
             ;; message (nrepls:// or nrepl+edns://).
             (let [url (str (nrepl.socket/as-nrepl-uri server (transport/uri-scheme transport-fn)))
                   >devnull (fn [& _] nil)
                   results (atom [])]
               (binding [*in* (java.io.PushbackReader. (java.io.StringReader. "(+ 1 2)"))]
                 (with-redefs [cmd/clean-up-and-exit >devnull]
                   (with-timeout 60000
                     (#'cmd/run-repl url nil
                                     {:tls-keys-str client-keys
                                      :prompt >devnull
                                      :err    >devnull
                                      :out    >devnull
                                      :value  #(swap! results conj %)}))
                   (is (= ["3"] @results)
                       (str "connecting via " url)))))))))))

#?(:clj
   (deftest can-connect-via-url
     (with-server-every-transport nil
       (testing "the built-in client can connect using a URL routed through url-connect"
         (let [url      (format "%s://%s:%d"
                                (transport/uri-scheme *transport-fn*)
                                (:host *server*)
                                (:port *server*))
               >devnull (fn [& _] nil)]
           (binding [*in* (java.io.PushbackReader. (java.io.StringReader. "(+ 1 2)"))]
             (with-redefs [cmd/clean-up-and-exit >devnull]
               (let [results (atom [])]
                 (with-timeout 60000
                   (#'cmd/run-repl url nil
                                   {:prompt >devnull
                                    :err    >devnull
                                    :out    >devnull
                                    :value  #(swap! results conj %)}))
                 (is (= ["3"] @results))))))))))

#?(:clj
   (deftest cmdline-namespace-resolution
     (testing "Ensuring that namespace resolution works in the cmdline repl"
       (let [test-input (str/join \newline ["::a"
                                            "(ns a)" "::a"
                                            "(ns b)" "::a"
                                            "(ns user)" "::a"
                                            "(require '[a :as z])" "::z/a"])
             expected-output [(str (keyword (str *ns*) "a"))
                              "nil" ":a/a"
                              "nil" ":b/a"
                              "nil" ":user/a"
                              "nil" ":a/a"]
             >devnull (fn [& _] nil)]
         (with-redefs [cmd/clean-up-and-exit >devnull]
           (with-server-every-transport nil
             (binding [*in* (java.io.PushbackReader. (java.io.StringReader. test-input))]
               (let [results (atom [])]
                 (with-timeout 60000
                   (#'cmd/run-repl (:host *server*) (:port *server*)
                                   {:transport *transport-fn*
                                    :prompt >devnull
                                    :err >devnull
                                    :out >devnull
                                    :value #(swap! results conj %)}))
                 (is (= expected-output @results))))))))))

#?(:clj
   (deftest ^:slow can-connect-to-unix-socket
     (when (is unix-domain-flavor)
       (testing "We can connect to unix domain socker from the cli."
         (let [test-input      (str/join \newline ["::a"
                                                   "(ns a)" "::a"
                                                   "(ns b)" "::a"
                                                   "(ns user)" "::a"
                                                   "(require '[a :as z])" "::z/a"])
               expected-output [(str (keyword (str *ns*) "a"))
                                "nil" ":a/a"
                                "nil" ":b/a"
                                "nil" ":user/a"
                                "nil" ":a/a"]
               >devnull        (fn [& _] nil)]
           (with-redefs [cmd/clean-up-and-exit >devnull]
             (with-server-every-transport (fn [] {:socket (str (create-tmpdir "target" "socket-test-") "/socket")})
               (binding [*in* (java.io.PushbackReader. (java.io.StringReader. test-input))]
                 (let [results (atom [])]
                   (with-timeout 60000
                     (#'cmd/run-repl {:server  *server*
                                      :options {:transport *transport-fn*
                                                :prompt >devnull
                                                :err >devnull
                                                :out >devnull
                                                :value #(swap! results conj %)}}))
                   (is (= expected-output @results)))))))))))
