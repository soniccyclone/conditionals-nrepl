(ns nrepl.cmdline
  "A proof-of-concept command-line client for nREPL.  Please see
  e.g. REPL-y for a proper command-line nREPL client @
  https://github.com/trptcolin/reply/"
  {:author "Chas Emerick"}
  (:require
   [clojure.string :as str]
   [nrepl.cmdline.options :as options]
   [nrepl.core :as nrepl]
   [nrepl.misc :refer [uuid]]
   [nrepl.server :as nrepl-server]
   [nrepl.transport :as transport]
   [nrepl.util.runtime :as runtime]
   [nrepl.util.threading :as threading]
   [nrepl.version :as version])
  (:import
   [clojure.lang LineNumberingPushbackReader]
   [java.io FileNotFoundException IOException]))

(defn- clean-up-and-exit
  "Performs any necessary clean up and calls `(System/exit status)`."
  [status]
  (shutdown-agents)
  (flush)
  (binding [*out* *err*] (flush))
  (runtime/exit! status))

(def exit options/exit)

(def die options/die)

(def help options/help)

(def parse-cli-values options/parse-cli-values)

(def args->cli-options options/args->cli-options)

(def server-started-message options/server-started-message)

(def save-port-file options/save-port-file)

(def ^:private connect-url-scheme options/connect-url-scheme)

(def ^:private unix-socket-url options/unix-socket-url)

(defmacro ^{:author "Colin Jones"} set-signal-handler!
  [signal f]
  (if (try (Class/forName "sun.misc.Signal")
           (catch Throwable _e))
    `(try
       (sun.misc.Signal/handle
        (sun.misc.Signal. ~signal)
        (proxy [sun.misc.SignalHandler] []
          (handle [signal#] (~f signal#))))
       (catch Throwable e#))
    `(println "Unable to set signal handlers.")))

(def colored-output
  {:err #(binding [*out* *err*]
           (print "\033[31m")
           (print %)
           (print "\033[m")
           (flush))
   :out print
   :value (fn [x]
            (print "\033[34m")
            (print x)
            (println "\033[m")
            (flush))})

(def running-repl (atom {:transport nil
                         :client nil}))

(defn- done?
  [input]
  (some (partial = input)
        ['exit 'quit '(exit) '(quit)]))

(defn repl-intro
  "Returns nREPL interactive repl intro copy and version info as a new-line
  separated string."
  []
  (format "nREPL %s
Clojure %s
%s %s
Interrupt: Control+C
Exit:      Control+D or (exit) or (quit)"
          (:version-string version/version)
          (clojure-version)
          (System/getProperty "java.vm.name")
          (System/getProperty "java.runtime.version")))

(def ^:private permissive-reader-resolver
  "A reader resolver that accepts any alias or class name. Used when reading
  user input purely to find form boundaries (see `read-form-for-server`):
  aliases and classes may exist only in the server's session, so the client
  must not reject them (this is what lets `::alias/kw` reach the server)."
  (reify clojure.lang.LispReader$Resolver
    (currentNS [_] 'user)
    (resolveClass [_ sym] sym)
    (resolveAlias [_ sym] sym)
    (resolveVar [_ sym] sym)))

(def ^:private eof-marker (Object.))

(defn- read-form-for-server
  "Reads a single form from `rdr` and returns a map of `:form` and `:code` -
  the exact input text that was consumed - or `::eof` when the input is
  exhausted.

  The client never evaluates the form; it is read only to find its boundary
  in the input stream (and to detect the exit/quit commands), while `:code`
  is sent to the server verbatim. Reading is therefore deliberately
  permissive: `*suppress-read*` keeps record and tagged literals inert, the
  resolver accepts unknown aliases/classes, and reader conditionals are
  preserved - all of them are left for the server to actually interpret.

  `*read-eval*` is off because `*suppress-read*` does not cover `#=`: the
  reader would otherwise evaluate it here, in the client, and the server
  would then evaluate the same text again. A `#=` form is rejected outright
  instead."
  [^LineNumberingPushbackReader rdr]
  (binding [*suppress-read* true
            *read-eval* false
            *reader-resolver* permissive-reader-resolver]
    (let [[form code] (try
                        (read+string {:eof eof-marker :read-cond :preserve} rdr)
                        (catch IOException _
                          ;; The input stream is gone (e.g. a closed pipe);
                          ;; there is nothing left to read, so treat it as
                          ;; end of input.
                          [eof-marker nil]))]
      (if (identical? form eof-marker)
        ::eof
        {:form form :code code}))))

(defn- skip-to-end-of-line
  "Discards the rest of the current line. Used after a reader error, the way
  `clojure.main`'s REPL does."
  [^LineNumberingPushbackReader rdr]
  (try
    (.readLine rdr)
    (catch IOException _ nil)))

(defn- run-repl-with-transport
  [transport {:keys [prompt err out value]
              :or   {prompt #(print (str % "=> "))
                     err    print
                     out    print
                     value  println}}]
  (let [client (nrepl/client transport Long/MAX_VALUE)
        awaiting (atom {})]
    (println (repl-intro))
    (threading/run-with @threading/transport-executor
      ;; Doesn't matter which executor we hijack here, just don't use `future`.
      (doseq [res (client)]
        (some-> (:out res) out)
        (some-> (:err res) err)
        (when-let [p (and (some #{:done "done"} (:status res))
                          (get @awaiting (:id res)))]
          (deliver p true))
        (when (some-> ^String (:id res)
                      (.startsWith "nrepl.cmdline-"))
          (some-> (:value res) value))
        (flush)))
    ;; We take 50ms to listen to any greeting messages, and display the value
    ;; in the `:out` slot.
    (Thread/sleep 50)
    (let [session (nrepl/client-session client)
          input-reader (LineNumberingPushbackReader. *in*)]
      (swap! running-repl assoc :transport transport :client session)
      (binding [*ns* *ns*]
        (loop []
          (prompt *ns*)
          (flush)
          (let [input (try
                        (read-form-for-server input-reader)
                        (catch Exception e
                          ;; A broken form should never take the REPL down.
                          ;; Report it and discard the rest of the line, like
                          ;; clojure.main does; the reader is then guaranteed
                          ;; to make progress on the next iteration.
                          (err (str "Syntax error: "
                                    (or (ex-message e)
                                        (some-> (ex-cause e) ex-message)
                                        (str e))
                                    "\n"))
                          (flush)
                          (skip-to-end-of-line input-reader)
                          ::retry))]
            (cond
              (identical? ::retry input)
              (recur)

              (or (identical? ::eof input) (done? (:form input)))
              (clean-up-and-exit 0)

              :else
              (let [id (str "nrepl.cmdline-" (uuid))]
                (swap! awaiting assoc id (promise))
                (doseq [res (nrepl/message session {:op "eval" :code (:code input) :id id})]
                  (when (:ns res) (set! *ns* (create-ns (symbol (:ns res))))))
                @(get @awaiting id)
                (swap! awaiting dissoc id)
                (recur)))))))))

(defn- ensure-url-scheme-support!
  "Make sure the built-in client can actually use `scheme`.

  HTTP(S) support is provided by the optional `nrepl/drawbridge` library, so
  require it on demand for those schemes (registering its
  `nrepl.core/url-connect` implementation) and fail helpfully when it's missing.
  The `telnet` scheme is rejected here, mirroring the tty guard in
  `interactive-repl`. Schemes handled by nREPL itself (e.g. `nrepl`) need no
  extra deps."
  [scheme]
  (cond
    (= "telnet" scheme)
    (die "The built-in client does not support the tty transport. Consider using `nc` or `telnet`.\n")

    (#{"http" "https"} scheme)
    (try
      (require 'drawbridge.client)
      (catch FileNotFoundException e
        ;; drawbridge pulls in clj-http and cheshire, which fail the same way,
        ;; so don't blame drawbridge for one of its dependencies.
        (if (str/includes? (str (ex-message e)) "drawbridge")
          (die (format "nREPL: connecting to %s URLs requires the nrepl/drawbridge library on the classpath.\n"
                       scheme))
          (die (format "nREPL: couldn't load drawbridge.client: %s\n" (ex-message e))))))))

(defn- reject-conflicting-url-options!
  "A URL already says where to connect and which transport to use, so options
  that would decide the same things can't be honored. The TLS options are the
  exception - a URL can't carry key material, so those are supplied alongside."
  [url {:keys [port socket transport]}]
  (when socket
    (die (format "nREPL: --socket can't be combined with the URL %s.\n" url)))
  (when port
    (die (format "nREPL: --port can't be combined with the URL %s.\n" url)))
  ;; bencode is the default, so anything else means --transport was given.
  (when (and transport (not= transport #'transport/bencode))
    (die (format "nREPL: --transport can't be combined with the URL %s; the transport comes from the scheme.\n" url))))

(defn- tls-url-connect
  "Connects to a TLS URL (see `tls-url-transports`), as advertised in a
  TLS-enabled server's startup message. The TLS keys must be supplied
  separately via the `:tls-keys-file`/`:tls-keys-str` options."
  [url scheme {:keys [tls-keys-file tls-keys-str]}]
  (when-not (or tls-keys-file tls-keys-str)
    (die (format "nREPL: %s:// URLs require --tls-keys-file or --tls-keys-str.\n" scheme)))
  (let [^java.net.URI uri (try
                            (java.net.URI. ^String url)
                            (catch java.net.URISyntaxException e
                              (die (format "nREPL: Invalid URL %s (%s).\n" url (ex-message e)))))
        host (.getHost uri)
        port (.getPort uri)]
    (when-not host
      (die (format "nREPL: Can't extract a host from the URL %s.\n" url)))
    (when (or (zero? port) (> port 65535))
      (die (format "nREPL: Invalid port %d in URL %s.\n" port url)))
    (nrepl/connect :host host
                   ;; The same default port nrepl.core/url-connect uses for
                   ;; the other nrepl schemes.
                   :port (if (pos? port) port 7888)
                   :transport-fn (options/tls-url-transports scheme)
                   :tls-keys-file tls-keys-file
                   :tls-keys-str tls-keys-str)))

(defn- run-repl
  ([{:keys [server options]}]
   (let [{:keys [host port socket] :or {host "127.0.0.1"}} server
         {:keys [transport tls-keys-file tls-keys-str] :or {transport #'transport/bencode}} options
         scheme (connect-url-scheme host)
         unix-url (unix-socket-url host)
         conflicting (assoc server :transport (:transport options))]
     (run-repl-with-transport
      (cond
        unix-url
        (do (reject-conflicting-url-options! host conflicting)
            (nrepl/connect :socket (second unix-url) :transport-fn (first unix-url)))

        (contains? options/tls-url-transports scheme)
        (do (reject-conflicting-url-options! host conflicting)
            (tls-url-connect host scheme options))

        scheme
        (do (when (or tls-keys-file tls-keys-str)
              (die (format "nREPL: TLS options with a URL are only supported for %s URLs.\n"
                           (str/join " or " (map #(str % "://") (keys options/tls-url-transports))))))
            (reject-conflicting-url-options! host conflicting)
            (ensure-url-scheme-support! scheme)
            (try
              (nrepl/url-connect host)
              (catch java.net.URISyntaxException e
                (die (format "nREPL: Invalid URL %s (%s).\n" host (ex-message e))))
              (catch IllegalArgumentException e
                (die (format "nREPL: %s\n" (ex-message e))))))

        socket
        (nrepl/connect :socket socket :transport-fn transport)

        (and host port)
        (nrepl/connect :host host :port port :transport-fn transport :tls-keys-file tls-keys-file :tls-keys-str tls-keys-str)

        :else
        (die "Must supply host/port, socket, or a URL."))
      options)))
  ([host port]
   (run-repl host port nil))
  ([host port options]
   (run-repl {:server  (cond-> {}
                         host (assoc :host host)
                         port (assoc :port port))
              :options options})))

(defn- handle-interrupt
  [_signal]
  (let [transport (:transport @running-repl)
        client (:client @running-repl)]
    (if (and transport client)
      (doseq [res (nrepl/message client {:op "interrupt"})]
        (when (= ["done" "session-idle"] (:status res))
          (runtime/exit! 0)))
      (runtime/exit! 0))))

(defn display-help
  "Prints the help copy to the screen and exits the program with exit code 0."
  []
  (println (help))
  (exit 0))

(defn display-version
  "Prints nREPL version to the screen and exits the program with exit code 0."
  []
  (println (:version-string version/version))
  (exit 0))

(defn connection-opts
  "Takes map of nREPL CLI options
  Returns map of processed options used to connect or start a nREPL server."
  [options]
  (options/connection-opts options #'run-repl))

(defn server-opts
  "Takes a map of nREPL CLI options
  Returns map of processed options to start an nREPL server."
  [options]
  (options/server-opts options #'run-repl))

(defn interactive-repl
  "Runs an interactive repl if :interactive CLI option is true otherwise
  puts the current thread to sleep
  Takes nREPL server map and processed CLI options map.
  Returns nil."
  [server options]
  (let [transport (:transport options)
        repl-fn (:repl-fn options)
        socket (:socket server)
        host (:host server)
        port (:port server)]
    (when (= transport #'transport/tty)
      (die "The built-in client does not support the tty transport. Consider using `nc` or `telnet`.\n"))
    (if socket
      (repl-fn {:server  server
                :options (merge (when (:color options) colored-output)
                                {:transport transport})})
      (repl-fn host port
               (merge (when (:color options) colored-output)
                      {:transport transport}
                      (select-keys options [:tls-keys-str :tls-keys-file]))))))

(defn connect-to-server
  "Connects to a running nREPL server and runs a REPL. Exits program when REPL
  is closed.
  Takes a map of nREPL CLI options."
  [{:keys [host port socket] :as options}]
  (interactive-repl {:host   host
                     :port   port
                     :socket socket}
                    options)
  (exit 0))

(defn start-server
  "Creates an nREPL server instance.
  Takes map of CLI options.
  Returns nREPL server map."
  [{:keys [port bind socket handler transport greeting ack-port tls-keys-str tls-keys-file]}]
  (nrepl-server/start-server
   :port port
   :bind bind
   :socket socket
   :handler handler
   :transport-fn transport
   :greeting-fn greeting
   :ack-port ack-port
   :tls-keys-str tls-keys-str
   :tls-keys-file tls-keys-file))

(defn dispatch-commands
  "Look at options to dispatch a specified command.
  Takes CLI options map. May return a server map, nil, or exit."
  [options]
  (cond (:help options)    (display-help)
        (:version options) (display-version)
        (:connect options) (connect-to-server (connection-opts options))
        :else (let [options (server-opts options)
                    server (start-server options)]
                (println (server-started-message server options))
                (save-port-file server options)
                (if (:interactive options)
                  (interactive-repl server options)
                  ;; Lock the main thread to prevent process from quitting while
                  ;; the server is running.
                  (runtime/block-forever!)))))

(defn -main
  [& args]
  (try
    (set-signal-handler! "INT" handle-interrupt)
    (let [[options _args] (args->cli-options args)]
      (dispatch-commands options))
    (catch clojure.lang.ExceptionInfo ex
      (let [{:keys [:nrepl/kind ::status]} (ex-data ex)]
        (case kind
          ::exit (clean-up-and-exit status)
          (:nrepl.server/no-filesystem-sockets
           :nrepl.server/invalid-start-request)
          (do
            (binding [*out* *err*]
              (println (ex-message ex)))
            (clean-up-and-exit 2))
          (throw ex))))))
