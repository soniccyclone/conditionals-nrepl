(ns nrepl.cmdline.options
  "Command-line option parsing, help text and server option processing,
  shared by the nREPL command-line entry points."
  {:author "Chas Emerick"}
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [nrepl.config :as config]
   [nrepl.misc :refer [parse-int]]
   [nrepl.server :as nrepl-server]
   [nrepl.socket :as socket]
   [nrepl.transport :as transport]
   [nrepl.util.runtime :as runtime]))

(defn exit
  "Requests that the process exit with the given `status`.  Does not
  return."
  [status]
  ;; :nrepl/kind is our shared (ns independent) ExceptionInfo discriminator
  (throw (ex-info nil {:nrepl/kind :nrepl.cmdline/exit :nrepl.cmdline/status status})))

(defn die
  "`Print`s items in `msg` to *err* and then exits with a status of 2."
  [& msg]
  (binding [*out* *err*]
    (doseq [m msg] (print m)))
  (exit 2))

(defn connect-url-scheme
  "If `s` looks like a URI with a scheme (e.g. \"nrepl://…\" or \"http://…\"),
  return the lower-cased scheme; otherwise nil."
  [s]
  (when (string? s)
    (some-> (re-find #"\A([a-zA-Z][a-zA-Z0-9+.-]*)://" s) second str/lower-case)))

(def ^:private unix-url-transports
  "Transports keyed by the scheme a filesystem-socket server advertises. Those
  URLs are opaque (e.g. `nrepl+unix:/tmp/nrepl.sock`), not authority-based, so
  they need handling separately from the `scheme://host:port` ones."
  (into {}
        (map (fn [transport] [(str (transport/uri-scheme transport) "+unix") transport]))
        [#'transport/bencode #'transport/edn]))

(defn unix-socket-url
  "If `s` is a URL a filesystem-socket server advertises, returns a
  `[transport path]` pair; otherwise nil."
  [s]
  (when (string? s)
    (when-let [[_ scheme path] (re-find #"\A([a-zA-Z][a-zA-Z0-9+.-]*):(?!//)(.+)\z" s)]
      (when-let [transport (unix-url-transports (str/lower-case scheme))]
        [transport path]))))

(def tls-url-transports
  "The transports the built-in client can use over TLS, keyed by the URL
  scheme a TLS-enabled server advertises in its startup message - the
  transport's own URI scheme with an appended \"s\", the same rule
  `nrepl.socket/as-nrepl-uri` uses to produce it."
  (into {}
        (map (fn [transport] [(str (transport/uri-scheme transport) "s") transport]))
        [#'transport/bencode #'transport/edn]))

(def ^:private option-shorthands
  {"-i" "--interactive"
   "-r" "--repl"
   "-f" "--repl-fn"
   "-c" "--connect"
   "-C" "--color"
   "-b" "--bind"
   "-h" "--host"
   "-p" "--port"
   "-s" "--socket"
   "-m" "--middleware"
   "-t" "--transport"
   "-n" "--handler"
   "-v" "--version"})

(def ^:private unary-options
  #{"--interactive"
    "--connect"
    "--color"
    "--help"
    "--version"
    "--verbose"})

(defn- expand-shorthands
  "Expand shorthand options into their full forms."
  [args]
  (map (fn [arg] (or (option-shorthands arg) arg)) args))

(defn- keywordize-options [options]
  (reduce-kv
   #(assoc %1 (keyword (str/replace-first %2 "--" "")) %3)
   {}
   options))

(defn- split-args
  "Convert `args` into a map of options + a list of args.
  Unary options are set to true during this transformation.
  Returns a vector combining the map and the list."
  [args]
  (loop [[arg & rem-args :as args] args
         options {}]
    (if-not (and arg (re-matches #"-.*" arg))
      [options args]
      (if (unary-options arg)
        (recur rem-args
               (assoc options arg true))
        (recur (rest rem-args)
               (assoc options arg (first rem-args)))))))

(defn help
  []
  ;; Updating this? Remember to also update server.adoc
  "Usage:

  -i/--interactive            Start nREPL and connect to it with the built-in client.
  -f/--repl-fn REPL-FN        The function to run as the built-in REPL when combined with --interactive; defaults to `nrepl.cmdline/run-repl`. Must be expressed as a namespace-qualified symbol. The underlying var will be automatically `require`d.
  -c/--connect                Connect to a running nREPL with the built-in client.
  -C/--color                  Use colors to differentiate values from output in the REPL. Must be combined with --interactive.
  -b/--bind ADDR              Bind address, by default \"127.0.0.1\".
  -h/--host ADDR              Host address to connect to when using --connect. Defaults to \"127.0.0.1\". May also be a URL the server advertises on startup - nrepl://host:port, nrepls://host:port for TLS servers (requires --tls-keys-file or --tls-keys-str), nrepl+unix:/path for filesystem sockets, or http(s)://host/repl with nrepl/drawbridge on the classpath. The transport comes from the scheme, so --transport, --port and --socket can't be combined with a URL.
  -p/--port PORT              Start nREPL on PORT. Defaults to 0 (random port) if not specified.
  -s/--socket PATH            Start nREPL on filesystem socket at PATH or nREPL to connect to when using --connect.
  --tls-keys-file FILE        Enable TLS, reading the CA certificate, own certificate and private key from FILE. Applies both when starting a server and when using --connect. See the TLS section of the manual for details on generating the keys.
  --tls-keys-str KEYS         Same as --tls-keys-file, but the key material is passed directly as a string.
  --ack ACK-PORT              Acknowledge the port of this server to another nREPL server running on ACK-PORT.
  -n/--handler HANDLER        The nREPL message handler to use for each incoming connection; defaults to the result of `(nrepl.server/default-handler)`. Must be expressed as a namespace-qualified symbol. The underlying var will be automatically `require`d.
  -m/--middleware MIDDLEWARE  A sequence of vars (expressed as namespace-qualified symbols), representing middleware you wish to mix in to the nREPL handler. The underlying vars will be automatically `require`d. If unavailable, the server will exit unless symbols are marked with ^:optional metadata.
  -t/--transport TRANSPORT    The transport to use (default `nrepl.transport/bencode`), expressed as a namespace-qualified symbol. The underlying var will be automatically `require`d.
  --help                      Show this help message.
  -v/--version                Display the nREPL version.
  --verbose                   Show verbose output.")

(defn- require-and-resolve
  "Attempts to resolve the config `key`'s `value` as a namespaced symbol
  and returns the related var if successful.  Otherwise calls `die`."
  [key sym]
  (when-not (symbol? sym)
    (die (format "nREPL %s: %s is not a symbol\n" (name key) (pr-str sym))))
  (when-not (namespace sym)
    (die (format "nREPL %s: %s has no namespace\n" (name key) sym)))
  (requiring-resolve sym))

(defn- resolve-mware-by-name [sym]
  (try (require-and-resolve :middleware sym)
       (catch #?(:clj java.io.FileNotFoundException :cljr System.IO.FileNotFoundException) _
         (when-not (:optional (meta sym))
           (die (format "nREPL middleware: unable to resolve %s\n" sym))))))

(defn- expand-list-of-mware
  [var]
  (let [x @var]
    (if (sequential? x)
      (into [] (keep resolve-mware-by-name) x)
      [var])))

(defn- ->mw-list
  [middleware-var-strs]
  (into [] (comp (keep #(resolve-mware-by-name (symbol %)))
                 (mapcat expand-list-of-mware))
        middleware-var-strs))

(defn- build-handler
  "Build an nREPL handler from `middleware`.
  `middleware` is a sequence of vars or string which can be resolved
  to vars, representing middleware you wish to mix in to the nREPL
  handler. Vars can resolve to a sequence of vars, in which case
  they'll be flattened into the list of middleware."
  [middleware]
  (apply nrepl-server/default-handler (->mw-list middleware)))

(defn- ->int [x]
  (cond
    (nil? x) x
    (number? x) x
    :else (parse-int x)))

(defn- sanitize-middleware-option
  "Sanitize the middleware option.  In the config it can be either a
  symbol or a vector of symbols."
  [mw-opt]
  (if (symbol? mw-opt)
    [mw-opt]
    mw-opt))

(defn parse-cli-values
  "Converts relevant command line argument values to their config
  representation."
  [options]
  (reduce-kv (fn [result k v]
               (case k
                 (:handler :transport :middleware) (assoc result k (edn/read-string v))
                 result))
             options
             options))

(defn- meta-merge
  "Metadata-aware function used for specifying merge behavior."
  [config-opt cli-opt]
  (if (some (comp :concat meta) [config-opt cli-opt]) (into config-opt cli-opt) cli-opt))

(defn args->cli-options
  "Takes CLI args list and returns vector of parsed options map and
  remaining args."
  [args]
  (let [[options _args] (split-args (expand-shorthands args))
        merge-config (partial merge-with meta-merge config/config)
        options (-> options
                    (keywordize-options)
                    (parse-cli-values)
                    (merge-config))]
    [options _args]))

(defn- options->transport
  "Takes a map of nREPL CLI options.
  Returns either a default transport or the value of :transport."
  [options]
  (or (some->> options
               (:transport)
               (require-and-resolve :transport))
      #'transport/bencode))

(defn- options->handler
  "Takes a map of nREPL CLI options and list of middleware.
  Returns a request handler function.
  If some handler was explicitly passed we'll use it, otherwise we'll build
  one from whatever was passed via --middleware"
  [options middleware]
  (or (some->> options
               (:handler)
               (require-and-resolve :handler))
      (build-handler middleware)))

(defn- options->ack-port
  "Takes a map of nREPL CLI options.
  Returns integer ack port or nil."
  [options]
  (->int (:ack options)))

(defn- options->repl-fn
  "Takes a map of nREPL CLI options and the entry point's default REPL function.
  Returns either the :repl-fn config option or the default."
  [options default-repl-fn]
  (or (some->> options
               (:repl-fn)
               (symbol)
               (require-and-resolve :repl-fn))
      default-repl-fn))

(defn- options->greeting
  "Takes a map of nREPL CLI options and the selected transport for the server.
  Returns a greeting function or nil."
  [_options transport]
  #?(:clj (when (= transport #'transport/tty)
            #'transport/tty-greeting)))

(defn connection-opts
  "Takes map of nREPL CLI options and, optionally, the entry point's default
  REPL function.
  Returns map of processed options used to connect or start a nREPL server."
  ([options]
   (connection-opts options nil))
  ([options default-repl-fn]
   {:port (->int (:port options))
    :host (:host options)
    :socket (:socket options)
    :transport (options->transport options)
    :repl-fn (options->repl-fn options default-repl-fn)
    :tls-keys-str (:tls-keys-str options)
    :tls-keys-file (:tls-keys-file options)}))

(defn server-opts
  "Takes a map of nREPL CLI options and, optionally, the entry point's default
  REPL function.
  Returns map of processed options to start an nREPL server."
  ([options]
   (server-opts options nil))
  ([options default-repl-fn]
   (let [middleware (sanitize-middleware-option (:middleware options))
         {:keys [host port socket transport tls-keys-str tls-keys-file]} (connection-opts options)]
     (merge options
            {:host host
             :port port
             :socket socket
             :transport transport
             :bind (:bind options)
             :middleware middleware
             :tls-keys-str tls-keys-str
             :tls-keys-file tls-keys-file
             :handler (options->handler options middleware)
             :greeting (options->greeting options transport)
             :ack-port (options->ack-port options)
             :repl-fn (options->repl-fn options default-repl-fn)}))))

(defn server-started-message
  "Returns nREPL server started message that some tools rely on to parse the
  connection details from.
  Takes nREPL server map and processed CLI options map.
  Returns connection header string."
  [{:keys [host port socket] :as server} options]
  (let [transport (:transport options)
        uri (socket/as-nrepl-uri-string server (transport/uri-scheme transport))]
    ;; The format here is important, as some tools (e.g. CIDER) parse the string
    ;; to extract from it the host and the port to connect to
    (if socket
      (str "nREPL server started on socket " uri)
      (format "nREPL server started on port %d on host %s - %s"
              port host uri))))

(defn save-port-file
  "Writes a file relative to project classpath with port number so other tools
  can infer the nREPL server port.
  Takes nREPL server map and processed CLI options map.
  Returns nil."
  [server _options]
  ;; Many clients look for this file to infer the port to connect to
  (let [port (:port server)
        port-file ".nrepl-port"]
    (runtime/delete-on-exit! port-file)
    (spit port-file port)))
