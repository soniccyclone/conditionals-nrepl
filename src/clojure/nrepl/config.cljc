(ns nrepl.config
  "Server configuration utilities.
  Some server options can be configured via configuration
  files (local or global).  This namespace provides
  convenient API to work with them.

  The config resolution algorithm is the following:
  The global config file .nrepl/nrepl.edn is merged with
  any local config file (.nrepl.edn) if present.
  The values in the local config file take precedence."
  {:author "Bozhidar Batsov"
   :added  "0.5"}
  (:require
   [clojure.string :as str]
   [nrepl.util.runtime :as runtime]))

(def ^:private home-dir
  "The user's home directory."
  (runtime/home-dir))

(def ^:private xdg-config-default-dir
  "Default XDG config directory (~/.config)."
  (runtime/path-join home-dir ".config"))

(defn- non-empty-env
  "Get the value of the environment variable.
  Return the value of the environment variable if it's non-empty,
  or `nil` otherwise."
  [env-var]
  (let [value (runtime/getenv env-var)]
    (when-not (str/blank? value) value)))

(def ^:private config-file-name "nrepl.edn")

(def config-file
  "nREPL's global configuration file.

  The location is determined with the following precedence:
  * $NREPL_CONFIG_DIR/nrepl.edn
  * $XDG_CONFIG_HOME/nrepl/nrepl.edn
  * ~/.config/nrepl/nrepl.edn (if ~/.config exists)
  * ~/.nrepl/nrepl.edn

  Return the first existing config file in this order. If no config
  file exists in any of these locations, return the default path
  `~/.nrepl/nrepl.edn`."
  (let [candidates
        [(some-> (non-empty-env "NREPL_CONFIG_DIR") (runtime/path-join config-file-name))
         (some-> (non-empty-env "XDG_CONFIG_HOME") (runtime/path-join "nrepl") (runtime/path-join config-file-name))
         (-> xdg-config-default-dir (runtime/path-join "nrepl") (runtime/path-join config-file-name))
         (-> home-dir (runtime/path-join ".nrepl") (runtime/path-join config-file-name))]]
    (or (some (fn [file]
                (when (and file (runtime/path-exists? file)) file))
              candidates)
        (last candidates))))

(def config-dir
  "nREPL's global configuration directory."
  (runtime/parent-dir config-file))

(defn- load-config
  "Load the configuration file identified by `file`.
  Return its contents as EDN if the file exists,
  or an empty map otherwise."
  [file]
  (if (runtime/path-exists? file)
    (runtime/read-edn-file file)
    {}))

(def config
  "Configuration map.
  It's created by merging the global configuration file
  with a local configuration file that would normally
  be placed in the directory in which you're running
  nREPL."
  (merge
   (load-config config-file)
   (load-config (runtime/path-join ".nrepl.edn"))))
