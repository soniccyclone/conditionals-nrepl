(ns nrepl.version
  "nREPL version information, read from the Maven-generated POM properties
  at build time."
  {:author "Colin Jones"
   :added  "0.5"}
  (:require [nrepl.misc :as misc]
            [nrepl.util.runtime :as runtime]))

(def ^:private version-string
  "Current version of nREPL as a string.
  See also `version`."
  (runtime/version-string))

(def version
  "Current version of nREPL.
  Map of :major, :minor, :incremental, :qualifier, and :version-string."
  (assoc (->> version-string
              (re-find #"(\d+)\.(\d+)\.(\d+)-?(.*)")
              rest
              (map #(try (misc/parse-int %) (catch Exception _e nil)))
              (zipmap [:major :minor :incremental :qualifier]))
         :version-string version-string))
