(ns nrepl.util.runtime
  "internal host seam"
  {:no-doc true}
  (:require
   #?(:clj [nrepl.util.runtime.jvm :as impl]
      :cljr [nrepl.util.runtime.clr :as impl])))

(def host impl/host)
(def host-version impl/host-version)
#?(:clj (def parse-java-version impl/parse-java-version))
(def java-version impl/java-version)
(def attach-self-enabled? impl/attach-self-enabled?)
(def class-name-of impl/class-name-of)
(def print-stack-trace impl/print-stack-trace)
(def load-file-bindings impl/load-file-bindings)
(def can-stop-threads? impl/can-stop-threads?)
(def exit! impl/exit!)
(def block-forever! impl/block-forever!)
(def on-sigint! impl/on-sigint!)
(def delete-on-exit! impl/delete-on-exit!)
(def home-dir impl/home-dir)
(def getenv impl/getenv)
(def path-join impl/path-join)
(def path-exists? impl/path-exists?)
(def parent-dir impl/parent-dir)
(def read-edn-file impl/read-edn-file)
(def version-string impl/version-string)
