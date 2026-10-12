(ns nrepl.util.reflect
  "internal host seam"
  {:no-doc true}
  (:require
   #?(:clj [nrepl.util.reflect.jvm :as impl]
      :cljr [nrepl.util.reflect.clr :as impl])))

(def resolve-class impl/resolve-class)
(def class-name impl/class-name)
(def simple-name impl/simple-name)
(def package-name impl/package-name)
(def members impl/members)
(def deftype-field? impl/deftype-field?)
(def cache-key impl/cache-key)
(def classpath-files impl/classpath-files)
(def all-keywords impl/all-keywords)
(def resource-path impl/resource-path)
