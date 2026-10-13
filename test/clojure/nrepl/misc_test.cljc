(ns nrepl.misc-test
  (:require [clojure.test :refer [deftest is]]
            [nrepl.misc :as misc]
            [nrepl.test-helpers :as th])
  #?(:clj (:import [java.net URI])))

#?(:clj
   (deftest parse-java-version-test
     (is (= misc/java-version (misc/parse-java-version)))))

(deftest sanitize-meta-test
  (is (not-empty (:file (misc/sanitize-meta {:file "clojure/core.clj"}))))

  (is (= "/foo/bar/baz.clj"
         (:file (misc/sanitize-meta {:file "/foo/bar/baz.clj"}))))

  (let [file (th/as-file "/foo/bar/baz.clj")]
    (is (= (str file)
           (:file (misc/sanitize-meta {:file file})))))

  (is (= #?(:clj "https://foo.bar" :cljr "https://foo.bar/")
         (:file (misc/sanitize-meta {:file #?(:clj (.toURL (URI. "https://foo.bar"))
                                              :cljr (System.Uri. "https://foo.bar"))})))))
