(ns nrepl.util.completion-test
  "Unit tests for completion utilities."
  (:require [clojure.set :as set]
            [clojure.test :refer :all]
            [matcher-combinators.matchers :as mc]
            [nrepl.util.completion :as completion :refer [completions]]
            [nrepl.test-helpers :refer [is+]])
  #?(:clj (:import (java.io File))
     :cljr (:import (System.Diagnostics Process)
                    (System.Globalization CultureInfo)
                    (System.IO Path))))

(def t-var "var" nil)
(defn t-fn "fn" [x] x)
(defmacro t-macro "macro" [y] y)
(def t-ord-Zed nil)
(def t-ord-alpha nil)
(def t-ord-Beta nil)
(def t-ord-gamma nil)

#?(:cljr (deftype Dummy []))

(def ^:private dummy-class-prefix
  #?(:clj "nrepl.t"
     :cljr "nrepl.util.completion_"))

(def ^:private dummy-class-name
  #?(:clj "nrepl.test.Dummy"
     :cljr "nrepl.util.completion_test.Dummy"))

(def ^:private make-alias
  #?(:clj "jio"
     :cljr "cio"))

(def ^:private make-candidates
  #?(:clj ["jio/make-input-stream" "jio/make-output-stream" "jio/make-parents" "jio/make-reader" "jio/make-writer"]
     :cljr ["cio/make-binary-reader" "cio/make-binary-writer" "cio/make-input-stream" "cio/make-output-stream" "cio/make-text-reader" "cio/make-text-writer"]))

(def ^:private clojure-core-namespaces
  #?(:clj ["clojure.core" "clojure.core.ArrayChunk" "clojure.core.ArrayManager" "clojure.core.IVecImpl" "clojure.core.Vec" "clojure.core.VecNode" "clojure.core.VecSeq" "clojure.core.protocols" "clojure.core.protocols.InternalReduce"]
     :cljr ["clojure.core" "clojure.core.ArrayChunk" "clojure.core.ArrayManager" "clojure.core.IVecImpl" "clojure.core.Vec" "clojure.core.VecNode" "clojure.core.VecSeq" "clojure.core.protocols" "clojure.core.protocols.InternalReduce" "clojure.core.server"]))

(defn- candidates
  "Return only the candidate names without any additional
  metadata for them."
  ([prefix]
   (candidates prefix *ns*))
  ([prefix ns]
   (map :candidate (completions prefix ns))))

(defn- distinct-candidates?
  "Return true if every candidate occurs in the list of
   candidates only once."
  ([prefix]
   (distinct-candidates? prefix *ns*))
  ([prefix ns]
   (apply distinct? (candidates prefix ns))))

(deftest completions-test
  (testing "var completion"
    (is+ #?(:clj ["alength" "alias" "all-ns" "alter" "alter-meta!" "alter-var-root" "aset-long"]
            :cljr ["add-ns-load-mapping" "alength" "alias" "alias-type" "all-ns" "alter" "alter-meta!" "alter-var-root" "aset-long" "assembly-load" "assembly-load-file" "assembly-load-from" "assembly-load-with-partial-name"])
         (candidates "al" 'clojure.core))

    (is+ make-candidates
         (candidates (str make-alias "/make") 'clojure.core))

    (is+ #?(:clj ["clojure.core/alter" "clojure.core/alter-meta!" "clojure.core/alter-var-root"]
            :cljr ["clojure.core/alias-type" "clojure.core/alter" "clojure.core/alter-meta!" "clojure.core/alter-var-root"])
         (candidates "clojure.core/alt" 'clojure.core))

    (is+ [] (candidates "fake-ns-here/"))

    (is+ [] (candidates "/")))

  (testing "namespace completion"
    (is+ #?(:clj ["nrepl.util.completion" "nrepl.util.completion-test"]
            :cljr ["nrepl.util.completion" "nrepl.util.completion-test" "nrepl.util.completion_test.Dummy"])
         (candidates "nrepl.util.comp"))

    (is+ (mc/embeds clojure-core-namespaces)
         (candidates "clojure.co")))

  (testing "namespace completion with host classes"
    (is+ (mc/embeds [dummy-class-name])
         (candidates dummy-class-prefix)))

  (testing "instance methods completion"
    (is+ [#?(:clj ".toUpperCase" :cljr ".ToUpper") #?@(:cljr [".ToUpperInvariant"])]
         (candidates #?(:clj ".toUpper" :cljr ".ToUpper")))
    (is (distinct-candidates? #?(:clj ".toString" :cljr ".ToString"))))

  (testing "static members completion"
    #?(:clj (do
              (is+ ["System/out"] (candidates "System/o"))
              (is+ ["java.lang.System/out"] (candidates "java.lang.System/out"))

              (is+ (mc/embeds ["String/valueOf"])  (candidates "String/"))
              (is (distinct-candidates? "String/v"))

              (is+ (mc/mismatch (mc/embeds ["String/indexOf" ".indexOf"])) (candidates "String/")))
       :cljr (do
               (is+ ["Math/Abs"] (candidates "Math/Ab"))
               (is+ ["System.Math/Abs"] (candidates "System.Math/Abs"))

               (is+ (mc/embeds ["String/Join"])  (candidates "String/"))
               (is (distinct-candidates? "String/Jo"))

               (is+ (mc/mismatch (mc/embeds ["String/ToUpper" ".ToUpper"])) (candidates "String/")))))

  (testing "candidate types"
    (is+ (mc/embeds [{:candidate "t-var"
                      :ns "nrepl.util.completion-test"
                      :type :var}])
         (completions "t-var" 'nrepl.util.completion-test))
    (is+ (mc/embeds [{:candidate "t-fn"
                      :ns "nrepl.util.completion-test"
                      :type :function}])
         (completions "t-fn" 'nrepl.util.completion-test))
    (is+ (mc/embeds [{:candidate "t-macro"
                      :ns "nrepl.util.completion-test"
                      :type :macro}])
         (completions "t-macro" 'nrepl.util.completion-test))
    (is+ (mc/embeds [{:candidate "unquote" :type :var, :ns "clojure.core"}])
         (completions "unquote" 'clojure.core))
    (is+ (mc/embeds [{:candidate "if" :type :special-form}])
         (completions "if" 'clojure.core))
    (is+ (mc/embeds [{:candidate #?(:clj "UnsatisfiedLinkError" :cljr "DllNotFoundException")
                      :type :class}])
         (completions #?(:clj "Unsatisfied" :cljr "DllNotFound") 'clojure.core))
    ;; ns with :doc meta
    (is+ (mc/embeds [{:candidate "clojure.core"
                      :file "clojure/core.clj"
                      :type :namespace}])
         (completions "clojure.core" 'clojure.core))
    (is+ (mc/embeds [{:candidate "clojure.core"
                      :type :namespace
                      :file "clojure/core.clj"}])
         (completions "clojure.core" 'clojure.core))
    ;; ns with docstring argument
    (is+ (mc/embeds [{:candidate "nrepl.util.completion-test" :type :namespace}])
         (completions "nrepl.util.completion-test" 'clojure.core))
    (is+ (mc/embeds [{:candidate #?(:clj "Integer/parseInt" :cljr "Int32/Parse")
                      :type :static-method}])
         (completions #?(:clj "Integer/parseInt" :cljr "Int32/Parse") 'clojure.core))
    (is+ (mc/embeds [{:candidate #?(:clj "File/separator" :cljr "Path/DirectorySeparatorChar")
                      :type :static-field}])
         (completions #?(:clj "File/" :cljr "Path/") 'nrepl.util.completion-test))
    (is+ (mc/embeds [{:candidate #?(:clj ".toString" :cljr ".ToString") :type :method}])
         (completions #?(:clj ".toString" :cljr ".ToString") 'clojure.core))))

#?(:cljr
   (deftest property-and-event-completions-test
     (testing "instance property"
       (is+ (mc/embeds [{:candidate ".Length" :type :field}])
            (completions ".Length" 'clojure.core))
       (is+ (mc/embeds [{:candidate "String/.Length" :type :field}])
            (completions "String/.Len" 'clojure.core)))
     (testing "static property"
       (is+ (mc/embeds [{:candidate "Environment/Version" :type :static-field}])
            (completions "Environment/Vers" 'clojure.core)))
     (testing "instance event"
       (is+ (mc/embeds [{:candidate ".Exited" :type :field}])
            (completions ".Exited" 'nrepl.util.completion-test))
       (is+ (mc/embeds [{:candidate "Process/.Exited" :type :field}])
            (completions "Process/.Exit" 'nrepl.util.completion-test)))
     (testing "static event"
       (is+ (mc/embeds [{:candidate "Console/CancelKeyPress" :type :static-field}])
            (completions "Console/Cancel" 'clojure.core)))))

#?(:cljr
   (deftest namespace-sources-test
     (testing "clojure.core comes from the runtime directory with a load-path relative file"
       (is+ (mc/embeds [{:candidate "clojure.core"
                         :file "clojure/core.clj"
                         :type :namespace}])
            (completions "clojure.core" 'clojure.core)))
     (testing "namespaces that are not loaded yet"
       (is+ (mc/embeds [{:candidate "clojure.data" :file "clojure/data.clj" :type :namespace}])
            (completions "clojure.da" 'clojure.core)))
     (testing "a .cljr namespace is a candidate"
       (is+ (mc/embeds [{:candidate "nrepl.util.reflect.clr"
                         :file "nrepl/util/reflect/clr.cljr"
                         :type :namespace}])
            (completions "nrepl.util.reflect.c" 'clojure.core)))
     (testing "a namespace with a .cljr and a .clj file is listed once"
       (is (= 1 (count (filter #{"nrepl.socket"} (candidates "nrepl.socket"))))))))

#?(:cljr
   (def ^:private culture-keywords [(keyword "I\u0307dot") (keyword "\u0131dot")]))

#?(:cljr
   (defn- with-culture
     [name f]
     (let [original CultureInfo/CurrentCulture]
       (try
         (set! CultureInfo/CurrentCulture (CultureInfo. ^String name))
         (f)
         (finally
           (set! CultureInfo/CurrentCulture original))))))

#?(:cljr
   (deftest culture-test
     (let [found (fn []
                   (filter (set (map str culture-keywords))
                           (candidates ":I" 'nrepl.util.completion-test)))
           invariant (with-culture "" found)
           turkish (with-culture "tr-TR" found)]
       (is (= [(str (first culture-keywords))] invariant))
       (is (= invariant turkish)))))

(deftest default-package-class-completion-test
  (with-redefs [completion/primitive-cache (atom {})
                completion/classes-on-classpath (fn [] ["DefaultPkgClass" "a.b.Other"])
                completion/root-packages-on-classpath (fn [] #{"a"})
                completion/namespaces&files-on-classpath (fn [] [])]
    (is+ (mc/embeds [{:candidate "DefaultPkgClass" :type :class}])
         (completions "DefaultPkg" 'nrepl.util.completion-test))))

(deftest string-helpers-test
  (let [soft-hyphen "a\u00ADb"
        combining "I\u0307x"]
    (testing "matching is ordinal"
      (is (not (#'completion/starts-with? soft-hyphen "ab")))
      (is (not (#'completion/ends-with? soft-hyphen "ab")))
      (is (not (#'completion/includes? soft-hyphen "ab")))
      (is (= -1 (#'completion/index-of soft-hyphen "ab")))
      (is (= -1 (#'completion/last-index-of soft-hyphen "ab")))
      (is (#'completion/starts-with? combining "I"))
      (is (= 0 (#'completion/index-of combining "I")))
      (is (#'completion/ends-with? combining "\u0307x"))
      (is (= 1 (#'completion/last-index-of combining "\u0307"))))
    (testing "positions and absence"
      (is (= 1 (#'completion/index-of "a/b/c" "/")))
      (is (= 3 (#'completion/last-index-of "a/b/c" "/")))
      (is (= -1 (#'completion/index-of "abc" "x")))
      (is (= -1 (#'completion/last-index-of "abc" "x")))
      (is (#'completion/ends-with? "x.cljc" ".cljc"))
      (is (not (#'completion/ends-with? "x.cljc" ".clj"))))
    (testing "case and order"
      (is (#'completion/upper-case-char? \I))
      (is (not (#'completion/upper-case-char? \i)))
      (is (pos? (#'completion/ordinal-compare "a" "B")))
      (is (neg? (#'completion/ordinal-compare "B" "a")))
      (is (zero? (#'completion/ordinal-compare "a" "a"))))))

(deftest candidate-order-test
  (is+ ["t-ord-Beta" "t-ord-Zed" "t-ord-alpha" "t-ord-gamma"]
       (candidates "t-ord" 'nrepl.util.completion-test)))

(deftest keyword-completions-test
  (testing "colon prefix"
    (is+ (mc/embeds [":doc" ":refer" ":refer-clojure"])
         (candidates ":" *ns*)))

  (testing "unqualified keywords"
    (do #{:t-key-foo :t-key-bar :t-key-baz :t-key/quux}
        (is+ (mc/embeds [":t-key-foo" ":t-key-bar" ":t-key-baz" ":t-key/quux"])
             (candidates ":t-key" *ns*))))

  (testing "auto-resolved unqualified keywords"
    (do #{::foo ::bar ::baz}
        (is+ (mc/embeds [":nrepl.util.completion-test/bar" ":nrepl.util.completion-test/baz"])
             (candidates ":nrepl.util.completion-test/ba" *ns*))
        (is+ (mc/embeds ["::bar" "::baz"])
             (candidates "::ba" 'nrepl.util.completion-test))))

  (testing "auto-resolved qualified keywords"
    (do #{:nrepl.core/aliased-one :nrepl.core/aliased-two}
        (require '[nrepl.core :as core])
        (is+ (mc/embeds ["::core/aliased-one" "::core/aliased-two"])
             (candidates "::core/ali" *ns*))))

  (testing "namespace aliases"
    (is+ (mc/embeds ["::set/"])
         (candidates "::s" 'nrepl.util.completion-test)))

  (testing "namespace aliases without namespace"
    (is+ [] (candidates "::/" *ns*))))
