(ns nrepl.util.reflect-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [nrepl.util.reflect :as reflect])
  #?(:clj (:import
           (java.util Map$Entry))
     :cljr (:import
            (System.IO Directory File Path)
            (System.Reflection Assembly MemberInfo MethodInfo ReflectionTypeLoadException))))

(deftype Dt [a_b])

(defn- by-name
  [ms n kind]
  (filter #(and (= n (:name %)) (= kind (:kind %))) ms))

(deftest resolve-class-test
  (is (= String (reflect/resolve-class 'clojure.core 'String)))
  #?(:clj (is (= Map$Entry (reflect/resolve-class 'clojure.core 'java.util.Map$Entry)))
     :cljr (is (= System.Environment+SpecialFolder
                  (reflect/resolve-class 'clojure.core 'System.Environment+SpecialFolder))))
  (is (nil? (reflect/resolve-class 'clojure.core 'map)))
  (is (nil? (reflect/resolve-class 'clojure.core 'no-such-symbol-here))))

(deftest names-test
  #?(:clj (do
            (is (= "java.util.Map$Entry" (reflect/class-name Map$Entry)))
            (is (= "Entry" (reflect/simple-name Map$Entry)))
            (is (= "java.util" (reflect/package-name Map$Entry)))
            (is (nil? (reflect/package-name Integer/TYPE))))
     :cljr (let [list-def (Type/GetType "System.Collections.Generic.List`1")
                 list-int (.MakeGenericType list-def (into-array Type [Int32]))]
             (is (= "System.Environment+SpecialFolder" (reflect/class-name System.Environment+SpecialFolder)))
             (is (= "SpecialFolder" (reflect/simple-name System.Environment+SpecialFolder)))
             (is (= "System" (reflect/package-name System.Environment+SpecialFolder)))
             (is (= "System.Collections.Generic.List`1[System.Int32]" (reflect/class-name list-int)))
             (is (= "List`1" (reflect/simple-name list-int)))
             (is (= "System.Collections.Generic" (reflect/package-name list-int))))))

(deftest members-test
  (let [ms (reflect/members String)
        by (fn [pred] (filter pred ms))]
    (is (= #?(:clj #{:method :field :constructor}
              :cljr #{:method :field :constructor :property})
           (set (map :kind ms))))
    (testing "an instance method"
      (let [m (first (by-name ms #?(:clj "length" :cljr "ToUpperInvariant") :method))]
        (is (false? (:static? m)))
        (is (= String (:declaring-class m)))
        (is (= #?(:clj "length" :cljr "ToUpperInvariant")
               (#?(:clj .getName :cljr .Name)
                ^#?(:clj java.lang.reflect.Method :cljr MethodInfo) (:member m))))))
    (testing "a static method"
      (is (every? :static? (by-name ms #?(:clj "valueOf" :cljr "Join") :method)))
      (is (seq (by-name ms #?(:clj "valueOf" :cljr "Join") :method))))
    (testing "a static field"
      (is (true? (:static? (first (by #(= #?(:clj "CASE_INSENSITIVE_ORDER" :cljr "Empty") (:name %))))))))
    (testing "constructors are named new"
      (is (seq (by #(= :constructor (:kind %)))))
      (is (every? #(= "new" (:name %)) (by #(= :constructor (:kind %))))))
    #?(:cljr
       (testing "a property"
         (let [p (first (by-name ms "Length" :property))]
           (is (false? (:static? p)))
           (is (= String (:declaring-class p)))
           (is (instance? MemberInfo (:member p))))))
    #?(:cljr
       (testing "accessor methods are dropped"
         (is (empty? (by-name ms "get_Length" :method)))
         (is (empty? (by-name ms "op_Equality" :method)))
         (is (not-any? #(and (= :method (:kind %)) (.IsSpecialName ^MethodInfo (:member %))) ms))))))

#?(:cljr
   (deftest static-members-test
     (testing "static property"
       (let [p (first (by-name (reflect/members Environment) "Version" :property))]
         (is (true? (:static? p)))
         (is (= Environment (:declaring-class p)))))
     (testing "static event"
       (let [e (first (by-name (reflect/members Console) "CancelKeyPress" :event))]
         (is (true? (:static? e)))))
     (testing "instance event"
       (let [e (first (by-name (reflect/members System.Diagnostics.Process) "Exited" :event))]
         (is (false? (:static? e)))
         (is (= System.Diagnostics.Process (:declaring-class e)))))
     (testing "inherited instance members report their declaring class"
       (let [m (first (by-name (reflect/members Exception) "GetHashCode" :method))]
         (is (= Object (:declaring-class m)))))))

(deftest deftype-field-test
  (let [fields (filter #(= :field (:kind %)) (reflect/members Dt))
        field (first (filter #(= "a_b" (:name %)) fields))
        string-field (first (filter #(= #?(:clj "CASE_INSENSITIVE_ORDER" :cljr "Empty") (:name %))
                                    (reflect/members String)))]
    (is (some? field))
    (is (some? string-field))
    (is (true? (reflect/deftype-field? field Dt)))
    (is (false? (reflect/deftype-field? string-field String)))
    (is (false? (reflect/deftype-field?
                  (first (filter #(= :method (:kind %)) (reflect/members Dt)))
                  Dt)))))

#?(:clj
   (deftest cache-key-test
     (is (= (reflect/cache-key) (reflect/cache-key)))
     (let [before (reflect/cache-key)]
       (try
         (System/setProperty "fake.class.path" "/nrepl/reflect-test/nowhere")
         (is (not= before (reflect/cache-key)))
         (finally
           (System/clearProperty "fake.class.path")))
       (is (= before (reflect/cache-key))))))

#?(:cljr
   (deftest cache-key-test
     (reflect/cache-key)
     (is (= (reflect/cache-key) (reflect/cache-key)))
     (testing "a new assembly changes the key"
       (let [before (reflect/cache-key)]
         (eval '(deftype CacheKeyProbe []))
         (is (not= before (reflect/cache-key)))))
     (testing "the load path changes the key"
       (let [before (reflect/cache-key)
             original (Environment/GetEnvironmentVariable "CLOJURE_LOAD_PATH")]
         (try
           (Environment/SetEnvironmentVariable "CLOJURE_LOAD_PATH" (str original "/nrepl/reflect-test/nowhere"))
           (is (not= before (reflect/cache-key)))
           (finally
             (Environment/SetEnvironmentVariable "CLOJURE_LOAD_PATH" original)))
         (is (= before (reflect/cache-key)))))))

(deftest classpath-files-test
  (let [files (reflect/classpath-files)]
    #?(:clj (do
              (is (some #{"clojure/core.clj"} files))
              (is (some #{"nrepl/util/reflect.cljc"} files))
              (is (some #{"java/lang/String.class"} files)))
       :cljr (do
               (is (some #{"nrepl/util/reflect.cljc"} files))
               (is (some #{"nrepl/util/reflect/clr.cljr"} files))
               (is (some #{"System/String.class"} files))
               (is (some #{"System/Collections/Generic/List`1.class"} files))
               (testing "nested and non-public types are left out"
                 (is (not-any? #(re-find #"\+" %) files))
                 (let [internal (Type/GetType "System.Runtime.CompilerServices.CastHelpers")]
                   (is (some? internal))
                   (is (not (.IsPublic internal)))
                   (is (not (some #{"System/Runtime/CompilerServices/CastHelpers.class"} files)))))
               (testing "paths are separated with a slash"
                 (is (not-any? #(re-find #"\\" %) files)))))
    (is (= (count files) (count (set files))))))

#?(:cljr
   (deftest classpath-files-load-path-test
     (let [root (Path/Combine (Path/GetTempPath) (str "nrepl-reflect-test-" (Guid/NewGuid)))
           other (Path/Combine root "other")
           original (Environment/GetEnvironmentVariable "CLOJURE_LOAD_PATH")
           touch (fn [dir name]
                   (let [path (Path/Combine dir name)]
                     (Directory/CreateDirectory (Path/GetDirectoryName path))
                     (File/WriteAllText path "x")))
           sep (str Path/PathSeparator)]
       (try
         (touch root "a/b.clj")
         (touch root "c.cljr")
         (touch root "e/f/g.cljc")
         (touch root "d.txt")
         (touch root "h.CLJ")
         (touch other "o.clj")
         (Environment/SetEnvironmentVariable
          "CLOJURE_LOAD_PATH"
          (str root sep sep (Path/Combine root "missing") sep other sep root))
         (let [listed (reflect/classpath-files)
               files (set listed)]
           (is (every? files ["a/b.clj" "c.cljr" "e/f/g.cljc" "o.clj" "other/o.clj"]))
           (is (not (contains? files "d.txt")))
           (is (not (contains? files "h.CLJ")))
           (is (= (count listed) (count files))))
         (finally
           (Environment/SetEnvironmentVariable "CLOJURE_LOAD_PATH" original)
           (Directory/Delete root true))))))

#?(:cljr
   (deftest classpath-files-unloadable-assembly-test
     (let [types (#'nrepl.util.reflect.clr/assembly-types
                  (proxy [Assembly] []
                    (GetTypes []
                      (throw (ReflectionTypeLoadException.
                              (into-array Type [String nil])
                              (into-array Exception [(Exception. "unloadable")]))))))]
       (is (= [String] (vec types))))
     (is (= [] (vec (#'nrepl.util.reflect.clr/assembly-types
                     (proxy [Assembly] []
                       (GetTypes [] (throw (NotSupportedException. "dynamic"))))))))))

(deftest all-keywords-test
  (let [k (keyword "nrepl.util.reflect-test" "interned-for-test")]
    (is (some #{k} (reflect/all-keywords)))
    (is (every? keyword? (take 100 (reflect/all-keywords))))))

#?(:cljr
   (deftest all-keywords-skips-collected-keywords-test
     (let [table-field (.GetField clojure.lang.Keyword "_symKeyMap"
                                  (enum-or System.Reflection.BindingFlags/NonPublic
                                           System.Reflection.BindingFlags/Static))
           dead-targets (fn []
                          (let [table (.GetValue table-field nil)
                                values (.GetValue (.GetProperty ^Type (class table) "Values") table)]
                            (count (filter nil? (map (fn [^WeakReference r] (.Target r)) values)))))]
       (dotimes [i 300]
         (keyword "nrepl.reflect-test.garbage" (str i)))
       (GC/Collect)
       (GC/WaitForPendingFinalizers)
       (is (pos? (dead-targets)))
       (is (every? keyword? (reflect/all-keywords))))))

(defn- slashed
  [s]
  (str/replace s "\\" "/"))

(deftest resource-path-test
  (is (re-find #"clojure/core\.clj$" (slashed (reflect/resource-path "clojure/core.clj"))))
  (is (re-find #"nrepl/util/reflect\.cljc$" (slashed (reflect/resource-path "nrepl/util/reflect.cljc"))))
  (is (nil? (reflect/resource-path "no/such/resource.clj"))))
