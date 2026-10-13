(ns nrepl.util.reflect-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [nrepl.util.reflect :as reflect])
  (:import
   (java.util Map$Entry)))

(deftype Dt [a_b])

(deftest resolve-class-test
  (is (= String (reflect/resolve-class 'clojure.core 'String)))
  (is (= Map$Entry (reflect/resolve-class 'clojure.core 'java.util.Map$Entry)))
  (is (nil? (reflect/resolve-class 'clojure.core 'map)))
  (is (nil? (reflect/resolve-class 'clojure.core 'no-such-symbol-here))))

(deftest names-test
  (is (= "java.util.Map$Entry" (reflect/class-name Map$Entry)))
  (is (= "Entry" (reflect/simple-name Map$Entry)))
  (is (= "java.util" (reflect/package-name Map$Entry)))
  (is (nil? (reflect/package-name Integer/TYPE))))

(deftest members-test
  (let [ms (reflect/members String)
        by (fn [pred] (filter pred ms))]
    (is (= #{:method :field :constructor} (set (map :kind ms))))
    (testing "an instance method"
      (let [m (first (by #(and (= "length" (:name %)) (= :method (:kind %)))))]
        (is (false? (:static? m)))
        (is (= String (:declaring-class m)))
        (is (= "length" (.getName ^java.lang.reflect.Method (:member m))))))
    (testing "a static method"
      (is (every? :static? (by #(and (= "valueOf" (:name %)) (= :method (:kind %)))))))
    (testing "a static field"
      (is (true? (:static? (first (by #(= "CASE_INSENSITIVE_ORDER" (:name %))))))))
    (testing "constructors are named new"
      (is (seq (by #(= :constructor (:kind %)))))
      (is (every? #(= "new" (:name %)) (by #(= :constructor (:kind %))))))))

(deftest deftype-field-test
  (let [fields (filter #(= :field (:kind %)) (reflect/members Dt))
        field (first (filter #(= "a_b" (:name %)) fields))]
    (is (some? field))
    (is (true? (reflect/deftype-field? field Dt)))
    (is (false? (reflect/deftype-field?
                  (first (filter #(= "CASE_INSENSITIVE_ORDER" (:name %)) (reflect/members String)))
                  String)))
    (is (false? (reflect/deftype-field?
                  (first (filter #(= :method (:kind %)) (reflect/members Dt)))
                  Dt)))))

(deftest cache-key-test
  (is (= (reflect/cache-key) (reflect/cache-key)))
  (let [before (reflect/cache-key)]
    (try
      (System/setProperty "fake.class.path" "/nrepl/reflect-test/nowhere")
      (is (not= before (reflect/cache-key)))
      (finally
        (System/clearProperty "fake.class.path")))
    (is (= before (reflect/cache-key)))))

(deftest classpath-files-test
  (let [files (reflect/classpath-files)]
    (is (some #{"clojure/core.clj"} files))
    (is (some #{"nrepl/util/reflect.cljc"} files))
    (is (some #{"java/lang/String.class"} files))
    (is (= (count files) (count (set files))))))

(deftest all-keywords-test
  (let [k (keyword "nrepl.util.reflect-test" "interned-for-test")]
    (is (some #{k} (reflect/all-keywords)))
    (is (every? keyword? (take 100 (reflect/all-keywords))))))

(deftest resource-path-test
  (is (re-find #"clojure/core\.clj$" (reflect/resource-path "clojure/core.clj")))
  (is (nil? (reflect/resource-path "no/such/resource.clj"))))
