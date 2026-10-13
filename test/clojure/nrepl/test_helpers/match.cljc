(ns nrepl.test-helpers.match
  (:require [clojure.string :as str]
            #?(:cljr [clojure.test :as t])))

(defn- regex? [x]
  #?(:clj (instance? java.util.regex.Pattern x)
     :cljr (instance? System.Text.RegularExpressions.Regex x)))

(defn- matcher? [x]
  (and (map? x) (contains? x ::kind)))

(def absent {::kind :absent})

(defn embeds [expected] {::kind :embeds, ::expected expected})

(defn equals [expected] {::kind :equals, ::expected expected})

(defn via [f expected] {::kind :via, ::fn f, ::expected expected})

(defn all-of [& matchers] {::kind :all-of, ::expected (vec matchers)})

(defn mismatch [expected] {::kind :mismatch, ::expected expected})

(defn- absent? [x]
  (and (matcher? x) (= :absent (::kind x))))

(defn- miss [path expected actual why]
  [{:path path, :expected expected, :actual actual, :why why}])

(declare check)

(defn- check-equal [path expected actual]
  (if (= expected actual)
    []
    (miss path expected actual "values differ")))

(defn- check-regex [path expected actual]
  (if (and (string? actual) (re-find expected actual))
    []
    (miss path expected actual "regex does not match")))

(defn- check-pred [path expected actual]
  (if (expected actual)
    []
    (miss path expected actual "predicate returned a falsey value")))

(defn- check-map [path expected actual exact?]
  (if-not (map? actual)
    (miss path expected actual "expected a map")
    (vec (concat
          (mapcat (fn [[k e]]
                    (cond
                      (absent? e) (when (contains? actual k)
                                    (miss (conj path k) 'absent (get actual k) "key must be absent"))
                      (contains? actual k) (check (conj path k) e (get actual k))
                      :else (miss (conj path k) e nil "key is missing")))
                  expected)
          (when exact?
            (mapcat (fn [[k v]]
                      (when-not (contains? expected k)
                        (miss (conj path k) nil v "unexpected key")))
                    actual))))))

(defn- check-seq [path expected actual]
  (if-not (sequential? actual)
    (miss path expected actual "expected a sequential collection")
    (let [e (vec expected)
          a (vec actual)]
      (vec (mapcat (fn [i]
                     (cond
                       (>= i (count a)) (miss (conj path i) (e i) nil "element is missing")
                       (>= i (count e)) (miss (conj path i) nil (a i) "unexpected element")
                       :else (check (conj path i) (e i) (a i))))
                   (range (max (count e) (count a))))))))

(defn- assign [n m ok?]
  (letfn [(go [i used]
            (if (= i n)
              []
              (some (fn [j]
                      (when (and (not (contains? used j)) (ok? i j))
                        (when-let [tail (go (inc i) (conj used j))]
                          (into [j] tail))))
                    (range m))))]
    (go 0 #{})))

(defn- check-any-order [path expected actual exact?]
  (let [e (vec expected)
        a (vec actual)
        ok? (memoize (fn [i j] (empty? (check path (e i) (a j)))))
        sized? (or (not exact?) (= (count e) (count a)))]
    (if (and sized? (assign (count e) (count a) ok?))
      []
      (let [unmatched (remove (fn [i] (some #(ok? i %) (range (count a))))
                              (range (count e)))
            found (concat
                   (when-not sized?
                     (miss path expected actual
                           (str "expected " (count e) " elements, got " (count a))))
                   (map (fn [i] (first (miss path (e i) actual "no element matches")))
                        unmatched))]
        (vec (if (seq found)
               found
               (miss path expected actual "no one-to-one pairing of expected and actual elements")))))))

(defn- check-set [path expected actual exact?]
  (if-not (set? actual)
    (miss path expected actual "expected a set")
    (check-any-order path expected actual exact?)))

(defn- check-embeds-seq [path expected actual]
  (if-not (sequential? actual)
    (miss path expected actual "expected a sequential collection")
    (check-any-order path expected actual false)))

(defn- check-matcher [path m actual]
  (let [e (::expected m)]
    (case (::kind m)
      :absent (miss path 'absent actual "absent is only valid as a map value")
      :embeds (cond
                (map? e) (check-map path e actual false)
                (set? e) (check-set path e actual false)
                (sequential? e) (check-embeds-seq path e actual)
                :else (miss path m actual "embeds takes a map, a set or a sequence"))
      :equals (cond
                (map? e) (check-map path e actual true)
                (set? e) (check-set path e actual true)
                (sequential? e) (check-seq path e actual)
                :else (check-equal path e actual))
      :via (let [t (try {:value ((::fn m) actual)}
                        (catch Exception ex {:error ex}))]
             (if (contains? t :error)
               (miss path e actual (str "transform threw " (:error t)))
               (check path e (:value t))))
      :all-of (vec (mapcat #(check path % actual) e))
      :mismatch (if (empty? (check path e actual))
                  (miss path m actual "matched, but was expected not to")
                  []))))

(defn- check [path expected actual]
  (cond
    (matcher? expected) (check-matcher path expected actual)
    (map? expected) (check-map path expected actual false)
    (set? expected) (check-set path expected actual true)
    (sequential? expected) (check-seq path expected actual)
    (regex? expected) (check-regex path expected actual)
    (fn? expected) (check-pred path expected actual)
    :else (check-equal path expected actual)))

(defn mismatches
  "Returns a vector of {:path :expected :actual :why} maps, one for every place
  where ACTUAL does not match EXPECTED; empty when it matches."
  [expected actual]
  (check [] expected actual))

(defn match?
  [expected actual]
  (empty? (mismatches expected actual)))

(defn- render [x]
  (cond
    (absent? x) 'absent
    (matcher? x) (cons (symbol (name (::kind x)))
                       (case (::kind x)
                         :all-of (::expected x)
                         :via [(::fn x) (::expected x)]
                         [(::expected x)]))
    :else x))

(defn- show [x]
  (let [s (pr-str (render x))]
    (if (> (count s) 300)
      (str (subs s 0 300) "...")
      s)))

(defn describe
  "Returns the MISMATCHES of `mismatches` as a multi-line string naming the path
  of each failure."
  [mismatches]
  (str/join "\n"
            (map (fn [{:keys [path expected actual why]}]
                   (str "at " (if (seq path) (pr-str path) "<root>") ": " why
                        "\n  expected: " (show expected)
                        "\n  actual:   " (show actual)))
                 mismatches)))

#?(:cljr
   (defmethod t/assert-expr 'match? [msg form]
     (if (= 3 (count form))
       `(let [expected# ~(second form)
              actual# ~(nth form 2)
              ms# (mismatches expected# actual#)
              ok# (empty? ms#)]
          (t/do-report (if ok#
                         {:type :pass, :message ~msg, :expected '~form,
                          :actual (list '~'match? expected# actual#)}
                         {:type :fail, :message ~msg, :expected '~form,
                          :actual (symbol (describe ms#))}))
          ok#)
       `(do (t/do-report {:type :fail, :message ~msg,
                          :expected (symbol "match? expects 2 arguments: expected and actual"),
                          :actual '~form})
            false))))
