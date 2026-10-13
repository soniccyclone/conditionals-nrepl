(ns nrepl.test-helpers.match-test
  (:require [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing]]
            [nrepl.test-helpers.match :as m]
            #?(:cljr [matcher-combinators.matchers :as mc])))

(defn- paths [expected actual]
  (mapv :path (m/mismatches expected actual)))

(defn- whys [expected actual]
  (mapv :why (m/mismatches expected actual)))

(deftest nested-submaps
  (is (m/match? {:a {:b 1}} {:a {:b 1, :c 2}, :d 3}))
  (is (m/match? {} {:a 1}))
  (is (not (m/match? {:a {:b 1}} {:a {:b 2}})))
  (is (= [[:a :b]] (paths {:a {:b 1}} {:a {:b 2}})))
  (is (= [[:a]] (paths {:a 1} {})))
  (is (= ["key is missing"] (whys {:a 1} {})))
  (is (= [[]] (paths {:a 1} [1])))
  (is (= [[:a] [:b]] (paths {:a 1, :b 2} {:a 3, :b 4}))))

(deftest exact-sequences
  (is (m/match? [1 2] [1 2]))
  (is (m/match? [1 2] '(1 2)))
  (is (m/match? [1 2] (map identity [1 2])))
  (is (m/match? [{:a 1}] [{:a 1, :b 2}]))
  (is (m/match? [] []))
  (is (= [[2]] (paths [1 2] [1 2 3])))
  (is (= ["unexpected element"] (whys [1 2] [1 2 3])))
  (is (= [[2]] (paths [1 2 3] [1 2])))
  (is (= ["element is missing"] (whys [1 2 3] [1 2])))
  (is (= [[0] [1]] (paths [1 2] [2 1])))
  (is (= [[0 :a]] (paths [{:a 1}] [{:a 2}])))
  (is (not (m/match? [1 2] #{1 2})))
  (is (not (m/match? [] nil))))

(deftest set-equality
  (is (m/match? #{:a :b} #{:b :a}))
  (is (m/match? #{#"a.c"} #{"abc"}))
  (is (not (m/match? #{:a :b} #{:a})))
  (is (not (m/match? #{:a} #{:a :b})))
  (is (not (m/match? #{:a :b} [:a :b])))
  (is (= [[]] (paths #{:a :b} #{:a :c})))
  (is (= ["no element matches"] (whys #{:a :b} #{:a :c})))
  (is (= ["expected 1 elements, got 2"] (take 1 (whys #{:a} #{:a :b})))))

(deftest regex-values
  (is (m/match? {:x #"ab+"} {:x "xabbc"}))
  (is (not (m/match? {:x #"ab+"} {:x "zzz"})))
  (is (= [[:x]] (paths {:x #"ab+"} {:x "zzz"})))
  (is (not (m/match? {:x #"1"} {:x 1})))
  (is (= ["regex does not match"] (whys {:x #"1"} {:x 1}))))

(deftest scalar-equality
  (is (m/match? 1 1))
  (is (m/match? "a" "a"))
  (is (m/match? :k :k))
  (is (m/match? nil nil))
  (is (m/match? {:a nil} {:a nil}))
  (is (not (m/match? 1 2)))
  (is (not (m/match? nil 1)))
  (is (not (m/match? {:a nil} {})))
  (is (= ["values differ"] (whys 1 2))))

(deftest predicate-fns
  (is (m/match? {:n even?} {:n 2}))
  (is (m/match? nil? nil))
  (is (not (m/match? {:n even?} {:n 3})))
  (is (= [[:n]] (paths {:n even?} {:n 3})))
  (is (not (m/match? {:n pos?} {})))
  (is (= ["predicate returned a falsey value"] (whys even? 3))))

(deftest absent-keys
  (is (m/match? {:a m/absent} {}))
  (is (m/match? {:a m/absent, :b 1} {:b 1}))
  (is (not (m/match? {:a m/absent} {:a 1})))
  (is (not (m/match? {:a m/absent} {:a nil})))
  (is (= [[:a]] (paths {:a m/absent} {:a 1})))
  (is (= ["key must be absent"] (whys {:a m/absent} {:a 1})))
  (is (not (m/match? m/absent 1))))

(deftest embeds-sequences
  (is (m/match? (m/embeds [2 1]) [1 2 3]))
  (is (m/match? (m/embeds []) [1]))
  (is (not (m/match? (m/embeds [1 4]) [1 2 3])))
  (is (= [4] (map :expected (m/mismatches (m/embeds [1 4]) [1 2 3]))))
  (is (not (m/match? (m/embeds [1 1]) [1 2])))
  (is (m/match? (m/embeds [1 1]) [1 1 2]))
  (is (m/match? (m/embeds [odd? 1]) [1 3]))
  (is (not (m/match? (m/embeds [odd? 1]) [1 2])))
  (is (m/match? (m/embeds [{:candidate "map"}])
                [{:candidate "map", :ns "clojure.core"} {:candidate "filter"}]))
  (is (not (m/match? (m/embeds [{:candidate "nope"}]) [{:candidate "map"}])))
  (is (= [[:status]] (paths {:status (m/embeds [:done])} {:status [:ok]})))
  (is (not (m/match? (m/embeds [1]) #{1})))
  (is (not (m/match? (m/embeds [1]) "x"))))

(deftest embeds-sets-and-maps
  (is (m/match? (m/embeds #{:a}) #{:a :b}))
  (is (not (m/match? (m/embeds #{:z}) #{:a :b})))
  (is (= [[:status]] (paths {:status (m/embeds #{:interrupted})} {:status #{:done}})))
  (is (not (m/match? (m/embeds #{:a}) [:a])))
  (is (m/match? (m/embeds {:a 1}) {:a 1, :b 2}))
  (is (not (m/match? (m/embeds {:a 1}) {:a 2})))
  (is (not (m/match? (m/embeds 1) 1))))

(deftest equals-matcher
  (is (m/match? (m/equals {}) {}))
  (is (not (m/match? (m/equals {}) {:a 1})))
  (is (= [[:a]] (paths (m/equals {}) {:a 1})))
  (is (= ["unexpected key"] (whys (m/equals {}) {:a 1})))
  (is (m/match? (m/equals {:a 1}) {:a 1}))
  (is (m/match? (m/equals {:a {:b 1}}) {:a {:b 1, :c 2}}))
  (is (m/match? (m/equals [1 2]) [1 2]))
  (is (not (m/match? (m/equals [1 2]) [1 2 3])))
  (is (m/match? (m/equals #{1}) #{1}))
  (is (not (m/match? (m/equals #{1}) #{1 2})))
  (is (m/match? (m/equals 5) 5))
  (is (not (m/match? (m/equals 5) 6))))

(deftest via-matcher
  (is (m/match? (m/via #(map keyword %) [:need-input]) ["need-input"]))
  (is (m/match? {:status (m/via #(map keyword %) [:need-input])} {:status ["need-input"]}))
  (is (not (m/match? (m/via #(map keyword %) [:need-input]) ["other"])))
  (is (= [[0]] (paths (m/via #(map keyword %) [:need-input]) ["other"])))
  (is (not (m/match? (m/via (fn [_] (throw (ex-info "boom" {}))) 1) 1)))
  (is (str/starts-with? (first (whys (m/via (fn [_] (throw (ex-info "boom" {}))) 1) 1))
                        "transform threw")))

(deftest all-of-matcher
  (is (m/match? (m/all-of #"a" #"b") "ab"))
  (is (m/match? (m/all-of {:a 1} {:b 2}) {:a 1, :b 2}))
  (is (m/match? (m/all-of) 1))
  (is (not (m/match? (m/all-of #"a" #"b") "a")))
  (is (= ["b"] (map (comp str :expected) (m/mismatches (m/all-of #"a" #"b") "a"))))
  (is (= 2 (count (m/mismatches (m/all-of #"x" #"y") "z")))))

(deftest mismatch-matcher
  (is (m/match? (m/mismatch #"x") "abc"))
  (is (not (m/match? (m/mismatch #"x") "xyz")))
  (is (= ["matched, but was expected not to"] (whys (m/mismatch #"x") "xyz")))
  (is (m/match? (m/all-of #"a\)" (m/mismatch #"b\]") (m/mismatch #"c\}")) "a)"))
  (is (not (m/match? (m/all-of #"a\)" (m/mismatch #"b\]") (m/mismatch #"c\}")) "a) b]")))
  (is (m/match? (m/mismatch (m/embeds [4])) [1 2 3])))

(deftest mismatch-report
  (let [report (m/describe (m/mismatches {:a {:b [1 2]}} {:a {:b [1 3]}}))]
    (is (str/includes? report "at [:a :b 1]: values differ"))
    (is (str/includes? report "expected: 2"))
    (is (str/includes? report "actual:   3")))
  (is (= "" (m/describe (m/mismatches {:a 1} {:a 1}))))
  (is (str/includes? (m/describe (m/mismatches 1 2)) "at <root>"))
  (is (str/includes? (m/describe (m/mismatches {:a (m/mismatch #"x")} {:a "xyz"}))
                     "expected: (mismatch #\"x\")"))
  (is (str/includes? (m/describe (m/mismatches {:a m/absent} {:a 1}))
                     "expected: absent"))
  (let [report (m/describe (m/mismatches 1 (vec (range 1000))))]
    (is (< (count report) 400))
    (is (str/includes? report "..."))))

#?(:cljr
   (deftest assert-expr-reports
     (let [reports (atom [])]
       (binding [t/report #(swap! reports conj %)]
         (is (match? {:a 1} {:a 1, :b 2}))
         (is (match? {:a {:b 1}} {:a {:b 2}}))
         (is (match? 1 2 3)))
       (let [[pass fail arity] @reports]
         (is (= [:pass :fail :fail] (map :type @reports)))
         (is (str/includes? (str (:actual fail)) "at [:a :b]: values differ"))
         (is (= '(match? {:a {:b 1}} {:a {:b 2}}) (:expected fail)))
         (is (str/includes? (str (:expected arity)) "2 arguments"))
         (is (= 'match? (first (:actual pass))))))))

#?(:cljr
   (deftest matcher-combinators-stub
     (is (identical? m/absent mc/absent))
     (is (m/match? (mc/embeds [2 1]) [1 2 3]))
     (is (not (m/match? (mc/embeds [4]) [1 2 3])))
     (is (m/match? (mc/equals {}) {}))
     (is (not (m/match? (mc/equals {}) {:a 1})))
     (is (m/match? (mc/via inc 2) 1))
     (is (not (m/match? (mc/via inc 2) 2)))
     (is (m/match? (mc/all-of #"a" #"b") "ab"))
     (is (not (m/match? (mc/all-of #"a" #"b") "a")))
     (is (m/match? (mc/mismatch 1) 2))
     (is (not (m/match? (mc/mismatch 1) 1)))))
