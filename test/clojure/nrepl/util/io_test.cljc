(ns nrepl.util.io-test
  (:require
   [clojure.test :refer [deftest is]]
   [nrepl.util.io :as io]))

(defn- high-surrogate?
  [c]
  (<= 0xD800 (int c) 0xDBFF))

(deftest callback-writer-surrogate-test
  (let [supplementary (str (char 0xD834) (char 0xDD1E))]
    (doseq [buffer-size (range 10 21)
            prefix-len (range 1 (inc buffer-size))]
      (let [chunks (atom [])
            writer (io/replying-writer #(swap! chunks conj %) buffer-size nil)
            prefix (apply str (repeat prefix-len \-))]
        (io/write-string! writer prefix)
        (io/write-string! writer supplementary)
        (io/flush! writer)
        (is (not-any? #(high-surrogate? (last %)) @chunks)
            (pr-str buffer-size prefix-len @chunks))
        (is (= (str prefix supplementary) (apply str @chunks))
            (pr-str buffer-size prefix-len @chunks))))))
