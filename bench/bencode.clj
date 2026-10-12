(ns bencode-bench
  (:require
   [criterium.core :as crit]
   [nrepl.bencode :as bencode])
  (:import
   (java.io ByteArrayInputStream ByteArrayOutputStream PushbackInputStream)))

(defn- build-message []
  (into {}
        (map (fn [n]
               [(str "key-" n)
                (case (int (mod n 4))
                  0 (apply str (repeat 300 (char (+ 97 (mod n 26)))))
                  1 (vec (range n (+ n 60)))
                  2 {"id" n "name" (str "entry " n) "tags" ["a" "b" "c"]}
                  3 (.getBytes (apply str (repeat 200 "é")) "UTF-8"))]))
        (range 3600)))

(def message (build-message))

(defn- encode ^bytes [m]
  (let [out (ByteArrayOutputStream.)]
    (bencode/write-bencode out m)
    (.toByteArray out)))

(defn round-trip [m]
  (bencode/read-bencode (PushbackInputStream. (ByteArrayInputStream. (encode m)))))

(println "encoded bytes:" (alength (encode message)))

(crit/report-result (crit/benchmark (round-trip message) {}))
