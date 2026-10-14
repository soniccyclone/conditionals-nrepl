;; Copyright (c) Meikel Brandmeyer. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.


(ns nrepl.bencode-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as tc.prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [nrepl.bencode :as bencode :refer [read-bencode
                                               read-netstring
                                               read-nrepl-message
                                               write-bencode
                                               write-netstring]]
            [nrepl.test-helpers.bounded :refer [await-result sleep-ms spawn]]
            [nrepl.util.io :as io])
  #?(:clj (:import [java.io ByteArrayInputStream PipedInputStream PipedOutputStream])
     :cljr (:import [System.IO MemoryStream]
                    [System.IO.Pipes AnonymousPipeClientStream AnonymousPipeServerStream
                     PipeDirection]
                    [System.Text Encoding])))

(defn #^{:private true} >bytes
  [#^String input]
  (io/utf8-bytes input))

(defn- input-stream
  [bytes]
  #?(:clj (ByteArrayInputStream. bytes)
     :cljr (MemoryStream. ^bytes bytes)))

(defn- byte-array-of
  [xs]
  (byte-array (map #?(:clj unchecked-byte :cljr #(bit-and % 0xFF)) xs)))

(def ^:private eof-exception
  #?(:clj java.io.EOFException :cljr System.IO.EndOfStreamException))

(def ^:private io-exception
  #?(:clj java.io.IOException :cljr System.IO.IOException))

(defmulti #^{:private true} <bytes class)

(defmethod <bytes :default
  [input]
  input)

(defmethod <bytes io/byte-array-class
  [input]
  (io/utf8-string input))

(defmethod <bytes clojure.lang.IPersistentVector
  [input]
  (vec (map <bytes input)))

(defmethod <bytes clojure.lang.IPersistentMap
  [input]
  (->> input
       (map (fn [[k v]] [k (<bytes v)]))
       (into {})))

(defn- decode
  [bytes & {:keys [reader]}]
  (-> bytes
      input-stream
      io/pushback-input-stream
      reader))

(defn- >input
  [^String input & args]
  (-> input
      >bytes
      (#(apply decode % args))
      <bytes))

(deftest test-netstring-reading
  (are [x y] (= (>input x :reader read-netstring) y)
    "0:,"                ""
    "13:Hello, World!,"  "Hello, World!"
    "16:Hällö, Würld!,"  "Hällö, Würld!"
    "25:Здравей, Свят!," "Здравей, Свят!"))

(deftest test-string-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "0:"                ""
    "13:Hello, World!"  "Hello, World!"
    "16:Hällö, Würld!"  "Hällö, Würld!"
    "25:Здравей, Свят!" "Здравей, Свят!"))

(deftest test-integer-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "i0e"     0
    "i42e"   42
    "i-42e" -42))

(deftest test-list-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "le"                    []
    "l6:cheesee"            ["cheese"]
    "l6:cheese3:ham4:eggse" ["cheese" "ham" "eggs"]))

(deftest test-map-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "de"            {}
    "d3:ham4:eggse" {"ham" "eggs"}))

(deftest test-nested-reading
  (are [x y] (= (>input x :reader read-bencode) y)
    "l6:cheesei42ed3:ham4:eggsee" ["cheese" 42 {"ham" "eggs"}]
    "d6:cheesei42e3:haml4:eggsee" {"cheese" 42 "ham" ["eggs"]}))

(defn- >stream
  [thing & {:keys [writer]}]
  (doto (io/byte-sink)
    (writer thing)))

(defn- >output
  [& args]
  (-> >stream
      (apply args)
      io/sink->bytes
      io/utf8-string))

(deftest test-netstring-writing
  (are [x y] (= (>output (>bytes x) :writer write-netstring) y)
    ""               "0:,"
    "Hello, World!"  "13:Hello, World!,"
    "Hällö, Würld!"  "16:Hällö, Würld!,"
    "Здравей, Свят!" "25:Здравей, Свят!,"))

(deftest test-byte-array-writing
  (are [x y] (= (>output (>bytes x) :writer write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "16:Hällö, Würld!"
    "Здравей, Свят!" "25:Здравей, Свят!"))

(deftest test-string-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "16:Hällö, Würld!"
    "Здравей, Свят!" "25:Здравей, Свят!"))

(deftest test-input-stream-writing
  (are [x y] (= (>output (input-stream (>bytes x))
                         :writer write-bencode) y)
    ""               "0:"
    "Hello, World!"  "13:Hello, World!"
    "Hällö, Würld!"  "16:Hällö, Würld!"
    "Здравей, Свят!" "25:Здравей, Свят!"))

(deftest test-integer-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    0 "i0e"
    42 "i42e"
    -42 "i-42e"

    ;; Works for all integral types.
    ;; Note: BigInts (42N) not tested, since they are not
    ;; supported in 1.2.
    #?(:clj (Byte/parseByte "42" 10) :cljr (byte 42))   "i42e"
    #?(:clj (Short/parseShort "42" 10) :cljr (short 42)) "i42e"
    #?(:clj (Integer/parseInt "42" 10) :cljr (int 42))   "i42e"
    #?(:clj (Long/parseLong "42" 10) :cljr (long 42))    "i42e"
    #?@(:cljr [42N "i42e"
               (sbyte 42) "i42e"
               (ushort 42) "i42e"
               (uint 42) "i42e"
               (ulong 42) "i42e"])))

(deftest test-named-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    :foo      "3:foo"
    :foo/bar  "7:foo/bar"
    'foo      "3:foo"
    'foo/bar  "7:foo/bar"))

(deftest test-list-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    nil                     "le"
    []                      "le"
    ["cheese"]              "l6:cheesee"
    ["cheese" "ham" "eggs"] "l6:cheese3:ham4:eggse"))

(deftest test-map-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    {}             "de"
    {"ham" "eggs"} "d3:ham4:eggse"
    {:ham "eggs"}  "d3:ham4:eggse"
    {'ham "eggs"}  "d3:ham4:eggse"
    {:h/am "eggs"} "d4:h/am4:eggse"))

(deftest test-nested-writing
  (are [x y] (= (>output x :writer write-bencode) y)
    ["cheese" 42 {"ham" "eggs"}] "l6:cheesei42ed3:ham4:eggsee"
    {"cheese" 42 "ham" ["eggs"]} "d6:cheesei42e3:haml4:eggsee"))

(deftest test-lexicographic-sorting
  (let [source (zipmap ["ham" "eggs" "hamburger" "hamburg" "cheese"] (range))]
    (is (= "d6:cheesei4e4:eggsi1e3:hami0e7:hamburgi3e9:hamburgeri2ee"
           (>output source :writer write-bencode)))))

(deftest unencoded-values
  ;; just some PNG data that won't round-trip cleanly through UTF-8 encoding, so
  ;; any default encoding in the bencode implementation will be caught immediately
  (let [binary-data (byte-array-of
                     [-119 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0
                      100 0 0 0 100 8 6 0 0 0 112 -30 -107 84 0 0 3 -16 105
                      67 67 80 73 67 67 32 80 114 111 102 105 108 101 0 0 40
                      -111 -115 85 -35 111 -37 84 20 63 -119 111 92 -92 22 63
                      -96 -79 -114 14 21 -117 -81 85 83 91 -71 27 26 -83 -58 6
                      73 -109 -91 -23 66 26 -71 -51 -40 42 -92 -55 117 110])]
    (is (= (seq binary-data)
           (-> {"data" binary-data}
               (>stream :writer write-bencode)
               io/sink->bytes
               (decode :reader read-bencode)
               (get "data")
               seq)))))

(deftest unwritable-values
  (testing "write-bencode writes eagerly"
    (let [out (io/byte-sink)]
      (is (thrown? #?(:clj IllegalArgumentException :cljr ArgumentException)
                   (write-bencode out {"obj" (Object.)})))
      (is (= "d3:obj" (io/utf8-string (io/sink->bytes out)))))))

;; ## Reading failures and partial input.

(defn- thrown-class
  [f]
  (try (f) nil (catch Exception e (class e))))

(deftest truncated-input-is-an-end-of-file
  (doseq [input ["" "5:abc" "i42" "l" "d3:ham" "d3:ham4:eggs" "3" "-" "5:"]]
    (is (= eof-exception (thrown-class #(>input input :reader read-bencode)))
        (pr-str input)))
  (testing "a netstring without its comma"
    (is (= eof-exception (thrown-class #(>input "1:a" :reader read-netstring)))))
  (testing "messages"
    (is (= "Invalid netstring. Unexpected end of input."
           (ex-message (try (>input "" :reader read-bencode) (catch Exception e e)))))
    (is (= "Invalid netstring. Less data available than expected."
           (ex-message (try (>input "5:abc" :reader read-bencode) (catch Exception e e)))))
    (is (= "Invalid bencode map. Unexpected end of input."
           (ex-message (try (>input "d3:hame" :reader read-bencode) (catch Exception e e)))))))

(deftest netstring-needs-its-comma
  (is (= io-exception (thrown-class #(>input "1:ax" :reader read-netstring))))
  (is (= "Invalid netstring. ',' expected."
         (ex-message (try (>input "1:ax" :reader read-netstring) (catch Exception e e))))))

(deftest nrepl-messages-are-maps
  (is (= {"op" "eval"} (<bytes (>input "d2:op4:evale" :reader read-nrepl-message))))
  (let [e (try (>input "i1e" :reader read-nrepl-message) (catch Exception e e))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (re-find #"nREPL message must be a map" (ex-message e)))
    (is (re-find #"Wrong first byte: 105 \(must be 100\)" (ex-message e)))))

(defn- pipe
  []
  #?(:clj (let [in (PipedInputStream.)]
            {:in in :out (PipedOutputStream. in)})
     :cljr (let [server (AnonymousPipeServerStream. PipeDirection/Out)]
             {:in (AnonymousPipeClientStream. PipeDirection/In (.ClientSafePipeHandle server))
              :out server})))

(defn- write-piece
  [out ^String s]
  (let [^bytes b (>bytes s)]
    #?(:clj (doto ^java.io.OutputStream out (.write b 0 (alength b)) .flush)
       :cljr (doto ^System.IO.Stream out (.Write b (int 0) (alength b)) .Flush))))

(deftest data-that-arrives-in-pieces-is-reassembled
  (let [{:keys [in out]} (pipe)
        reader (spawn #(<bytes (read-bencode (io/pushback-input-stream in))))]
    (doseq [piece ["d3:ham1" "0:eg" "gs" "plant!" "e"]]
      (write-piece out piece)
      (sleep-ms 100))
    (is (= {"ham" "eggsplant!"} (await-result reader)))
    (io/close! out)))

;; ## Generative testing.
;;
;; Verify that any valid value remains the same after encode-decode roundtrip.

(def valid-bencode-input-generator
  "This is a recursive test.check generator that can generate a string, a number,
  or a list or a map that contains any other valid value."
  (gen/recursive-gen #(gen/one-of [%
                                   (gen/vector % 0 20)
                                   (gen/map gen/string % {:min-elements 0
                                                          :max-elements 20})])
                     (gen/one-of [gen/string gen/large-integer])))

;; :num-tests is intentionally so small to avoid crashing with OOM on CI. When
;; making any changes to `nrepl.bencode`, make sure to increase this number and
;; extensively run this locally.

(defspec generative-roundtrip-test {:num-tests 30}
  (tc.prop/for-all
   [value valid-bencode-input-generator]
   (= value (>input (>output value :writer write-bencode) :reader read-bencode))))

(defn- encoded-latin1
  [write-fn value]
  (let [out (io/byte-sink)]
    (write-fn out value)
    (let [b (io/sink->bytes out)]
      #?(:clj (String. ^bytes b "ISO-8859-1")
         :cljr (.GetString Encoding/Latin1 ^bytes b)))))

(deftest write-path-exact-bytes-test
  (testing "netstring: length digits, colon, content, trailing comma"
    (is (= "2:ab," (encoded-latin1 write-netstring (>bytes "ab")))))
  (testing "every write site of a bencode value, with sorted map keys"
    (is (= (str "d"
                "5:a-sym" "6:ns/sym"
                "5:array" "l1:xi2ee"
                "5:bytes" "3:\u0001\u0002\u0003"
                "5:empty" "l0:0:e"
                "4:ints" "li2ei3ei4ei5ei6ei9223372036854775807ee"
                "4:list" "li1ei-42ei0e2:\u00c3\u00a92:kw3:symlee"
                "3:map" "d6:nesteddee"
                "5:z/key" "6:stream"
                "e")
           (encoded-latin1 write-bencode
                           {:z/key (input-stream (>bytes "stream"))
                            'a-sym 'ns/sym
                            "list" [1 -42 0 "\u00e9" :kw 'sym nil]
                            "bytes" (byte-array [1 2 3])
                            "array" (object-array ["x" 2])
                            "map" {"nested" {}}
                            "ints" [(int 2) (short 3) (byte 4) 5N
                                    #?(:clj (biginteger 6) :cljr 6N)
                                    #?(:clj Long/MAX_VALUE :cljr Int64/MaxValue)]
                            "empty" ["" (byte-array 0)]})))))

;; ## Performance testing
;;
;; This code is to be run manually, so it is in a comment block. Evaluate the
;; performance of bencode encoding and decoding when making changes to bencode
;; implementation, and compare it to the previous version.

(comment
  (def data (gen/generate valid-bencode-input-generator 5000 4))
  (count (str data)) ;; Sanity-check data size.

  ;; Writing benchmark
  (time+ (write-bencode (ByteArrayOutputStream.) data))

  ;; Reading benchmark
  (let [baos (doto (ByteArrayOutputStream.)
               (write-bencode data))
        arr (.toByteArray baos)]
    (time+
     (nrepl.bencode/read-bencode (PushbackInputStream. (java.io.ByteArrayInputStream. arr)))
     nil)))
