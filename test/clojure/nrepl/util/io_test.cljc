(ns nrepl.util.io-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [nrepl.test-helpers.bounded :refer [await-result bounded sleep-ms spawn wait-until]]
   [nrepl.util.io :as io])
  #?(:clj (:import
           (java.io ByteArrayInputStream ByteArrayOutputStream PrintStream))
     :cljr (:import
            (System.IO MemoryStream StringWriter)
            (System.Net IPAddress IPEndPoint)
            (System.Net.Sockets TcpClient TcpListener))))

#?(:cljr (import '(nrepl CallbackBufferedWriter QuotaBoundWriter TeeWriter)))

(def ^:private newline-str
  #?(:clj (System/lineSeparator)
     :cljr Environment/NewLine))

(defn- unsigned [bs]
  (mapv #(bit-and (long %) 0xFF) bs))

(defn- ->byte-array [xs]
  (byte-array (map #?(:clj unchecked-byte :cljr #(bit-and (long %) 0xFF)) xs)))

(defn- bytes-in [xs]
  #?(:clj (ByteArrayInputStream. (->byte-array xs))
     :cljr (MemoryStream. ^bytes (->byte-array xs))))

(defn- sink-with [xs]
  (let [sink (io/byte-sink)]
    (io/copy! (bytes-in xs) sink)
    sink))

(defn- rd [^#?(:clj java.io.Reader :cljr System.IO.TextReader) r]
  (#?(:clj .read :cljr .Read) r))

(defn- rd-bulk [^#?(:clj java.io.Reader :cljr System.IO.TextReader) r n]
  (let [buf (char-array n)
        k (#?(:clj .read :cljr .Read) r buf (int 0) (int n))]
    [k (apply str (take (max k 0) buf))]))

(defn- unread [^#?(:clj java.io.PushbackReader :cljr clojure.lang.PushbackTextReader) r c]
  (#?(:clj .unread :cljr .Unread) r (int c)))

(defn- rd* [r]
  (bounded #(rd r)))

(defn- read-all-chars [r]
  (apply str (map char (take-while #(not= -1 %) (repeatedly #(rd r))))))

(defn- position
  [^#?(:clj clojure.lang.LineNumberingPushbackReader :cljr clojure.lang.LineNumberingTextReader) r]
  #?(:clj [(.getLineNumber r) (.getColumnNumber r)]
     :cljr [(.LineNumber r) (.ColumnNumber r)]))

(defn- collector []
  (let [chunks (atom [])]
    [chunks #(swap! chunks conj %)]))

(defn- high-surrogate?
  [c]
  (<= 0xD800 (int c) 0xDBFF))

(def ^:private clef (str (char 0xD834) (char 0xDD1E)))

(deftest byte-arrays
  (testing "byte-array-class is the class of a byte array"
    (is (= (class (byte-array 3)) io/byte-array-class))
    (is (not= (class (long-array 3)) io/byte-array-class))
    (is (not= String io/byte-array-class)))
  (testing "byte-array? is true only for byte arrays"
    (is (true? (io/byte-array? (byte-array 3))))
    (is (true? (io/byte-array? (byte-array 0))))
    (is (false? (io/byte-array? (long-array 3))))
    (is (false? (io/byte-array? (object-array 3))))
    (is (false? (io/byte-array? "abc")))
    (is (false? (io/byte-array? [1 2 3]))))
  (testing "array? is true for any array and nothing else"
    (is (true? (io/array? (byte-array 3))))
    (is (true? (io/array? (long-array 3))))
    (is (true? (io/array? (object-array 0))))
    (is (false? (io/array? [1 2 3])))
    (is (false? (io/array? "abc")))
    (is (false? (io/array? (Object.))))))

(deftest byte-sink-and-copy
  (testing "an empty sink yields an empty byte array"
    (let [out (io/sink->bytes (io/byte-sink))]
      (is (true? (io/byte-array? out)))
      (is (= [] (unsigned out)))))
  (testing "copy! moves every byte, including values above 127"
    (is (= [0 1 127 128 200 255] (unsigned (io/sink->bytes (sink-with [0 1 127 128 200 255]))))))
  (testing "copy! appends to what the sink already holds"
    (let [sink (sink-with [1 2])]
      (io/copy! (bytes-in [3 4]) sink)
      (is (= [1 2 3 4] (unsigned (io/sink->bytes sink))))))
  (testing "copy! handles more data than one buffer"
    (let [xs (vec (take 100000 (cycle (range 256))))]
      (is (= xs (unsigned (io/sink->bytes (sink-with xs)))))))
  (testing "each sink is independent"
    (let [a (io/byte-sink) b (io/byte-sink)]
      (io/copy! (bytes-in [1]) a)
      (is (= [] (unsigned (io/sink->bytes b)))))))

(deftest utf8-round-trip
  (testing "bytes"
    (is (= [] (unsigned (io/utf8-bytes ""))))
    (is (= [97 98] (unsigned (io/utf8-bytes "ab"))))
    (is (= [0xC3 0xA9] (unsigned (io/utf8-bytes "é"))))
    (is (= [0xE2 0x82 0xAC] (unsigned (io/utf8-bytes "€"))))
    (is (= [0xF0 0x9D 0x84 0x9E] (unsigned (io/utf8-bytes clef)))))
  (testing "a string with a surrogate pair survives the round trip"
    (let [s (str "a" clef "é€" clef "z")
          bs (io/utf8-bytes s)]
      (is (true? (io/byte-array? bs)))
      (is (= 15 (count bs)))
      (is (= s (io/utf8-string bs)))
      (is (= 8 (count (io/utf8-string bs))))))
  (testing "decoding"
    (is (= "" (io/utf8-string (byte-array 0))))
    (is (= clef (io/utf8-string (->byte-array [0xF0 0x9D 0x84 0x9E]))))
    (is (= "hé" (io/utf8-string (->byte-array [0x68 0xC3 0xA9]))))))

(defn- read-byte [^#?(:clj java.io.InputStream :cljr System.IO.Stream) in]
  (#?(:clj .read :cljr .ReadByte) in))

(defn- unread-byte [^#?(:clj java.io.PushbackInputStream :cljr clojure.lang.PushbackInputStream) in b]
  (#?(:clj .unread :cljr .Unread) in (#?(:clj int :cljr byte) b)))

(defn- read-bulk [^#?(:clj java.io.InputStream :cljr System.IO.Stream) in ^bytes buf n]
  (#?(:clj .read :cljr .Read) in buf (int 0) (int n)))

(deftest pushback-input-stream-test
  (testing "an unread byte is returned by the next read, then the stream continues"
    (let [in (io/pushback-input-stream (bytes-in [10 20 30]))]
      (is (= 10 (read-byte in)))
      (unread-byte in 10)
      (is (= 10 (read-byte in)))
      (is (= 20 (read-byte in)))
      (is (= 30 (read-byte in)))
      (is (= -1 (read-byte in)))))
  (testing "a bulk read after an unread starts with the unread byte"
    (let [in (io/pushback-input-stream (bytes-in [10 20 30]))
          buf (byte-array 3)]
      (is (= 10 (read-byte in)))
      (unread-byte in 10)
      (is (= 3 (read-bulk in buf 3)))
      (is (= [10 20 30] (unsigned buf))))))

#?(:cljr
   (defn- socket-pair []
     (let [listener (TcpListener. IPAddress/Loopback (int 0))]
       (.Start listener)
       (let [port (.Port ^IPEndPoint (.LocalEndpoint listener))
             client (TcpClient.)]
         (.Connect client IPAddress/Loopback (int port))
         (let [server (.AcceptTcpClient listener)]
           (.Stop listener)
           {:client client :server server})))))

#?(:cljr
   (defn- write-bytes! [^System.IO.Stream s xs]
     (let [buf (->byte-array xs)]
       (.Write s buf (int 0) (int (alength buf)))
       (.Flush s))))

#?(:cljr
   (deftest pushback-input-stream-over-a-non-seekable-stream
     (let [{:keys [^TcpClient client ^TcpClient server]} (socket-pair)]
       (try
         (write-bytes! (.GetStream server) [65])
         (let [in (io/pushback-input-stream (.GetStream client))]
           (is (= 65 (bounded #(read-byte in))))
           (unread-byte in 65)
           (testing "unread then read does not wait for more data"
             (is (= 65 (bounded #(read-byte in))))))
         (finally
           (.Dispose client)
           (.Dispose server))))))

(deftest text-reader-and-writer
  (testing "a text writer over a byte sink encodes UTF-8 once flushed"
    (let [sink (io/byte-sink)
          w (io/text-writer sink)]
      (io/write-string! w (str "hé" clef "\n"))
      (io/flush! w)
      (is (= (unsigned (io/utf8-bytes (str "hé" clef "\n")))
             (unsigned (io/sink->bytes sink))))
      (io/write-string! w "more")
      (io/close! w)
      (is (= (str "hé" clef "\nmore")
             (io/utf8-string (io/sink->bytes sink))))))
  (testing "a text reader over bytes decodes UTF-8"
    (let [r (io/text-reader (bytes-in (unsigned (io/utf8-bytes (str "hé" clef)))))]
      (is (= (str "hé" clef) (read-all-chars r)))))
  (testing "a text reader does not mistake the bytes for another encoding"
    (let [r (io/text-reader (bytes-in [0xC3 0xA9]))]
      (is (= 0xE9 (rd r)))
      (is (= -1 (rd r))))))

(deftest pushback-reader-test
  (let [r (io/pushback-reader (io/string-reader "xyz"))]
    (is (= (int \x) (rd r)))
    (unread r (int \x))
    (is (= (int \x) (rd r)))
    (is (= (int \y) (rd r)))
    (is (= "z" (read-all-chars r))))
  (testing "predicates"
    (is (true? (io/pushback-reader? (io/pushback-reader (io/string-reader "")))))
    (is (true? (io/pushback-reader? (io/line-numbering-reader (io/string-reader "")))))
    (is (false? (io/pushback-reader? (io/string-reader ""))))
    (is (false? (io/pushback-reader? nil)))
    (is (false? (io/pushback-reader? "x")))))

(deftest line-numbering-reader-test
  (testing "line and column advance with the characters read"
    (let [r (io/line-numbering-reader (io/string-reader "ab\ncd\n"))]
      (is (= [1 1] (position r)))
      (rd r)
      (is (= [1 2] (position r)))
      (rd r)
      (rd r)
      (is (= [2 1] (position r)))
      (rd r)
      (is (= [2 2] (position r)))))
  (testing "it is still a pushback reader and reads like the reader it wraps"
    (let [r (io/line-numbering-reader (io/string-reader "xy"))]
      (is (= (int \x) (rd r)))
      (unread r (int \x))
      (is (= "xy" (read-all-chars r)))))
  (testing "set-line! and set-column! are visible through the reader's position"
    (let [r (io/line-numbering-reader (io/string-reader "ab\ncd"))]
      (io/set-line! r 10)
      (is (= [10 1] (position r)))
      (io/set-column! r (int 5))
      (is (= [10 5] (position r)))
      (rd r)
      (is (= [10 6] (position r)))
      (rd r)
      (rd r)
      (is (= [11 1] (position r)))))
  (testing "set-line! accepts a long"
    (let [r (io/line-numbering-reader (io/string-reader "a"))]
      (io/set-line! r (long 7))
      (io/set-column! r (int 3))
      (is (= [7 3] (position r)))))
  (testing "a form read from it carries the line that was set and a column that follows the one that was set"
    (let [form-meta (fn [column]
                      (meta (read (doto (io/line-numbering-reader (io/string-reader "(x)"))
                                    (io/set-line! 21)
                                    (io/set-column! (int column))))))]
      (is (= 21 (:line (form-meta 9))))
      (is (= 4 (- (:column (form-meta 9)) (:column (form-meta 5)))))))
  (testing "predicates"
    (is (true? (io/line-numbering-reader? (io/line-numbering-reader (io/string-reader "")))))
    (is (false? (io/line-numbering-reader? (io/pushback-reader (io/string-reader "")))))
    (is (false? (io/line-numbering-reader? (io/string-reader ""))))
    (is (false? (io/line-numbering-reader? nil)))))

(deftest reader-ready
  (testing "a reader with unread input is ready"
    (let [r (io/line-numbering-reader (io/string-reader "foo bar"))]
      (is (true? (io/reader-ready? r)))))
  (testing "after a read that pushed its delimiter back the reader is ready, though nothing is queued behind it"
    (let [qpr (io/stdin-reader (fn []))
          r (io/line-numbering-reader qpr)]
      (io/add-input! qpr "foo\n")
      (is (= 'foo (bounded #(read r))))
      (is (true? (bounded #(io/reader-ready? r))))
      (is (= (int \newline) (rd* r)))
      (is (false? (bounded #(io/reader-ready? r))))))
  (testing "an empty stdin reader is not ready and the question does not block or ask for input"
    (let [calls (atom 0)
          qpr (io/stdin-reader #(swap! calls inc))
          r (io/line-numbering-reader qpr)]
      (is (false? (bounded #(io/reader-ready? r))))
      (is (= 0 @calls))
      (io/add-input! qpr "x")
      (is (true? (bounded #(io/reader-ready? r))))
      (is (= (int \x) (rd* r)))
      (is (false? (bounded #(io/reader-ready? r))))
      (is (= 0 @calls))))
  (testing "an eof marker is not input"
    (let [qpr (io/stdin-reader (fn []))
          r (io/line-numbering-reader qpr)]
      (io/add-eof! qpr)
      (is (false? (bounded #(io/reader-ready? r)))))))

(deftest string-reader-and-writer
  (testing "string-reader reads its string"
    (is (= "abc" (read-all-chars (io/string-reader "abc"))))
    (is (= -1 (rd (io/string-reader "")))))
  (testing "string-writer accumulates what is written"
    (let [w (io/string-writer)]
      (is (= "" (io/writer->string w)))
      (io/write-string! w "ab")
      (io/write-string! w (str clef "c"))
      (is (= (str "ab" clef "c") (io/writer->string w)))))
  (testing "writers are independent"
    (let [a (io/string-writer) b (io/string-writer)]
      (io/write-string! a "x")
      (is (= "" (io/writer->string b))))))

(deftest write-flush-close
  (testing "a buffered text writer shows nothing downstream until flush!"
    (let [sink (io/byte-sink)
          w (io/text-writer sink)]
      (io/write-string! w "abc")
      (is (= [] (unsigned (io/sink->bytes sink))))
      (io/flush! w)
      (is (= [97 98 99] (unsigned (io/sink->bytes sink))))))
  (testing "close! flushes what is buffered"
    (let [sink (io/byte-sink)
          w (io/text-writer sink)]
      (io/write-string! w "abc")
      (io/close! w)
      (is (= [97 98 99] (unsigned (io/sink->bytes sink))))))
  (testing "close! hands a replying writer's tail to its callback"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 nil)]
      (io/write-string! w "tail")
      (io/close! w)
      (is (= "tail" (apply str @chunks)))))
  (testing "flush! pushes a buffered byte stream down to what it wraps"
    (let [sink (io/byte-sink)
          buffered #?(:clj (java.io.BufferedOutputStream. sink)
                      :cljr (System.IO.BufferedStream. sink))
          buf (->byte-array [1 2 3])]
      #?(:clj (.write ^java.io.OutputStream buffered ^bytes buf 0 3)
         :cljr (.Write ^System.IO.Stream buffered ^bytes buf (int 0) (int 3)))
      (is (= [] (unsigned (io/sink->bytes sink))))
      (is (nil? (io/flush! buffered)))
      (is (= [1 2 3] (unsigned (io/sink->bytes sink))))))
  (testing "close! on a buffered byte stream also pushes its contents down"
    (let [sink (io/byte-sink)
          buffered #?(:clj (java.io.BufferedOutputStream. sink)
                      :cljr (System.IO.BufferedStream. sink))
          buf (->byte-array [4 5])]
      #?(:clj (.write ^java.io.OutputStream buffered ^bytes buf 0 2)
         :cljr (.Write ^System.IO.Stream buffered ^bytes buf (int 0) (int 2)))
      (io/close! buffered)
      (is (= [4 5] (unsigned (io/sink->bytes sink))))))
  (testing "close! works on a byte stream and a reader"
    (is (nil? (io/close! (io/byte-sink))))
    (is (nil? (io/close! (io/string-reader "x")))))
  (testing "closeable?"
    (is (true? (io/closeable? (io/string-writer))))
    (is (true? (io/closeable? (io/byte-sink))))
    (is (true? (io/closeable? (io/string-reader ""))))
    (is (true? (io/closeable? (bytes-in [1]))))
    (is (false? (io/closeable? "a string")))
    (is (false? (io/closeable? 42)))
    (is (false? (io/closeable? nil)))
    (is (false? (io/closeable? (Object.))))))

(deftest file-name-test
  (is (= "c.clj" (io/file-name "a/b/c.clj")))
  (is (= "c.clj" (io/file-name "c.clj")))
  (is (= "c.clj" (io/file-name "/c.clj")))
  (is (= "c" (io/file-name "a/b/c")))
  (is (= "" (io/file-name ""))))

(deftest stdin-reader-test
  (testing "queued input is read in order across chunks without asking for more"
    (let [calls (atom 0)
          qpr (io/stdin-reader #(swap! calls inc))]
      (io/add-input! qpr "ab")
      (io/add-input! qpr "c")
      (is (= [97 98 99] [(rd* qpr) (rd* qpr) (rd* qpr)]))
      (is (= 0 @calls))))
  (testing "an empty string is not an end of file"
    (let [qpr (io/stdin-reader (fn []))]
      (io/add-input! qpr "")
      (io/add-input! qpr "x")
      (is (= (int \x) (rd* qpr)))))
  (testing "a read on an empty queue asks for input once and waits for it"
    (let [calls (atom 0)
          qpr (io/stdin-reader #(swap! calls inc))
          h (spawn #(rd qpr))]
      (is (wait-until #(= 1 @calls)))
      (sleep-ms 50)
      (is (= 1 @calls))
      (is (not (realized? h)))
      (io/add-input! qpr "zq")
      (is (= (int \z) (await-result h)))
      (is (= (int \q) (rd* qpr)))
      (is (= 1 @calls))))
  (testing "eof ends a read with -1, without asking for input, after the queued text"
    (let [calls (atom 0)
          qpr (io/stdin-reader #(swap! calls inc))]
      (io/add-input! qpr "a")
      (io/add-eof! qpr)
      (is (= [97 -1] [(rd* qpr) (rd* qpr)]))
      (is (= 0 @calls))))
  (testing "eof is consumed by the read that reports it, so the next read asks for input again"
    (let [calls (atom 0)
          qpr (io/stdin-reader #(swap! calls inc))]
      (io/add-eof! qpr)
      (is (= -1 (rd* qpr)))
      (is (= 0 @calls))
      (let [h (spawn #(rd qpr))]
        (is (wait-until #(= 1 @calls)))
        (io/add-input! qpr "k")
        (is (= (int \k) (await-result h))))))
  (testing "a bulk read returns what is queued and signals eof the host's way"
    (let [qpr (io/stdin-reader (fn []))]
      (io/add-input! qpr "ab")
      (io/add-eof! qpr)
      (is (= [2 "ab"] (bounded #(rd-bulk qpr 10))))
      (is (= [#?(:clj -1 :cljr 0) ""] (bounded #(rd-bulk qpr 10))))))
  (testing "a bulk read waits for the first character only"
    (let [calls (atom 0)
          qpr (io/stdin-reader #(swap! calls inc))
          h (spawn #(rd-bulk qpr 10))]
      (is (wait-until #(= 1 @calls)))
      (io/add-input! qpr "xy")
      (is (= [2 "xy"] (await-result h)))
      (is (= 1 @calls))))
  (testing "read-line over a line-numbering reader, across chunks"
    (let [qpr (io/stdin-reader (fn []))
          r (io/line-numbering-reader qpr)]
      (io/add-input! qpr "foo\nba")
      (io/add-input! qpr "r\n")
      (io/add-eof! qpr)
      (is (= "foo" (bounded #(binding [*in* r] (read-line)))))
      (is (= "bar" (bounded #(binding [*in* r] (read-line)))))
      (is (nil? (bounded #(binding [*in* r] (read-line)))))))
  (testing "read of a form split across chunks, then read-line gets the rest of the line"
    (let [qpr (io/stdin-reader (fn []))
          r (io/line-numbering-reader qpr)]
      (io/add-input! qpr "(+ 1 ")
      (io/add-input! qpr "2) tail\n")
      (is (= '(+ 1 2) (bounded #(binding [*in* r] (read)))))
      (is (true? (io/reader-ready? r)))
      (is (= " tail" (bounded #(binding [*in* r] (read-line)))))
      (is (false? (io/reader-ready? r))))))

(defn- chunks-ok? [input chunks]
  (and (= input (apply str chunks))
       (every? seq chunks)
       (not-any? #(high-surrogate? (last %)) chunks)))

(deftest callback-writer-surrogate-test
  (let [supplementary (str (char 0xD834) (char 0xDD1E))]
    (doseq [buffer-size (range 7 21)
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

(deftest replying-writer-surrogates-in-longer-text
  (doseq [size [7 8 9 10 11]
          offset (range 0 12)]
    (let [[chunks cb] (collector)
          w (io/replying-writer cb size nil)
          input (str (apply str (repeat offset "a")) clef clef (apply str (repeat 9 "b")) clef)]
      (io/write-string! w input)
      (io/flush! w)
      (is (chunks-ok? input @chunks) (pr-str {:size size :offset offset :chunks @chunks})))))

(deftest replying-writer-lines-and-chunks
  (testing "lines reach the callback once flushed"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 nil)]
      (io/write-string! w "a\nb\nc")
      (io/flush! w)
      (is (= "a\nb\nc" (apply str @chunks)))
      (is (= "c" (last @chunks)))
      (is (every? #(or (str/ends-with? % "\n") (= % "c")) @chunks))))
  (testing "a partial line is held back until it is completed"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 nil)]
      (io/write-string! w "par")
      (io/flush! w)
      (is (= ["par"] @chunks))
      (io/write-string! w "tial\n")
      (io/flush! w)
      (is (= "partial\n" (apply str @chunks)))))
  (testing "text longer than buffer-size is cut into buffer-size chunks"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 4 nil)]
      (io/write-string! w "abcdefghij")
      (io/flush! w)
      (is (= ["abcd" "efgh" "ij"] @chunks))))
  (testing "each flushed line is its own chunk"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 nil)]
      (io/write-string! w "x\n")
      (io/flush! w)
      (io/write-string! w "y\n")
      (io/flush! w)
      (is (= ["x\n" "y\n"] @chunks))))
  #?(:cljr
     (testing "the clr writer hands lines over without a flush!"
       (let [[chunks cb] (collector)
             w (io/replying-writer cb 1024 nil)]
         (io/write-string! w "a\nb")
         (is (= ["a\n"] @chunks))))
     :clj
     (testing "the jvm writer holds text in its encoder until flush!"
       (let [[chunks cb] (collector)
             w (io/replying-writer cb 1024 nil)]
         (io/write-string! w "a\nb")
         (is (= [] @chunks))
         (io/flush! w)
         (is (= "a\nb" (apply str @chunks))))))
  (testing "it can be bound as *out* and printed to"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 nil)]
      (binding [*out* w]
        (print 1 :a "s" \c 1.5 nil)
        (pr "q" \z)
        (prn {:a [1 2]})
        (println "line")
        (printf "%s-%d" "f" 7)
        (flush))
      (is (= (str "1 :a s c 1.5 nil" "\"q\" " "\\z" "{:a [1 2]}" newline-str
                  "line" newline-str "f-7")
             (apply str @chunks))))))

(deftest replying-writer-quota
  (testing "exactly the quota reaches the callback, then print-with-quota is true"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 5)]
      (is (true? (io/print-with-quota w #(with-open [w w]
                                           (io/write-string! w "abcdefgh")))))
      (is (= "abcde" (apply str @chunks)))))
  (testing "the quota counts across writes"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 5)]
      (is (true? (io/print-with-quota w #(with-open [w w]
                                           (io/write-string! w "abc")
                                           (io/write-string! w "def")
                                           (io/write-string! w "gh")))))
      (is (= "abcde" (apply str @chunks)))))
  (testing "text within the quota is not truncated"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 5)]
      (is (false? (io/print-with-quota w #(with-open [w w]
                                            (io/write-string! w "abcde")))))
      (is (= "abcde" (apply str @chunks)))))
  (testing "close, flush and the callback happen before print-with-quota returns"
    (let [order (atom [])
          w (io/replying-writer #(swap! order conj [:chunk %]) 1024 3)
          truncated (io/print-with-quota w #(with-open [w w]
                                              (io/write-string! w "abcdef")))]
      (swap! order conj [:returned truncated])
      (is (= [[:chunk "abc"] [:returned true]] @order))))
  (testing "a user catch of Exception around the overflowing write cannot hide the truncation"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 5)]
      (is (true? (io/print-with-quota w #(with-open [w w]
                                           (try (io/write-string! w "abcdefgh")
                                                (catch Exception _ nil))))))
      (is (= "abcde" (apply str @chunks)))))
  (testing "a quota of 1 delivers one character"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 1)]
      (is (true? (io/print-with-quota w #(with-open [w w]
                                           (io/write-string! w "xyz")))))
      (is (= "x" (apply str @chunks)))))
  (testing "printing through print-method stops at the quota"
    (let [[chunks cb] (collector)
          w (io/replying-writer cb 1024 12)]
      (is (true? (io/print-with-quota w #(with-open [w w]
                                           (binding [*out* w]
                                             (pr (range 100)))))))
      (is (= "(0 1 2 3 4 5" (apply str @chunks)))))
  (testing "a non-positive quota is rejected"
    (is (thrown? #?(:clj IllegalArgumentException :cljr ArgumentException)
                 (io/replying-writer (fn [_]) 1024 0)))))

(deftest print-with-quota-test
  (testing "false when nothing is truncated, whatever f returns"
    (let [[_ cb] (collector)
          w (io/replying-writer cb 1024 100)]
      (is (false? (io/print-with-quota w (fn [] :truthy))))
      (is (false? (io/print-with-quota w (fn [] nil))))
      (is (false? (io/print-with-quota w #(io/write-string! w "short"))))))
  (testing "false for a writer without a quota"
    (let [[_ cb] (collector)
          w (io/replying-writer cb 1024 nil)]
      (is (false? (io/print-with-quota w #(io/write-string! w (apply str (repeat 5000 "x"))))))))
  (testing "f runs exactly once"
    (let [n (atom 0)]
      (io/print-with-quota (io/string-writer) #(swap! n inc))
      (is (= 1 @n))))
  (testing "a non-quota exception from f propagates"
    (is (thrown? clojure.lang.ExceptionInfo
                 (io/print-with-quota (io/string-writer) #(throw (ex-info "boom" {}))))))
  (testing "a truncated string writer"
    (let [w (io/quota-writer (io/string-writer) 5)]
      (is (true? (io/print-with-quota w #(io/write-string! w "abcdefgh"))))
      (is (= "abcde" (io/writer->string w)))))
  (testing "a user catch inside f cannot hide the truncation of a string writer"
    (let [w (io/quota-writer (io/string-writer) 5)]
      (is (true? (io/print-with-quota w #(try (io/write-string! w "abcdefgh")
                                              (catch Exception _ nil)))))))
  (testing "a writer without a quota is never truncated"
    (let [w (io/string-writer)]
      (is (false? (io/print-with-quota w #(io/write-string! w (apply str (repeat 5000 "x"))))))
      (is (= 5000 (count (io/writer->string w)))))))

(deftest quota-writer-test
  (testing "no quota returns the writer itself"
    (let [w (io/string-writer)]
      (is (identical? w (io/quota-writer w nil)))))
  (testing "with a quota it wraps the writer and passes text through up to the quota"
    (let [inner (io/string-writer)
          w (io/quota-writer inner 6)]
      (is (not (identical? inner w)))
      (io/write-string! w "abc")
      (is (= "abc" (io/writer->string inner)))
      (io/write-string! w "def")
      (is (= "abcdef" (io/writer->string inner)))
      (is (true? (io/print-with-quota w #(io/write-string! w "g"))))
      (is (= "abcdef" (io/writer->string inner)))))
  (testing "the quota is counted in characters"
    (let [inner (io/string-writer)
          w (io/quota-writer inner 3)]
      (is (true? (io/print-with-quota w #(io/write-string! w "é€é€"))))
      (is (= "é€é" (io/writer->string inner)))))
  (testing "a non-positive quota is rejected"
    (is (thrown? #?(:clj IllegalArgumentException :cljr ArgumentException)
                 (io/quota-writer (io/string-writer) 0)))
    (is (thrown? #?(:clj IllegalArgumentException :cljr ArgumentException)
                 (io/quota-writer (io/string-writer) -3)))))

(defn- new-original []
  #?(:clj (let [bos (ByteArrayOutputStream.)]
            {:stream (PrintStream. bos true "UTF-8")
             :text #(.toString bos "UTF-8")})
     :cljr (let [sw (StringWriter.)]
             {:stream sw
              :text #(str sw)})))

(defn- print-to [stream s]
  (#?(:clj .print :cljr .Write)
   ^#?(:clj java.io.PrintStream :cljr System.IO.TextWriter) stream
   ^String s))

(deftest tee-writer-test
  (testing "the original gets everything, the callback complete lines"
    (let [[chunks cb] (collector)
          {:keys [stream text]} (new-original)
          tee (io/tee-writer stream cb)]
      (print-to tee "a\nb")
      (is (= "a\nb" (text)))
      (is (= ["a\n"] @chunks))
      (print-to tee "c\nd")
      (is (= "a\nbc\nd" (text)))
      (is (= "a\nbc\n" (apply str @chunks)))))
  (testing "flush! hands the callback the partial line"
    (let [[chunks cb] (collector)
          {:keys [stream text]} (new-original)
          tee (io/tee-writer stream cb)]
      (print-to tee "x\ny")
      (io/flush! tee)
      (is (= "x\ny" (text)))
      (is (= "x\ny" (apply str @chunks)))
      (is (= "y" (last @chunks)))))
  (testing "non-ASCII text arrives intact on both sides"
    (let [[chunks cb] (collector)
          {:keys [stream text]} (new-original)
          tee (io/tee-writer stream cb)
          s (str "hé" clef "\n")]
      (print-to tee s)
      (is (= s (text)))
      (is (= [s] @chunks))))
  (testing "close! hands the callback its tail"
    (let [[chunks cb] (collector)
          {:keys [stream]} (new-original)
          tee (io/tee-writer stream cb)]
      (print-to tee "tail")
      (io/close! tee)
      (is (= "tail" (apply str @chunks)))))
  (testing "a large write is split by the callback buffer, with nothing lost or duplicated"
    (let [[chunks cb] (collector)
          {:keys [stream text]} (new-original)
          tee (io/tee-writer stream cb)
          s (apply str (repeat 3000 "0123456789"))]
      (print-to tee s)
      (io/flush! tee)
      (is (= s (text)))
      (is (= s (apply str @chunks))))))

(deftest root-writer-test
  (testing "text written to the root writer reaches the stream beneath it"
    (let [{:keys [stream text]} (new-original)
          w (io/root-writer stream)]
      (io/write-string! w (str "ré" clef))
      (io/flush! w)
      (is (= (str "ré" clef) (text)))))
  (testing "printing through a root writer over a tee reaches both the original and the callback"
    (let [[chunks cb] (collector)
          {:keys [stream text]} (new-original)
          tee (io/tee-writer stream cb)
          w (io/root-writer tee)]
      (binding [*out* w]
        (println "hello")
        (print "partial")
        (flush))
      (is (= (str "hello" newline-str "partial") (text)))
      (is (= (str "hello" newline-str "partial") (apply str @chunks))))))

(defn- print-system! [which s]
  #?(:clj (.print ^PrintStream (case which :out System/out :err System/err) ^String s)
     :cljr (.Write ^System.IO.TextWriter (case which :out Console/Out :err Console/Error) ^String s)))

(defn- println-system! [which s]
  #?(:clj (.println ^PrintStream (case which :out System/out :err System/err) ^String s)
     :cljr (.WriteLine ^System.IO.TextWriter (case which :out Console/Out :err Console/Error) ^String s)))

(deftest system-writers-test
  (let [before (io/system-writers)
        out (new-original)
        err (new-original)]
    (testing "system-writers returns the current stdout and stderr"
      (is (= #{:out :err} (set (keys before))))
      (is (some? (:out before)))
      (is (some? (:err before)))
      (is (not (identical? (:out before) (:err before)))))
    (try
      (io/install-system-writers! (:stream out) (:stream err))
      (testing "what the process writes to stdout and stderr goes to the installed writers"
        (print-system! :out "to-out")
        (print-system! :err "to-err")
        (is (= "to-out" ((:text out))))
        (is (= "to-err" ((:text err)))))
      (testing "system-writers now returns writers that lead to the installed ones"
        (let [after (io/system-writers)]
          (is (not (identical? (:out before) (:out after))))
          (is (not (identical? (:err before) (:err after))))
          #?(:clj (do (is (identical? (:stream out) (:out after)))
                      (is (identical? (:stream err) (:err after))))
             :cljr (do (is (not (identical? (:stream out) (:out after))))
                       (is (instance? System.IO.TextWriter (:out after)))
                       (is (str/includes? (str (class (:out after))) "SyncTextWriter"))))))
      (finally
        (io/install-system-writers! (:out before) (:err before))))
    (testing "the originals are restored"
      (let [after (io/system-writers)]
        (is (identical? (:out before) (:out after)))
        (is (identical? (:err before) (:err after))))))
  (testing "a tee installed as the system writer sees what the process prints"
    (let [before (io/system-writers)
          [chunks cb] (collector)
          {:keys [stream text]} (new-original)
          tee (io/tee-writer stream cb)]
      (try
        (io/install-system-writers! tee tee)
        (print-system! :out "via-system\n")
        (is (= "via-system\n" (text)))
        (is (= ["via-system\n"] @chunks))
        (println-system! :out "line")
        (is (= (str "via-system\nline" newline-str) (text)))
        (is (= ["via-system\n" (str "line" newline-str)] @chunks))
        (finally
          (io/install-system-writers! (:out before) (:err before)))))))

#?(:cljr
   (deftest clr-replying-writer-chain
     (testing "the writer is the quota writer over the callback writer when there is a quota"
       (let [w (io/replying-writer (fn [_]) 1024 5)]
         (is (instance? QuotaBoundWriter w))
         (is (false? (.Exceeded ^QuotaBoundWriter w)))))
     (testing "and the callback writer itself when there is not"
       (is (instance? CallbackBufferedWriter (io/replying-writer (fn [_]) 1024 nil))))
     (testing "Exceeded is what print-with-quota reports after a swallowed overflow"
       (let [w (io/replying-writer (fn [_]) 1024 3)]
         (try (io/write-string! w "abcdef") (catch Exception _ nil))
         (is (true? (.Exceeded ^QuotaBoundWriter w)))
         (is (true? (io/print-with-quota w (fn []))))))
     (testing "a flush keeps a lone high surrogate for its low half, close! delivers it"
       (let [[chunks cb] (collector)
             w (io/replying-writer cb 1024 nil)]
         (io/write-string! w (str "ab" (char 0xD834)))
         (io/flush! w)
         (is (= ["ab"] @chunks))
         (io/write-string! w (str (char 0xDD1E)))
         (io/flush! w)
         (is (= ["ab" clef] @chunks))
         (io/write-string! w (str (char 0xD834)))
         (io/close! w)
         (is (= ["ab" clef (str (char 0xD834))] @chunks))))
     (testing "the callback receives strings"
       (let [seen (atom nil)
             w (io/replying-writer #(reset! seen %) 1024 nil)]
         (io/write-string! w "s\n")
         (is (string? @seen))))
     (testing "tee-writer is a TeeWriter and replying-writer output can be written to from many threads"
       (is (instance? TeeWriter (io/tee-writer (StringWriter.) (fn [_]))))
       (let [[chunks cb] (collector)
             lock (Object.)
             cb #(locking lock (cb %))
             w (io/replying-writer cb 64 nil)
             hs (doall (for [t (range 4)]
                         (spawn (fn []
                                  (dotimes [i 500]
                                    (io/write-string! w (str "t" t "-" i "\n")))))))]
         (doseq [h hs] (await-result h))
         (io/flush! w)
         (is (= (set (for [t (range 4) i (range 500)] (str "t" t "-" i)))
                (set (str/split-lines (apply str @chunks)))))
         (is (= 2000 (count (str/split-lines (apply str @chunks)))))))))

#?(:cljr
   (deftest dll-loading
     (let [dll-path @#'nrepl.util.io.clr/dll-path
           load-dll @#'nrepl.util.io.clr/load-dll
           path (dll-path)]
       (testing "the dll path ends in the build output directory or comes from the environment"
         (is (str/ends-with? path "nrepl.dll")))
       (testing "loading it again is harmless and yields the assembly the types came from"
         (let [asm (load-dll path)]
           (is (identical? asm (load-dll path)))
           (is (identical? asm (.Assembly ^Type (identity CallbackBufferedWriter))))
           (is (identical? asm (System.Reflection.Assembly/LoadFrom path)))))
       (testing "a missing dll fails with a message naming the build command"
         (let [e (try (load-dll "/nonexistent/nrepl.dll")
                      (catch clojure.lang.ExceptionInfo e e))]
           (is (instance? clojure.lang.ExceptionInfo e))
           (is (str/includes? (ex-message e) "dotnet build -c Release src/csharp/nrepl/nrepl.csproj"))
           (is (str/includes? (ex-message e) "/nonexistent/nrepl.dll"))
           (is (= :nrepl.util.io.clr/dll-not-found (:nrepl/kind (ex-data e)))))))))

#?(:cljr
   (defn- creation-ms [make-writer n]
     (let [watch (System.Diagnostics.Stopwatch/StartNew)]
       (dotimes [_ n] (make-writer))
       (double (.ElapsedMilliseconds watch)))))

#?(:cljr
   (deftest writer-creation-cost-is-flat
     (testing "replying writers dispatch to their own callback"
       (let [seen (atom [])
             writers (mapv (fn [i] (io/replying-writer #(swap! seen conj [i %]) 1024 nil))
                           (range 3))]
         (doseq [[i w] (map-indexed vector writers)]
           (io/write-string! w (str "w" i "\n")))
         (is (= [[0 "w0\n"] [1 "w1\n"] [2 "w2\n"]] @seen))))
     (testing "tee writers dispatch to their own callback"
       (let [seen (atom [])
             tees (mapv (fn [i] (io/tee-writer (StringWriter.) #(swap! seen conj [i %]))) (range 3))]
         (doseq [[i w] (map-indexed vector tees)]
           (io/write-string! w (str "t" i "\n")))
         (is (= [[0 "t0\n"] [1 "t1\n"] [2 "t2\n"]] @seen))))
     (testing "the last thousand of 6000 replying writers cost about what the first thousand did and stay cheap"
       (let [make #(io/replying-writer (fn [_]) 1024 nil)
             first-ms (creation-ms make 1000)
             _ (creation-ms make 4000)
             last-ms (creation-ms make 1000)]
         (is (< last-ms (min 150.0 (+ 50.0 (* 3.0 first-ms)))) (str first-ms " ms then " last-ms " ms"))))
     (testing "the same holds for tee writers"
       (let [original (StringWriter.)
             make #(io/tee-writer original (fn [_]))
             first-ms (creation-ms make 1000)
             _ (creation-ms make 4000)
             last-ms (creation-ms make 1000)]
         (is (< last-ms (min 150.0 (+ 50.0 (* 3.0 first-ms)))) (str first-ms " ms then " last-ms " ms"))))))

#?(:cljr
   (deftest stdin-reader-creation-cost-is-flat
     (testing "stdin readers ask through their own callback"
       (let [asked (atom [])
             readers (mapv (fn [i] (io/stdin-reader #(swap! asked conj i))) (range 3))
             handles (mapv (fn [r] (spawn #(rd r))) readers)]
         (is (wait-until #(= 3 (count @asked))))
         (is (= [0 1 2] (sort @asked)))
         (doseq [r readers] (io/add-input! r "x"))
         (is (= [(int \x) (int \x) (int \x)] (mapv await-result handles)))))
     (testing "the last thousand of 6000 stdin readers cost about what the first thousand did and stay cheap"
       (let [make #(io/stdin-reader (fn []))
             first-ms (creation-ms make 1000)
             _ (creation-ms make 4000)
             last-ms (creation-ms make 1000)]
         (is (< last-ms (min 150.0 (+ 50.0 (* 3.0 first-ms)))) (str first-ms " ms then " last-ms " ms"))))))
