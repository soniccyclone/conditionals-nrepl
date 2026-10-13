(ns nrepl.util.io.jvm
  "internal host seam"
  {:no-doc true}
  (:require
   [clojure.java.io :as io])
  (:import
   (clojure.lang LineNumberingPushbackReader RT)
   (java.io ByteArrayOutputStream Closeable Flushable InputStream
            OutputStreamWriter PrintStream PrintWriter
            PushbackInputStream PushbackReader Reader StringReader
            StringWriter Writer)
   (java.lang.reflect Field)
   (java.nio.charset StandardCharsets)
   (nrepl.in QueuePollingReader)
   (nrepl.out CallbackBufferedOutputStream QuotaBoundWriter QuotaExceeded
              TeeOutputStream)))

(defn pushback-input-stream
  [^InputStream in]
  (PushbackInputStream. in))

(def byte-array-class (RT/classForName "[B"))

(defn byte-array?
  [o]
  (= Byte/TYPE (.getComponentType (class o))))

(defn array?
  [o]
  (.isArray (class o)))

(defn byte-sink
  []
  (ByteArrayOutputStream.))

(defn sink->bytes
  [^ByteArrayOutputStream s]
  (.toByteArray s))

(defn copy!
  [in out]
  (io/copy in out))

(defn utf8-bytes
  ^bytes [^String s]
  (.getBytes s StandardCharsets/UTF_8))

(defn utf8-string
  ^String [^bytes b]
  (String. b StandardCharsets/UTF_8))

(defn text-reader
  [x]
  (io/reader x))

(defn text-writer
  [x]
  (io/writer x))

(defn pushback-reader
  [^Reader r]
  (PushbackReader. r))

(defn pushback-reader?
  [x]
  (instance? PushbackReader x))

(defn line-numbering-reader
  [^Reader r]
  (LineNumberingPushbackReader. r))

(defn line-numbering-reader?
  [x]
  (instance? LineNumberingPushbackReader x))

(defn set-line!
  [^LineNumberingPushbackReader reader line]
  (-> reader (.setLineNumber line)))

(defn set-column!
  [^LineNumberingPushbackReader reader column]
  (when-let [field (->> LineNumberingPushbackReader
                        (.getDeclaredFields)
                        (filter #(= "_columnNumber" (.getName ^Field %)))
                        first)]
    (-> ^Field field
        (doto (.setAccessible true))
        (.set reader column))))

(defn reader-ready?
  [^Reader r]
  (.ready r))

(defn string-reader
  [^String s]
  (StringReader. s))

(defn string-writer
  []
  (StringWriter.))

(defn writer->string
  [w]
  (str w))

(defn write-string!
  [^Writer w ^String s]
  (.write w s))

(defn flush!
  [^Flushable w]
  (.flush w))

(defn close!
  [^Closeable x]
  (.close x))

(defn closeable?
  [x]
  (instance? Closeable x))

(defn file-name
  [path]
  (.getName (io/file path)))

(defn stdin-reader
  [need-input-fn]
  (QueuePollingReader. need-input-fn))

(defprotocol StdinSink
  (add-input! [r s])
  (add-eof! [r]))

(extend-protocol StdinSink
  QueuePollingReader
  (add-input! [r s] (.addInput r s))
  (add-eof! [r] (.addEof r)))

(defn quota-writer
  ^Writer [^Writer writer quota]
  (if quota
    (QuotaBoundWriter. writer quota)
    writer))

(defn replying-writer
  ^PrintWriter [callback buffer-size quota]
  (-> (CallbackBufferedOutputStream. callback buffer-size)
      (OutputStreamWriter. StandardCharsets/UTF_8)
      (quota-writer quota)
      (PrintWriter. true)))

(defn print-with-quota
  [_writer f]
  (try
    (f)
    false
    (catch QuotaExceeded _
      true)))

(defn tee-writer
  [original callback]
  (PrintStream.
   (TeeOutputStream. original
                     (CallbackBufferedOutputStream. callback 1024))))

(defn root-writer
  [^PrintStream stream]
  (PrintWriter. (OutputStreamWriter. stream StandardCharsets/UTF_8)))

(defn install-system-writers!
  [out err]
  (System/setOut out)
  (System/setErr err))

(defn system-writers
  []
  {:out System/out, :err System/err})
