(ns nrepl.util.out-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [nrepl.test-helpers :refer [eval-in-jvm is+ newline->sys]]
            [nrepl.util.out :as out])
  (:import nrepl.out.CallbackBufferedOutputStream))

(defn- reset-callbacks-fixture
  "Helper to reset the internal state for testing."
  [f]
  (reset! @#'out/callbacks {:out {}, :err {}})
  (f))

(use-fixtures :each reset-callbacks-fixture)

(defn- wait-for
  "Polls `pred` until it returns a truthy value or `timeout-ms` elapses.
  Callbacks are invoked on nREPL's transport executor, so the tests have to
  wait for them to have run instead of assuming they already have."
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (or (pred)
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 10)
            (recur))))))

(deftest test-wrap-standard-streams-idempotency
  (testing "wrap-standard-streams is idempotent"
    (let [original-out (:out @@#'out/original-print-streams)
          original-err (:err @@#'out/original-print-streams)]
      ;; First call should wrap streams
      (out/wrap-standard-streams)
      (let [first-out System/out
            first-err System/err]
        (is (not= original-out first-out))
        (is (not= original-err first-err))

        ;; Second call should not change streams again
        (out/wrap-standard-streams)
        (is (identical? first-out System/out))
        (is (identical? first-err System/err))))))

(defn- root-of [v] (alter-var-root v identity))

(def ^:private root-vars [#'*out* #'*err* #'clojure.test/*test-out*])

(defn- with-pristine-streams
  [f]
  (let [saved-system {:out System/out, :err System/err}
        saved-wrapped @out/wrapped?
        saved-original-streams @@#'out/original-print-streams
        saved-original-roots @@#'out/original-roots
        saved-roots (mapv root-of root-vars)]
    (try
      (when saved-wrapped
        (System/setOut (:out saved-original-streams))
        (System/setErr (:err saved-original-streams))
        (doseq [[v root] saved-original-roots]
          (alter-var-root v (constantly root)))
        (reset! out/wrapped? false))
      (f)
      (finally
        (System/setOut (:out saved-system))
        (System/setErr (:err saved-system))
        (reset! @#'out/original-print-streams saved-original-streams)
        (reset! @#'out/original-roots saved-original-roots)
        (reset! out/wrapped? saved-wrapped)
        (doseq [[v root] (map vector root-vars saved-roots)]
          (alter-var-root v (constantly root)))))))

(deftest test-unwrap-standard-streams-restores-originals
  (with-pristine-streams
    (fn []
      (let [pristine-out System/out
            pristine-err System/err
            pristine-roots (mapv root-of root-vars)]
        (out/wrap-standard-streams)
        (is (not (identical? pristine-out System/out)))
        (is (not (identical? pristine-err System/err)))
        (is (true? @out/wrapped?))
        (out/unwrap-standard-streams)
        (is (identical? pristine-out System/out))
        (is (identical? pristine-err System/err))
        (is (false? @out/wrapped?))
        (is (every? true? (map identical? pristine-roots (mapv root-of root-vars))))))))

(deftest test-unwrap-standard-streams-keeps-callbacks-and-allows-rewrap
  (with-pristine-streams
    (fn []
      (let [calls (atom [])
            callback #(when (str/includes? % "unwrap-test") (swap! calls conj %))]
        (out/set-callback :out ::kept callback)
        (out/wrap-standard-streams)
        (.println System/out "unwrap-test first")
        (is (wait-for #(= 1 (count @calls)) 5000))
        (out/unwrap-standard-streams)
        (is (= {::kept callback} (get @@#'out/callbacks :out)))
        (.println System/out "unwrap-test while unwrapped")
        (Thread/sleep 300)
        (is (= [(newline->sys "unwrap-test first\n")] @calls))
        (out/wrap-standard-streams)
        (is (true? @out/wrapped?))
        (.println System/out "unwrap-test again")
        (is (wait-for #(= 2 (count @calls)) 5000))
        (is (= [(newline->sys "unwrap-test first\n") (newline->sys "unwrap-test again\n")] @calls))))))

(deftest test-unwrap-standard-streams-is-idempotent
  (with-pristine-streams
    (fn []
      (reset! @#'out/original-print-streams {})
      (let [pristine-out System/out
            pristine-err System/err]
        (out/unwrap-standard-streams)
        (is (identical? pristine-out System/out))
        (is (identical? pristine-err System/err)))
      (out/wrap-standard-streams)
      (out/unwrap-standard-streams)
      (let [sentinel (java.io.PrintStream. (java.io.ByteArrayOutputStream.))]
        (System/setOut sentinel)
        (out/unwrap-standard-streams)
        (is (identical? sentinel System/out))))))

(deftest test-output-interception
  (testing "callbacks are invoked when output is written"
    (out/wrap-standard-streams)
    (let [out-calls (atom [])
          err-calls (atom [])]
      (out/set-callback :out :test1 #(swap! out-calls conj [:test1 %]))
      (out/set-callback :out :test2 #(swap! out-calls conj [:test2 %]))
      (out/set-callback :err :test3 #(swap! err-calls conj [:test3 %]))

      ;; Write to streams
      (.println System/out "Hello stdout")
      (.println System/err "Hello stderr")
      (wait-for #(and (= 2 (count @out-calls)) (= 1 (count @err-calls))) 5000)

      (is+ #{[:test1 (newline->sys "Hello stdout\n")]
             [:test2 (newline->sys "Hello stdout\n")]}
           (set @out-calls))
      (is+ [[:test3 (newline->sys "Hello stderr\n")]]
           @err-calls))))

(deftest test-root-binding-updates
  (testing "*out* and *err* root bindings are updated to point to wrapped streams"
    (out/wrap-standard-streams)
    (let [out-calls (atom [])
          err-calls (atom [])]
      (out/set-callback :out :test1 #(swap! out-calls conj [:test1 %]))
      (out/set-callback :out :test2 #(swap! out-calls conj [:test2 %]))
      (out/set-callback :err :test3 #(swap! err-calls conj [:test3 %]))

      ;; Write to streams
      (.start (Thread. #(println "Hello stdout")))
      (.start (Thread. #(binding [*out* *err*]
                          (println "Hello stderr"))))
      (wait-for #(and (= 2 (count @out-calls)) (= 1 (count @err-calls))) 5000)

      (is+ #{[:test1 (newline->sys "Hello stdout\n")]
             [:test2 (newline->sys "Hello stdout\n")]}
           (set @out-calls))
      (is+ [[:test3 (newline->sys "Hello stderr\n")]]
           @err-calls))))

(deftest test-error-handling-in-callbacks
  (testing "exceptions in one callback shouldn't break others"
    (out/wrap-standard-streams)
    (let [out-calls (atom [])]
      (out/set-callback :out ::bad-callback #(/ 1 %))
      (out/set-callback :out ::good-callback #(swap! out-calls conj %))

      (dotimes [_ 10]
        (.println System/out "1"))
      (wait-for #(= 10 (count @out-calls)) 5000)

      (is+ (vec (repeat 10 (newline->sys "1\n")))
           @out-calls))))

(deftest test-multibyte-character-handling
  (doseq [buffer-len (range 10 20)
          prefix-len (range 1 (inc buffer-len))]
    (let [chunks   (atom [])
          callback (fn [s] (swap! chunks conj s))
          stream   (CallbackBufferedOutputStream. callback buffer-len)
          prefix (byte-array prefix-len (byte (int \-)))
          sun (.getBytes "☀" "UTF-8")]
      (.write stream prefix 0 (count prefix))
      (.write stream sun 0 (count sun))
      (.flush stream)
      (is (str/ends-with? (last @chunks) "☀") (str @chunks))
      (is (every? #{\- \☀} (apply str @chunks)) (str @chunks)))))

(def ^:private e-acute-roundtrip-code
  "(require 'nrepl.util.out)
   (let [got (promise)]
     (nrepl.util.out/wrap-standard-streams)
     (nrepl.util.out/set-callback :out :test #(deliver got %))
     (.write *out* (str (char 0xe9)))
     (.flush *out*)
     (println \"RESULT\" (pr-str (mapv int (deref got 10000 \"\"))))
     (shutdown-agents))")

(deftest wrapped-standard-streams-encode-utf-8-regardless-of-default-charset
  (let [{:keys [out]} (eval-in-jvm {"file.encoding" "ISO-8859-1"}
                                   e-acute-roundtrip-code 120000)]
    (is (= [0xe9] (some->> out (re-find #"RESULT (\[[0-9 ]*\])") second read-string))
        out)))
