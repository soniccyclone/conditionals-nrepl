(ns nrepl.util.threading
  "internal host seam"
  {:added "1.3"
   :no-doc true}
  (:require
   #?(:clj [nrepl.util.threading.jvm :as impl]
      :cljr [nrepl.util.threading.clr :as impl])))

#?(:clj
   (def thread-factory
     "Return a thread factory that produces daemon threads with the name specified by
  `thread-name-fmt`."
     impl/thread-factory))

(def make-executor impl/make-executor)
(def submit! impl/submit!)
(def cancel! impl/cancel!)
(def cancelled? impl/cancelled?)
(def cancel-current! impl/cancel-current!)
(def make-queue impl/make-queue)
(def make-rendezvous impl/make-rendezvous)
(def put! impl/put!)
(def take! impl/take!)
(def poll! impl/poll!)
(def infinite-ms impl/infinite-ms)
(def schedule-once! impl/schedule-once!)
(def session-thread impl/session-thread)
(def start-session-thread! impl/start-session-thread!)
(def stop-exception? impl/stop-exception?)
(def alive? impl/alive?)

(defmacro run-with
  "Run the provided `body` using `executor`. Don't wait for the result (unless the
  executor itself is blocking)."
  {:style/indent 1}
  [executor & body]
  #?(:clj `(.submit ~(with-meta executor {:tag 'java.util.concurrent.ExecutorService})
                    (reify Runnable
                      (run [_#]
                        ~@body)))
     :cljr `(submit! ~executor (fn [] ~@body))))

(defmacro run-interruptible
  {:style/indent 1}
  [_sthread & body]
  #?(:clj `(do ~@body)
     :cljr `(impl/run-interruptible* ~_sthread (fn [] ~@body))))

;; Executors are wrapped in delays to defer their initialization.

(def listen-executor
  "Executor used to accept incoming connections."
  (delay (make-executor {:name-fmt "nREPL-server-%d"})))

(def connection-executor
  "Executor used to run the handler loop of each connection."
  (delay (make-executor {:name-fmt "nREPL-connection-%d"})))

(def transport-executor
  "Executor used to run the transport reader loops."
  (delay (make-executor {:name-fmt "nREPL-transport-%d"})))

(def handle-executor
  "Executor used to handle individual requests."
  (delay (make-executor {:name-fmt "nREPL-handler-%d"})))

;;;; Thread interruption. Used by session middleware to make eval interruptible.

(def force-stop-delay-ms 5000)

(defn interrupt-stop
  "Try to interrupt the thread normally. Asynchronously wait for
  `force-stop-delay-ms` and, if the thread didn't terminate itself cleanly,
  kill the thread using either `.stop` or JVMTI agent, unless the
  `:force-interrupt` config key is false. This behaviour strikes a balance
  between allowing a thread to respond to an interrupt, but also ensuring we
  clean up runaway processes."
  [t]
  (impl/interrupt-stop t force-stop-delay-ms))
