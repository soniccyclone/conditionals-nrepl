(ns nrepl.util.threading.jvm
  "internal host seam"
  {:no-doc true}
  (:require
   [clojure.main]
   [nrepl.config :refer [config]]
   [nrepl.misc :as misc]
   [nrepl.util.classloader :as classloader])
  (:import
   (clojure.lang Compiler$CompilerException)
   (java.lang.reflect Method)
   (java.util.concurrent BlockingQueue Executors ExecutorService Future
                         LinkedBlockingQueue ScheduledExecutorService
                         SynchronousQueue TimeUnit)
   (nrepl DaemonThreadFactory SessionThread)))

(defn thread-factory
  [thread-name-fmt]
  (DaemonThreadFactory. thread-name-fmt (classloader/dynamic-classloader)))

(def infinite-ms Long/MAX_VALUE)

(defn make-executor
  [{:keys [name-fmt]}]
  (Executors/newCachedThreadPool (thread-factory name-fmt)))

(defn submit!
  [executor f]
  (.submit ^ExecutorService executor ^Runnable f))

(defn cancel!
  [handle]
  (.cancel ^Future handle true))

(defn cancelled?
  []
  (Thread/interrupted))

(defn cancel-current!
  []
  (.interrupt (Thread/currentThread)))

(defn make-queue
  []
  (LinkedBlockingQueue.))

(defn make-rendezvous
  []
  (SynchronousQueue.))

(defn put!
  [q x]
  (.put ^BlockingQueue q x))

(defn take!
  [q]
  (.take ^BlockingQueue q))

(defn poll!
  ([q]
   (.poll ^BlockingQueue q))
  ([q timeout-ms]
   (.poll ^BlockingQueue q (long timeout-ms) TimeUnit/MILLISECONDS)))

(def ^:private reaper
  (delay (Executors/newScheduledThreadPool
          0 (thread-factory "nREPL-thread-reaper-%d"))))

(defn schedule-once!
  [f delay-ms]
  (.schedule ^ScheduledExecutorService @reaper
             ^Runnable f
             (long delay-ms) TimeUnit/MILLISECONDS))

(defn session-thread
  [name run-fn]
  (SessionThread. run-fn name (classloader/dynamic-classloader)))

(defn start-session-thread!
  [sthread]
  (.start ^Thread sthread))

(defn run-interruptible*
  [_sthread f]
  (f))

(defn alive?
  [sthread]
  (not= (.getState ^Thread sthread) Thread$State/TERMINATED))

(defn- deprecated-thread-stop [t]
  (try
    ;; JDK26+ has removed Thread.stop completely, so to avoid linter warnings,
    ;; use a roundabout reflection method to invoke it.
    (when-let [m (.getDeclaredMethod Thread "stop" (into-array Class []))]
      (.invoke ^Method m t (object-array 0)))
    (catch NoSuchMethodException _)))

(defn- jvmti-stop-thread [t]
  ((requiring-resolve 'nrepl.util.jvmti/stop-thread) t))

(defn- try-stop-thread [^Thread t]
  (cond
    (<= misc/java-version 19) (deprecated-thread-stop t)
    ;; Since JDK20, Thread.stop() no longer works. We must resort to using
    ;; JVMTI native agent which luckily still supports Stop Thread command.
    ;; Whether this is more dangerous than calling Thread.stop() in earlier
    ;; JDKs is unknown, but assume the worst and never use this if you can't
    ;; take the risk!
    (misc/jvmti-agent-enabled?) (jvmti-stop-thread t)

    (not (misc/attach-self-enabled?))
    (misc/log "Cannot stop thread on JDK21+ without -Djdk.attach.allowAttachSelf"
              "enabled, see https://nrepl.org/nrepl/installation.html#jvmti.")))

(defn interrupt-stop
  [^Thread t delay-ms]
  (.interrupt t)
  (when (:force-interrupt config true)
    (schedule-once! #(misc/log-exceptions
                      (when (alive? t)
                        (try-stop-thread t)))
                    delay-ms)))

(defn stop-exception?
  [^Throwable e]
  (or (instance? ThreadDeath (clojure.main/root-cause e))
      (and (instance? Compiler$CompilerException e)
           (instance? ThreadDeath (ex-cause e)))))
