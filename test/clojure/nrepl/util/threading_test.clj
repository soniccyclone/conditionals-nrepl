(ns nrepl.util.threading-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [nrepl.middleware.session :as session]
   [nrepl.test-helpers :refer [win?]]
   [nrepl.util.threading :as threading]
   [nrepl.util.threading.jvm :as jvm])
  (:import
   (clojure.lang Compiler$CompilerException)
   (java.util.concurrent ExecutorService)))

(def ^:private helper-executor
  (delay (threading/make-executor {:name-fmt "threading-test-helper-%d"})))

(deftest make-executor-threads-test
  (let [executor (threading/make-executor {:name-fmt "threading-test-%d"})
        release (promise)
        names (atom [])
        task (fn []
               (swap! names conj [(.getName (Thread/currentThread))
                                  (.isDaemon (Thread/currentThread))])
               @release)]
    (try
      (threading/submit! executor task)
      (threading/submit! executor task)
      (loop [n 0]
        (when (and (< (count @names) 2) (< n 500))
          (Thread/sleep 10)
          (recur (inc n))))
      (is (= #{["threading-test-0" true] ["threading-test-1" true]}
             (set @names)))
      (finally
        (deliver release true)
        (.shutdown ^ExecutorService executor)))))

(deftest cancel-ends-a-cancellable-loop-test
  (let [executor (threading/make-executor {:name-fmt "threading-test-%d"})
        started (promise)
        exited (promise)]
    (try
      (let [handle (threading/submit! executor
                                      (fn []
                                        (deliver started true)
                                        (while (not (threading/cancelled?))
                                          (Thread/onSpinWait))
                                        (deliver exited true)))]
        (is (true? (deref started 5000 false)))
        (Thread/sleep 100)
        (is (not (realized? exited)))
        (threading/cancel! handle)
        (is (true? (deref exited 5000 false))))
      (finally
        (.shutdown ^ExecutorService executor)))))

(deftest schedule-once-test
  (let [calls (atom [])
        start (System/nanoTime)
        elapsed-ms #(quot (- (System/nanoTime) start) 1000000)]
    (threading/schedule-once! #(swap! calls conj (elapsed-ms)) 400)
    (Thread/sleep 150)
    (is (empty? @calls))
    (Thread/sleep 1500)
    (is (= 1 (count @calls)))
    (is (<= 380 (first @calls)))))

(deftest rendezvous-test
  (testing "put! does not return until another thread takes the item"
    (let [queue (threading/make-rendezvous)
          done (promise)]
      (threading/submit! @helper-executor
                         (fn []
                           (threading/put! queue :x)
                           (deliver done true)))
      (Thread/sleep 200)
      (is (not (realized? done)))
      (is (= :x (threading/take! queue)))
      (is (true? (deref done 5000 false)))))

  (testing "put! also returns once the item was polled"
    (let [queue (threading/make-rendezvous)
          done (promise)]
      (threading/submit! @helper-executor
                         (fn []
                           (threading/put! queue :y)
                           (deliver done true)))
      (is (= :y (threading/poll! queue 5000)))
      (is (true? (deref done 5000 false)))))

  (testing "poll! with a timeout returns nil when nothing arrives"
    (let [queue (threading/make-rendezvous)
          start (System/nanoTime)]
      (is (nil? (threading/poll! queue 100)))
      (is (<= 90 (quot (- (System/nanoTime) start) 1000000)))))

  (testing "poll! accepts the infinite timeout and waits for the item"
    (let [queue (threading/make-rendezvous)]
      (threading/submit! @helper-executor
                         (fn []
                           (Thread/sleep 100)
                           (threading/put! queue :z)))
      (is (= :z (threading/poll! queue threading/infinite-ms)))))

  (testing "the 1-arity poll! of a queue returns at once"
    (let [queue (threading/make-queue)]
      (is (nil? (threading/poll! queue)))
      (threading/put! queue :w)
      (is (= :w (threading/poll! queue)))
      (is (nil? (threading/poll! queue))))))

(deftest session-thread-test
  (let [release (promise)
        ran (promise)
        thread (threading/session-thread "threading-test-session"
                                         (fn []
                                           (deliver ran [(.getName (Thread/currentThread))
                                                         (.isDaemon (Thread/currentThread))])
                                           @release))]
    (Thread/sleep 200)
    (is (not (realized? ran)))
    (threading/start-session-thread! thread)
    (is (= ["threading-test-session" true] (deref ran 5000 :timeout)))
    (is (true? (threading/alive? thread)))
    (deliver release true)
    (.join ^Thread thread 5000)
    (is (false? (threading/alive? thread)))))

(deftest interrupt-stop-wakes-a-sleeping-thread-test
  (let [woke (promise)
        thread (threading/session-thread "threading-test-sleeper"
                                         (fn []
                                           (try (Thread/sleep 30000)
                                                (catch InterruptedException _
                                                  (deliver woke true)))))]
    (threading/start-session-thread! thread)
    (Thread/sleep 100)
    (jvm/interrupt-stop thread 60000)
    (is (true? (deref woke 2000 false)))))

(when-not win?
  (deftest interrupt-stop-kills-a-spinning-thread-test
    (let [thread (threading/session-thread "threading-test-spinner"
                                           (fn []
                                             (try (loop [] (recur))
                                                  (catch ThreadDeath _))))]
      (threading/start-session-thread! thread)
      (with-redefs [threading/force-stop-delay-ms 1000]
        (threading/interrupt-stop thread))
      (Thread/sleep 300)
      (is (true? (threading/alive? thread)))
      (.join ^Thread thread 6000)
      (is (false? (threading/alive? thread))))))

(deftest stop-exception-test
  (is (true? (threading/stop-exception? (ThreadDeath.))))
  (is (true? (threading/stop-exception? (RuntimeException. "wrapped" (ThreadDeath.)))))
  (is (true? (threading/stop-exception? (Compiler$CompilerException. "f.clj" 1 1 (ThreadDeath.)))))
  (is (false? (threading/stop-exception? (InterruptedException.))))
  (is (false? (threading/stop-exception? (Compiler$CompilerException. "f.clj" 1 1 (InterruptedException.)))))
  (is (false? (threading/stop-exception? (RuntimeException. "plain")))))

(deftest runnable-in-a-persistent-session-test
  (let [session' (#'session/create-session {})
        ran (promise)]
    (alter-meta! session' into (session/session-exec session'))
    (try
      ((:exec (meta session'))
       "runnable-id"
       (reify Runnable
         (run [_] (deliver ran (.getName (Thread/currentThread)))))
       nil
       {:session session'})
      (is (= (str "nREPL-session-" (:id (meta session'))) (deref ran 5000 :timeout)))
      (finally
        ((:close (meta session')))))))
