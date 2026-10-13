(ns nrepl.util.threading-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [nrepl.config :as config]
   [nrepl.misc :as misc]
   [nrepl.test-helpers.bounded :as bounded]
   [nrepl.util.threading :as threading])
  #?(:clj (:require
           [nrepl.util.threading.jvm :as impl])
     :cljr (:require
            [nrepl.util.threading.clr :as impl]))
  #?@(:clj [(:import
             (clojure.lang Compiler$CompilerException)
             (java.util.concurrent ExecutorService))]
      :cljr [(:import
              (clojure.lang Compiler+CompilerException)
              (nrepl.util.threading.clr Rendezvous SessionThread)
              (System.Reflection BindingFlags)
              (System.Threading ThreadAbortException ThreadInterruptedException))]))

(def ^:private interrupted-exception
  #?(:clj InterruptedException :cljr ThreadInterruptedException))

(def ^:private win?
  #?(:clj (.startsWith (System/getProperty "os.name") "Windows")
     :cljr (System.Runtime.InteropServices.RuntimeInformation/IsOSPlatform
            System.Runtime.InteropServices.OSPlatform/Windows)))

(def ^:private helper-executor
  (delay (threading/make-executor {:name-fmt "threading-test-helper-%d"})))

(defn- current-thread-name
  []
  #?(:clj (.getName (Thread/currentThread))
     :cljr (.Name System.Threading.Thread/CurrentThread)))

(defn- current-thread-background?
  []
  #?(:clj (.isDaemon (Thread/currentThread))
     :cljr (.IsBackground System.Threading.Thread/CurrentThread)))

(defn- now-ms
  []
  #?(:clj (quot (System/nanoTime) 1000000)
     :cljr (Environment/TickCount64)))

(defn- spin
  []
  #?(:clj (Thread/yield)
     :cljr (System.Threading.Thread/SpinWait 1)))

(defn- shutdown!
  [executor]
  #?(:clj (.shutdown ^ExecutorService executor)
     :cljr nil))

(defn- await-dead
  [sthread]
  (bounded/wait-until #(not (threading/alive? sthread))))

(deftest make-executor-threads-test
  (let [executor (threading/make-executor {:name-fmt "threading-test-%d"})
        release (promise)
        names (atom [])
        task (fn []
               (swap! names conj [(current-thread-name) (current-thread-background?)])
               @release)]
    (try
      (threading/submit! executor task)
      (threading/submit! executor task)
      (bounded/wait-until #(= 2 (count @names)))
      (is (= #{["threading-test-0" true] ["threading-test-1" true]}
             (set @names)))
      (finally
        (deliver release true)
        (shutdown! executor)))))

(deftest run-with-and-shared-executors-test
  (let [names (promise)]
    (threading/run-with @helper-executor
      (deliver names [(current-thread-name) (current-thread-background?)]))
    (let [[n background?] (deref names bounded/wait-ms [nil nil])]
      (is (re-matches #"threading-test-helper-\d+" (str n)))
      (is (true? background?))))
  (doseq [[executor pattern] [[threading/listen-executor #"nREPL-server-\d+"]
                              [threading/connection-executor #"nREPL-connection-\d+"]
                              [threading/transport-executor #"nREPL-transport-\d+"]
                              [threading/handle-executor #"nREPL-handler-\d+"]]]
    (let [names (promise)]
      (threading/run-with @executor
        (deliver names (current-thread-name)))
      (is (re-matches pattern (str (deref names bounded/wait-ms nil)))))))

(deftest many-tasks-test
  (let [n 1000
        done (atom 0)
        all (promise)]
    (dotimes [_ n]
      (threading/submit! @helper-executor
                         (fn []
                           (when (= n (swap! done inc))
                             (deliver all true)))))
    (is (true? (deref all bounded/wait-ms false)))))

(deftest cancel-ends-a-cancellable-loop-test
  (let [executor (threading/make-executor {:name-fmt "threading-test-%d"})
        started (promise)
        exited (atom false)]
    (try
      (let [handle (threading/submit! executor
                                      (fn []
                                        (deliver started true)
                                        (while (not (threading/cancelled?))
                                          (spin))
                                        (reset! exited true)))]
        (is (true? (deref started bounded/wait-ms false)))
        (bounded/sleep-ms 100)
        (is (false? @exited))
        (threading/cancel! handle)
        (is (true? (bounded/wait-until #(deref exited)))))
      (finally
        (shutdown! executor)))))

(deftest cancel-current-test
  (let [result (promise)]
    (threading/submit! @helper-executor
                       (fn []
                         (let [before (threading/cancelled?)]
                           (try (threading/cancel-current!)
                                (deliver result [before (threading/cancelled?)])
                                (catch Exception _
                                  (deliver result [before :threw]))))))
    (is (= [false true] (deref result bounded/wait-ms :timeout)))))

(deftest cancel-wakes-a-blocked-task-test
  (testing "a take! on a queue"
    (let [queue (threading/make-queue)
          started (promise)
          woke (promise)
          handle (threading/submit! @helper-executor
                                    (fn []
                                      (try (deliver started true)
                                           (threading/take! queue)
                                           (catch #?(:clj InterruptedException :cljr ThreadInterruptedException) _
                                             (deliver woke true)))))]
      (is (true? (deref started bounded/wait-ms false)))
      (bounded/sleep-ms 100)
      (is (not (realized? woke)))
      (threading/cancel! handle)
      (is (true? (deref woke bounded/wait-ms false)))))

  (testing "a put! on a rendezvous drops its item"
    (let [queue (threading/make-rendezvous)
          started (promise)
          done (promise)
          handle (threading/submit! @helper-executor
                                    (fn []
                                      (try (deliver started true)
                                           (threading/put! queue :dropped)
                                           (catch #?(:clj InterruptedException :cljr ThreadInterruptedException) _))
                                      (deliver done true)))]
      (is (true? (deref started bounded/wait-ms false)))
      (bounded/sleep-ms 100)
      (is (not (realized? done)))
      (threading/cancel! handle)
      (is (true? (deref done bounded/wait-ms false)))
      (is (nil? (threading/poll! queue 100)))))

  (testing "a cancelled put! does not clobber the item of a waiting put!"
    (let [queue (threading/make-rendezvous)
          waiting (promise)
          done (promise)]
      (threading/submit! @helper-executor
                         (fn []
                           (deliver waiting true)
                           (threading/put! queue :kept)))
      (is (true? (deref waiting bounded/wait-ms false)))
      (bounded/sleep-ms 200)
      (threading/submit! @helper-executor
                         (fn []
                           (threading/cancel-current!)
                           (try (threading/put! queue :clobber)
                                (catch #?(:clj InterruptedException :cljr ThreadInterruptedException) _))
                           (deliver done true)))
      (is (true? (deref done bounded/wait-ms false)))
      (is (= :kept (bounded/bounded #(threading/take! queue))))
      (is (nil? (threading/poll! queue 100)))))

  (testing "a put! after cancel-current! does not block and drops its item"
    (let [queue (threading/make-rendezvous)
          done (promise)]
      (threading/submit! @helper-executor
                         (fn []
                           (threading/cancel-current!)
                           (try (threading/put! queue :dropped)
                                (catch #?(:clj InterruptedException :cljr ThreadInterruptedException) _))
                           (deliver done true)))
      (is (true? (deref done bounded/wait-ms false)))
      (is (nil? (threading/poll! queue 100))))))

(deftest a-failing-task-does-not-end-the-run-test
  (let [ran (promise)]
    (threading/submit! @helper-executor
                       (fn [] (throw (ex-info "threading-test failing task (expected)" {}))))
    (threading/submit! @helper-executor
                       (fn [] (deliver ran true)))
    (is (true? (deref ran bounded/wait-ms false)))))

(deftest schedule-once-test
  (let [calls (atom [])
        start (now-ms)
        elapsed-ms #(- (now-ms) start)]
    (threading/schedule-once! #(swap! calls conj (elapsed-ms)) 400)
    (bounded/sleep-ms 150)
    (is (empty? @calls))
    (bounded/sleep-ms 1500)
    (is (= 1 (count @calls)))
    (is (<= 380 (first @calls)))))

(deftest schedule-once-survives-a-failing-function-test
  (let [ran (promise)]
    (threading/schedule-once! (fn [] (throw (ex-info "threading-test failing reaper (expected)" {}))) 10)
    (threading/schedule-once! #(deliver ran true) 100)
    (is (true? (deref ran bounded/wait-ms false)))))

(deftest queue-test
  (let [queue (threading/make-queue)]
    (testing "infinite-ms is the host's wait-forever timeout"
      (is (= #?(:clj Long/MAX_VALUE :cljr System.Threading.Timeout/Infinite) threading/infinite-ms)))
    (testing "items come out in order"
      (threading/put! queue :a)
      (threading/put! queue :b)
      (threading/put! queue :c)
      (is (= [:a :b :c] [(threading/take! queue) (threading/poll! queue) (threading/poll! queue 10)])))
    (testing "poll! returns nil when the queue is empty"
      (is (nil? (threading/poll! queue)))
      (is (nil? (threading/poll! queue 0))))
    (testing "poll! with a timeout waits for the full time"
      (let [start (now-ms)]
        (is (nil? (threading/poll! queue 100)))
        (is (<= 90 (- (now-ms) start)))))
    (testing "a negative timeout other than infinite-ms does not throw"
      (is (nil? (threading/poll! queue -5)))
      (threading/put! queue :neg)
      (is (= :neg (threading/poll! queue -5))))
    (testing "poll! returns an item that arrives in time"
      (threading/submit! @helper-executor
                         (fn []
                           (bounded/sleep-ms 100)
                           (threading/put! queue :late)))
      (is (= :late (threading/poll! queue 5000))))
    (testing "take! blocks until an item arrives"
      (threading/submit! @helper-executor
                         (fn []
                           (bounded/sleep-ms 100)
                           (threading/put! queue :taken)))
      (is (= :taken (bounded/bounded #(threading/take! queue)))))
    (testing "poll! accepts the infinite timeout"
      (threading/submit! @helper-executor
                         (fn []
                           (bounded/sleep-ms 100)
                           (threading/put! queue :infinite)))
      (is (= :infinite (bounded/bounded #(threading/poll! queue threading/infinite-ms)))))
    (testing "poll! clamps a timeout larger than an int instead of throwing"
      (threading/put! queue :huge)
      (is (= :huge (threading/poll! queue (bit-shift-left 1 40))))
      (threading/put! queue :max)
      (is (= :max (threading/poll! queue #?(:clj Long/MAX_VALUE :cljr Int64/MaxValue)))))
    (testing "an interrupt wakes a blocked take!"
      (let [woke (promise)
            t (bounded/start-thread
               (fn []
                 (try (threading/take! queue)
                      (catch #?(:clj InterruptedException :cljr ThreadInterruptedException) _
                        (deliver woke true)))))]
        (bounded/sleep-ms 100)
        (bounded/interrupt-thread! t)
        (is (true? (deref woke bounded/wait-ms false)))))))

(deftest rendezvous-test
  (testing "put! does not return until another thread takes the item"
    (let [queue (threading/make-rendezvous)
          done (promise)]
      (threading/submit! @helper-executor
                         (fn []
                           (threading/put! queue :x)
                           (deliver done true)))
      (bounded/sleep-ms 200)
      (is (not (realized? done)))
      (is (= :x (bounded/bounded #(threading/take! queue))))
      (is (true? (deref done bounded/wait-ms false)))))

  (testing "put! also returns once the item was polled"
    (let [queue (threading/make-rendezvous)
          done (promise)]
      (threading/submit! @helper-executor
                         (fn []
                           (threading/put! queue :y)
                           (deliver done true)))
      (is (= :y (threading/poll! queue 5000)))
      (is (true? (deref done bounded/wait-ms false)))))

  (testing "poll! with a timeout returns nil when nothing arrives"
    (let [queue (threading/make-rendezvous)
          start (now-ms)]
      (is (nil? (threading/poll! queue 100)))
      (is (<= 90 (- (now-ms) start)))))

  (testing "poll! accepts the infinite timeout and waits for the item"
    (let [queue (threading/make-rendezvous)]
      (threading/submit! @helper-executor
                         (fn []
                           (bounded/sleep-ms 100)
                           (threading/put! queue :z)))
      (is (= :z (bounded/bounded #(threading/poll! queue threading/infinite-ms))))))

  (testing "poll! clamps a timeout larger than an int"
    (let [queue (threading/make-rendezvous)]
      (threading/submit! @helper-executor
                         (fn []
                           (bounded/sleep-ms 100)
                           (threading/put! queue :big)))
      (is (= :big (bounded/bounded #(threading/poll! queue (bit-shift-left 1 40)))))))

  (testing "the 1-arity poll! returns nil unless a putter is waiting"
    (let [queue (threading/make-rendezvous)
          got (atom nil)]
      (is (nil? (threading/poll! queue)))
      (threading/submit! @helper-executor
                         (fn [] (threading/put! queue :w)))
      (is (true? (bounded/wait-until #(when-let [x (threading/poll! queue)]
                                        (reset! got x)))))
      (is (= :w @got))
      (is (nil? (threading/poll! queue)))))

  (testing "items from several putters each go to exactly one taker"
    (let [queue (threading/make-rendezvous)]
      (doseq [i (range 5)]
        (threading/submit! @helper-executor
                           (fn [] (threading/put! queue i))))
      (is (= #{0 1 2 3 4}
             (set (repeatedly 5 #(bounded/bounded (fn [] (threading/take! queue))))))))))

#?(:cljr
   (deftest rendezvous-poll-keeps-its-deadline-across-wake-ups-test
     (let [queue (threading/make-rendezvous)
           result (promise)
           start (now-ms)]
       (bounded/start-thread
        (fn []
          (deliver result [(threading/poll! queue 500) (- (now-ms) start)])))
       (bounded/sleep-ms 250)
       (locking (.lock ^Rendezvous queue)
         (System.Threading.Monitor/PulseAll (.lock ^Rendezvous queue)))
       (let [[item elapsed] (deref result bounded/wait-ms [:timeout nil])]
         (is (nil? item))
         (is (<= 450 elapsed))
         (is (< elapsed 700))))))

(deftest poll-and-take-on-both-kinds-test
  (testing "poll! and take! work on a queue and on a rendezvous"
    (let [queue (threading/make-queue)
          rendezvous (threading/make-rendezvous)]
      (threading/put! queue :q)
      (threading/submit! @helper-executor
                         (fn [] (threading/put! rendezvous :r)))
      (is (= :q (threading/take! queue)))
      (is (= :r (bounded/bounded #(threading/take! rendezvous)))))))

(deftest session-thread-test
  (let [release (promise)
        ran (promise)
        thread (threading/session-thread "threading-test-session"
                                         (fn []
                                           (deliver ran [(current-thread-name)
                                                         (current-thread-background?)])
                                           @release))]
    (bounded/sleep-ms 200)
    (is (not (realized? ran)))
    (threading/start-session-thread! thread)
    (is (= ["threading-test-session" true] (deref ran bounded/wait-ms :timeout)))
    (is (true? (threading/alive? thread)))
    (deliver release true)
    (is (true? (await-dead thread)))
    (is (false? (threading/alive? thread)))))

#?(:cljr
   (deftest session-thread-with-a-configured-stack-size-test
     (let [ran (promise)
           thread (with-redefs [config/config {:thread-opts {:max-stack-size 4194304}}]
                    (threading/session-thread "threading-test-stack"
                                              (fn [] (deliver ran true))))]
       (threading/start-session-thread! thread)
       (is (true? (deref ran bounded/wait-ms false))))))

(deftest session-thread-failure-does-not-end-the-run-test
  (let [ran (promise)
        thread (threading/session-thread "threading-test-failing-session"
                                         (fn []
                                           (deliver ran true)
                                           (throw (ex-info "threading-test failing session (expected)" {}))))]
    (threading/start-session-thread! thread)
    (is (true? (deref ran bounded/wait-ms false)))
    (is (true? (await-dead thread)))))

(deftest run-interruptible-test
  (let [thread (threading/session-thread "threading-test-unstarted" (fn []))]
    (testing "it returns the value of the body"
      (is (= 42 (threading/run-interruptible thread (+ 40 2)))))
    (testing "the body sees the caller's dynamic bindings"
      (binding [*print-length* 7]
        (is (= 7 (threading/run-interruptible thread *print-length*)))))
    (testing "an exception propagates unchanged"
      (let [e (ex-info "boom" {})
            caught (try (threading/run-interruptible thread (throw e))
                        (catch Exception c c))]
        (is (identical? e caught))))
    (testing "it runs the body on the calling thread"
      (is (= (current-thread-name)
             (threading/run-interruptible thread (current-thread-name)))))))

(deftest interrupt-stop-wakes-a-sleeping-thread-test
  (let [woke (promise)
        thread (threading/session-thread "threading-test-sleeper"
                                         (fn []
                                           (try (bounded/sleep-ms 30000)
                                                (catch #?(:clj InterruptedException :cljr ThreadInterruptedException) _
                                                  (deliver woke true)))))]
    (threading/start-session-thread! thread)
    (bounded/sleep-ms 100)
    (impl/interrupt-stop thread 60000)
    (is (true? (deref woke 2000 false)))))

(defn- interruptibly
  [sthread body]
  (threading/run-interruptible sthread (body)))

(defn- spinner
  [ran ready]
  (let [holder (promise)
        thread (threading/session-thread
                "threading-test-spinner"
                (fn []
                  (try (interruptibly @holder
                                      (fn []
                                        (try (deliver ready true)
                                             (loop [] (recur))
                                             (finally (reset! ran true)))))
                       (catch #?(:clj ThreadDeath :cljr Exception) _))))]
    (deliver holder thread)
    thread))

(when-not win?
  (deftest interrupt-stop-kills-a-spinning-thread-test
    (let [ran (atom false)
          ready (promise)
          thread (spinner ran ready)]
      (threading/start-session-thread! thread)
      (is (true? (deref ready bounded/wait-ms false)))
      (with-redefs [threading/force-stop-delay-ms 1000]
        (threading/interrupt-stop thread))
      (bounded/sleep-ms 300)
      (is (true? (threading/alive? thread)))
      (is (true? (await-dead thread)))
      (is (false? (threading/alive? thread)))
      (is (true? @ran)))))

(deftest force-interrupt-false-keeps-the-thread-test
  (let [stop (atom false)
        ready (promise)
        woke (promise)
        holder (promise)
        thread (threading/session-thread
                "threading-test-persistent"
                (fn []
                  (interruptibly @holder
                                 (fn []
                                   (deliver ready true)
                                   (try (bounded/sleep-ms 30000)
                                        (catch #?(:clj InterruptedException :cljr ThreadInterruptedException) _
                                          (deliver woke true)))
                                   (loop []
                                     (when-not @stop
                                       (recur)))))))]
    (deliver holder thread)
    (with-redefs [config/config {:force-interrupt false}]
      (threading/start-session-thread! thread)
      (is (true? (deref ready bounded/wait-ms false)))
      (impl/interrupt-stop thread 300)
      (is (true? (deref woke bounded/wait-ms false)))
      (bounded/sleep-ms 1200)
      (is (true? (threading/alive? thread))))
    (reset! stop true)
    (is (true? (await-dead thread)))))

(deftest stop-exception-test
  #?(:clj
     (do
       (is (true? (threading/stop-exception? (ThreadDeath.))))
       (is (true? (threading/stop-exception? (RuntimeException. "wrapped" (ThreadDeath.)))))
       (is (true? (threading/stop-exception? (Compiler$CompilerException. "f.clj" 1 1 (ThreadDeath.)))))
       (is (false? (threading/stop-exception? (InterruptedException.))))
       (is (false? (threading/stop-exception? (Compiler$CompilerException. "f.clj" 1 1 (InterruptedException.)))))
       (is (false? (threading/stop-exception? (RuntimeException. "plain")))))
     :cljr
     (let [abort (Activator/CreateInstance ThreadAbortException
                                           (enum-or BindingFlags/NonPublic BindingFlags/Instance)
                                           nil (object-array 0) nil)
           cancelled (OperationCanceledException. "cancelled")]
       (is (true? (threading/stop-exception? abort)))
       (is (true? (threading/stop-exception? cancelled)))
       (is (true? (threading/stop-exception? (System.Threading.Tasks.TaskCanceledException. "task"))))
       (is (true? (threading/stop-exception? (InvalidOperationException. "wrapped" abort))))
       (is (true? (threading/stop-exception? (InvalidOperationException. "wrapped" (InvalidOperationException. "deeper" cancelled)))))
       (is (true? (threading/stop-exception? (Compiler+CompilerException. "f.clj" 1 1 abort))))
       (is (false? (threading/stop-exception? (ThreadInterruptedException.))))
       (is (false? (threading/stop-exception? (Compiler+CompilerException. "f.clj" 1 1 (ThreadInterruptedException.)))))
       (is (false? (threading/stop-exception? (InvalidOperationException. "plain")))))))

#?(:cljr
   (deftest a-failing-task-is-logged-test
     (let [logged (atom [])
           ran (promise)]
       (with-redefs [misc/log (fn [& args] (swap! logged conj args))]
         (threading/submit! @helper-executor
                            (fn [] (throw (ex-info "logged failure" {}))))
         (threading/submit! @helper-executor
                            (fn [] (deliver ran true)))
         (deref ran bounded/wait-ms false)
         (bounded/wait-until #(seq @logged)))
       (is (= 1 (count @logged)))
       (is (= "logged failure" (ex-message (ffirst @logged)))))))

#?(:cljr
   (deftest a-cancelled-task-is-not-logged-test
     (let [logged (atom [])
           started (promise)
           queue (threading/make-queue)]
       (with-redefs [misc/log (fn [& args] (swap! logged conj args))]
         (let [handle (threading/submit! @helper-executor
                                         (fn []
                                           (deliver started true)
                                           (threading/take! queue)))]
           (deref started bounded/wait-ms false)
           (bounded/sleep-ms 100)
           (threading/cancel! handle)
           (bounded/sleep-ms 300)))
       (is (empty? @logged)))))

#?(:cljr
   (deftest an-interrupted-task-that-was-not-cancelled-is-logged-test
     (let [logged (atom [])
           started (promise)
           queue (threading/make-queue)
           thread (atom nil)]
       (with-redefs [misc/log (fn [& args] (swap! logged conj args))]
         (threading/submit! @helper-executor
                            (fn []
                              (reset! thread System.Threading.Thread/CurrentThread)
                              (deliver started true)
                              (threading/take! queue)))
         (deref started bounded/wait-ms false)
         (bounded/wait-until #(some? @thread))
         (bounded/sleep-ms 100)
         (bounded/interrupt-thread! @thread)
         (bounded/wait-until #(seq @logged)))
       (is (= 1 (count @logged)))
       (is (instance? ThreadInterruptedException (ffirst @logged))))))

#?(:cljr
   (deftest reaper-reinterrupts-a-wait-entered-since-test
     (let [logged (atom [])
           ready (promise)
           holder (promise)
           thread (threading/session-thread
                   "threading-test-second-wait"
                   (fn []
                     (interruptibly @holder
                                    (fn []
                                      (deliver ready true)
                                      (try (bounded/sleep-ms 30000)
                                           (catch ThreadInterruptedException _
                                             (bounded/sleep-ms 30000)))))))]
       (deliver holder thread)
       (with-redefs [misc/log (fn [& args] (swap! logged conj args))]
         (threading/start-session-thread! thread)
         (is (true? (deref ready bounded/wait-ms false)))
         (bounded/sleep-ms 100)
         (impl/interrupt-stop thread 300)
         (is (true? (await-dead thread)))
         (is (instance? ThreadInterruptedException (ffirst @logged)))))))

#?(:cljr
   (deftest forced-stop-test
     (testing "the body's own catch sees an abort stop-exception? recognises and cannot swallow it"
       (let [seen (atom nil)
             after (atom false)
             ready (promise)
             holder (promise)
             thread (threading/session-thread
                     "threading-test-forced"
                     (fn []
                       (interruptibly @holder
                                      (fn []
                                        (try (deliver ready true)
                                             (loop [] (recur))
                                             (catch Exception e
                                               (reset! seen e)))
                                        (reset! after true)))))]
         (deliver holder thread)
         (threading/start-session-thread! thread)
         (is (true? (deref ready bounded/wait-ms false)))
         (impl/interrupt-stop thread 300)
         (is (true? (await-dead thread)))
         (is (threading/stop-exception? @seen))
         (is (instance? ThreadAbortException @seen))
         (is (false? @after))))
     (testing "run-interruptible returns nil for a killed body and clears the cts slot"
       (let [result (atom :none)
             ready (promise)
             holder (promise)
             thread (threading/session-thread
                     "threading-test-forced-value"
                     (fn []
                       (reset! result [(interruptibly @holder
                                                      (fn []
                                                        (deliver ready true)
                                                        (loop [] (recur))))
                                       (nil? @(.cts-slot ^SessionThread @holder))])))]
         (deliver holder thread)
         (threading/start-session-thread! thread)
         (is (true? (deref ready bounded/wait-ms false)))
         (is (some? @(.cts-slot ^SessionThread thread)))
         (impl/interrupt-stop thread 300)
         (is (true? (await-dead thread)))
         (is (= [nil true] @result))))
     (testing "the slot is also cleared after a normal return and after an exception"
       (let [thread (threading/session-thread "threading-test-slot" (fn []))
             slot (.cts-slot ^SessionThread thread)]
         (threading/run-interruptible thread :ok)
         (is (nil? @slot))
         (try (threading/run-interruptible thread (throw (ex-info "x" {})))
              (catch Exception _))
         (is (nil? @slot))))
     (testing "a reaper that finds the body already finished cancels nothing"
       (let [thread (threading/session-thread "threading-test-late-reaper" (fn []))]
         (impl/interrupt-stop thread 100)
         (bounded/sleep-ms 400)
         (is (= :ok (threading/run-interruptible thread :ok)))))))

#?(:cljr
   (deftest cancel-without-a-task-test
     (is (false? (threading/cancelled?)))
     (is (nil? (threading/cancel-current!)))
     (is (false? (threading/cancelled?)))))
