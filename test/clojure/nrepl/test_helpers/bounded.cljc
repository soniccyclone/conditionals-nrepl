(ns nrepl.test-helpers.bounded
  #?(:cljr (:import (System.Threading Thread ThreadStart))))

(def wait-ms 5000)

(defn spawn
  [f]
  #?(:clj (future (try {:value (f)}
                       (catch Throwable t {:error t})))
     :cljr (let [result (promise)
                 t (Thread. (gen-delegate ThreadStart []
                                          (deliver result (try {:value (f)}
                                                               (catch Exception e {:error e})))))]
             (set! (.IsBackground t) true)
             (.Start t)
             result)))

(defn await-result
  [h]
  (let [r (deref h wait-ms ::timeout)]
    (cond (= ::timeout r) ::timeout
          (:error r) (throw (:error r))
          :else (:value r))))

(defn bounded
  [f]
  (await-result (spawn f)))

(defn sleep-ms [n]
  (#?(:clj Thread/sleep :cljr Thread/Sleep) (int n)))

(defn wait-until
  [pred]
  (loop [n 0]
    (cond (pred) true
          (> n 1000) false
          :else (do (sleep-ms 5)
                    (recur (inc n))))))

(defn start-thread
  [f]
  #?(:clj (doto (Thread. ^Runnable f)
            (.setDaemon true)
            (.start))
     :cljr (let [t (Thread. (gen-delegate ThreadStart [] (f)))]
             (set! (.IsBackground t) true)
             (.Start t)
             t)))

(defn interrupt-thread!
  [t]
  (#?(:clj .interrupt :cljr .Interrupt) ^Thread t))

(defn thread-alive?
  [t]
  (#?(:clj .isAlive :cljr .IsAlive) ^Thread t))
