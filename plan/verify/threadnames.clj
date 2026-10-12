(require '[clojure.string :as str]
         '[nrepl.core :as nrepl]
         '[nrepl.server :as server])

(defn normalise [n]
  (-> n
      (str/replace #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" "<uuid>")
      (str/replace #"\d+$" "N")))

(with-open [srv (server/start-server)
            conn (nrepl/connect :port (:port srv))]
  (let [client (nrepl/client conn 5000)
        sess (nrepl/client-session client)]
    (doall (nrepl/message sess {:op "eval" :code "(+ 1 1)"}))
    (doall (nrepl/message client {:op "eval" :code "(+ 2 2)"}))
    (Thread/sleep 200)
    (->> (keys (Thread/getAllStackTraces))
         (map #(.getName ^Thread %))
         (filter #(str/starts-with? % "nREPL"))
         (map normalise)
         frequencies
         (into (sorted-map))
         prn)))
(shutdown-agents)
