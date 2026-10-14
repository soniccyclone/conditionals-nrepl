(ns nrepl.sanity-test
  (:require
   [clojure.test :refer :all]
   [nrepl.core :as nrepl]
   [nrepl.middleware.print :as print]
   [nrepl.test-helpers :as th]
   [nrepl.transport :as transport :refer [piped-transports]]))

(deftest repl-out-writer
  (let [[local remote] (piped-transports)
        w (print/replying-PrintWriter :out {:transport remote} {})]
    #?(:clj
       (do
         (doto w
           .flush
           (.println "println")
           (.write "abcd")
           (.write (.toCharArray "ef") 0 2)
           (.write "gh" 0 2)
           (.write (.toCharArray "ij"))
           (.write "   klm" 5 1)
           (.write 32)
           .flush)
         (with-open [out (java.io.PrintWriter. w)]
           (binding [*out* out]
             (newline)
             (prn #{})
             (flush))))
       :cljr
       (let [^System.IO.TextWriter w w]
         (doto w
           .Flush
           (.WriteLine "println")
           (.Write "abcd")
           (.Write (.ToCharArray "ef") (int 0) (int 2))
           (.Write "gh")
           (.Write (.ToCharArray "ij"))
           (.Write "m")
           (.Write (char 32))
           .Flush)
         (binding [*out* w]
           (newline)
           (prn #{})
           (flush))
         (.Dispose w)))

    (is (= #?(:clj [(str "println" th/sys-newline)
                    "abcdefghijm "
                    (th/newline->sys "\n#{}\n")]
              :cljr [(str "println" th/sys-newline)
                     "abcdefghijm "
                     th/sys-newline
                     (th/newline->sys "#{}\n")])
           (->> (nrepl/response-seq local 0)
                (map :out))))))
;; ci49 scratch
