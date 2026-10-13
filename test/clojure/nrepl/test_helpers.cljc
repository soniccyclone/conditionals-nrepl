(ns nrepl.test-helpers
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [nrepl.core :as nrepl]
            #?(:clj [matcher-combinators.test]
               :cljr [nrepl.test-helpers.match]))
  (:import
   #?@(:clj [(com.hypirion.io ClosingPipe Pipe)
             (java.io File)
             (java.net ServerSocket SocketException)
             (java.nio.file Files)
             (java.nio.file.attribute FileAttribute)
             (java.util.concurrent TimeUnit)]
       :cljr [(System.Diagnostics Process ProcessStartInfo)
              (System.IO Directory File FileInfo IOException Path)
              (System.Net IPAddress IPEndPoint)
              (System.Net.Sockets SocketError SocketException TcpListener)
              (System.Runtime.InteropServices OSPlatform RuntimeInformation)
              (System.Threading Thread ThreadStart)])))

(def sys-newline
  "The system's newline character sequence."
  #?(:clj (System/lineSeparator)
     :cljr Environment/NewLine))

(def win?
  "Whether we are running on MS-Windows."
  #?(:clj (.startsWith (System/getProperty "os.name") "Windows")
     :cljr (RuntimeInformation/IsOSPlatform OSPlatform/Windows)))

(defn string=
  "Like `clojure.core/=` applied on STRING1 and STRING2, but treats
  all line endings as equal."
  [string1 string2]
  (= (str/split-lines string1) (str/split-lines string2)))

(defn newline->sys
  "Returns TEXT, converting `\n` line endings to those of the underlying
  system's, if different."
  [text]
  (if-not (= sys-newline "\n")
    (str/replace text "\n" sys-newline)
    text))

(def sys-file-sep
  "The character separating components in a path."
  #?(:clj File/separator
     :cljr (str Path/DirectorySeparatorChar)))

(defn dir-sep->sys
  "Replace the `/` separator component used in tests with the one used
  by the system."
  [path]
  (if-not (= sys-file-sep "/")
    (str/replace path "/" sys-file-sep)
    path))

#?(:clj
   (defn sh
     "A version of clojure.java.shell/sh that streams in/out/err.
  Taken and edited from https://github.com/technomancy/leiningen/blob/f7e1adad6ff5137d6ea56bc429c3b620c6f84128/leiningen-core/src/leiningen/core/eval.clj"
     ^Process
     [cmd]
     (let [proc (.exec (Runtime/getRuntime) ^"[Ljava.lang.String;" (into-array String cmd))]
       (future (with-open [out (.getInputStream proc)
                           err (.getErrorStream proc)
                           in (.getOutputStream proc)]
                 (let [_pump-out (doto (Pipe. out System/out) .start)
                       _pump-err (doto (Pipe. err System/err) .start)
                       _pump-in (ClosingPipe. System/in in)]
                   (.waitFor proc))))
       proc))
   :cljr
   (defn sh
     "Starts the command CMD (a vector of strings) as a child process that
  inherits this process's standard streams."
     ^Process
     [cmd]
     (let [info (ProcessStartInfo. ^String (first cmd))]
       (set! (.UseShellExecute info) false)
       (doseq [arg (rest cmd)]
         (.Add (.ArgumentList info) (str arg)))
       (Process/Start info))))

#?(:clj
   (defn eval-in-jvm
     [props code timeout-ms]
     (let [java (str (System/getProperty "java.home") File/separator "bin" File/separator "java")
           cmd  (concat [java]
                        (map (fn [[k v]] (str "-D" k "=" v)) props)
                        ["-cp" (System/getProperty "java.class.path")
                         "clojure.main" "-"])
           proc (-> (ProcessBuilder. ^java.util.List (vec cmd))
                    (.redirectErrorStream true)
                    (.start))
           out  (future (slurp (.getInputStream proc)))]
       (with-open [in (.getOutputStream proc)]
         (.write in (.getBytes ^String code "UTF-8")))
       (if (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)
         {:exit (.exitValue proc), :out @out}
         (do (.destroyForcibly proc)
             {:exit nil, :out (deref out 1000 "")})))))

(defn destroy!
  "Kills the process PROC started by `sh`."
  [^Process proc]
  #?(:clj (.destroy proc)
     :cljr (when-not (.HasExited proc)
             (.Kill proc true))))

(defmacro with-process [[binding cmd-vector] & body]
  (let [sym (gensym)]
    `(let [~sym (sh ~cmd-vector)
           ~binding ~sym]
       (try ~@body
            (finally (destroy! ~binding))))))

#?(:cljr
   (defn run-with-timeout
     [ms f]
     (let [f (bound-fn* f)
           result (promise)
           thread (Thread. (gen-delegate ThreadStart []
                                         (deliver result
                                                  (try {:value (f)}
                                                       (catch Exception e {:error e})))))]
       (set! (.IsBackground thread) true)
       (.Start thread)
       (let [r (deref result ms ::timeout)]
         (cond
           (identical? r ::timeout)
           (throw (ex-info (str "Timed out after " ms "ms") {:timeout-ms ms}))

           (contains? r :error) (throw (:error r))
           :else (:value r))))))

(defmacro with-timeout
  "Runs BODY on another thread and gives up after MS milliseconds, throwing
  instead of hanging the whole test run. For code that blocks on responses
  with no timeout of its own, like the built-in client."
  [ms & body]
  #?(:clj `(let [f# (future ~@body)
                 v# (deref f# ~ms ::timeout)]
             (if (identical? v# ::timeout)
               (do (future-cancel f#)
                   (throw (ex-info (str "Timed out after " ~ms "ms") {:timeout-ms ~ms})))
               v#))
     :cljr `(run-with-timeout ~ms (fn [] ~@body))))

(defn free-port []
  #?(:clj (let [sock (ServerSocket. 0)
                port (.getLocalPort sock)]
            (.close sock)
            port)
     :cljr (let [listener (TcpListener. IPAddress/Loopback (int 0))]
             (.Start listener)
             (let [port (.Port ^IPEndPoint (.LocalEndpoint listener))]
               (.Stop listener)
               port))))

(defn sleep
  "Blocks the calling thread for MS milliseconds."
  [ms]
  #?(:clj (Thread/sleep (long ms))
     :cljr (Thread/Sleep (int ms))))

(defn current-thread-name
  "The name of the calling thread."
  []
  #?(:clj (.getName (Thread/currentThread))
     :cljr (.Name ^Thread Thread/CurrentThread)))

(defn now-ms
  "Milliseconds since the epoch."
  []
  #?(:clj (System/currentTimeMillis)
     :cljr (.ToUnixTimeMilliseconds ^DateTimeOffset DateTimeOffset/UtcNow)))

(defn disconnected?
  "Whether E is what a transport or client throws once its peer has gone away."
  [e]
  #?(:clj (instance? SocketException e)
     :cljr (or (instance? IOException e)
               (instance? SocketException e))))

(defn connection-refused?
  "Whether E is what connecting to a port nobody listens on throws."
  [e]
  #?(:clj (instance? java.net.ConnectException e)
     :cljr (loop [e e]
             (cond
               (nil? e) false
               (and (instance? SocketException e)
                    (= SocketError/ConnectionRefused (.SocketErrorCode ^SocketException e))) true
               :else (recur (.InnerException ^Exception e))))))

(defn close!
  "Closes the Closeable (JVM) or IDisposable (CLR) X."
  [x]
  #?(:clj (.close ^java.io.Closeable x)
     :cljr (.Dispose ^IDisposable x)))

(defmacro thrown-with?
  "True when BODY throws an exception for which the predicate PRED holds."
  [pred & body]
  `(try ~@body
        false
        (catch ~'#?(:clj Throwable :cljr Exception) e# (boolean (~pred e#)))))

(defn project-file
  "The file PATH, relative to the project's base directory, in a form `slurp`
  accepts."
  [path]
  #?(:clj (File. (File. (System/getProperty "nrepl.basedir" ".")) ^String path)
     :cljr (Path/Combine Environment/CurrentDirectory ^String path)))

(defn as-file
  "A file object for PATH of the host's own type, as source metadata can carry."
  [path]
  #?(:clj (File. ^String path)
     :cljr (FileInfo. ^String path)))

(defn make-temp-dir
  "Creates an empty directory with a short path under the system's temporary
  directory (a unix socket path is limited to about 100 characters) and
  returns its path."
  [prefix]
  #?(:clj (str (Files/createTempDirectory ^String prefix (into-array FileAttribute [])))
     :cljr (let [dir (Path/Combine (Path/GetTempPath)
                                   (str prefix (subs (str (Guid/NewGuid)) 0 8)))]
             (Directory/CreateDirectory dir)
             dir)))

(defn path-join
  "The path of the entry NAME inside the directory DIR."
  [dir name]
  (str dir sys-file-sep name))

(defn path-exists?
  "Whether a file or directory exists at PATH."
  [path]
  #?(:clj (.exists (File. ^String path))
     :cljr (or (File/Exists ^String path) (Directory/Exists ^String path))))

(defn delete-recursively!
  "Deletes the file or directory at PATH and everything below it, if it exists."
  [path]
  #?(:clj (let [f (File. ^String path)]
            (when (.isDirectory f)
              (doseq [^File child (.listFiles f)]
                (delete-recursively! (.getPath child))))
            (.delete f))
     :cljr (cond
             (Directory/Exists ^String path) (Directory/Delete ^String path true)
             (File/Exists ^String path) (File/Delete ^String path))))

;; nrepl-specific helpers

(defn eval-value1
  "Transform `code-form` into a string, send to `client-or-session` via `eval` op,
  read and combine the response and return the first successful value."
  [client-or-session code-form]
  (-> (nrepl/message client-or-session {:op "eval", :code (nrepl/code* code-form)})
      nrepl/response-values
      first))

;; matcher-combinators.test is needed for `match?`

(defmacro is+
  "Like `is` but wraps expected value in matcher-combinators's `match?`."
  ([expected actual]
   `(is+ ~expected ~actual nil))
  ([expected actual message]
   `(is (~'match? ~expected ~actual) ~message)))
