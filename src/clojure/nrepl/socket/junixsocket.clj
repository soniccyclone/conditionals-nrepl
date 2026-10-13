(ns nrepl.socket.junixsocket
  "Should only be required if junixsocket is present on the classpath."
  (:import
   (java.io File)
   (java.net BindException StandardSocketOptions)
   (java.nio.file Files LinkOption)
   (org.newsclub.net.unix AFUNIXServerSocketChannel AFUNIXSocketAddress
                          AFUNIXSocketChannel)))

(defn- path-taken?
  [^File file]
  (Files/exists (.toPath file) (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))

(defn unix-server-socket
  "Return a server filesystem socket bound to the given path. Throws a
  BindException if anything exists at the path."
  [^String path]
  (when (path-taken? (File. path))
    (throw (BindException. "Address already in use")))
  (let [addr (AFUNIXSocketAddress/of (File. path))
        ^AFUNIXServerSocketChannel socket-chan (AFUNIXServerSocketChannel/open)]
    (.setOption socket-chan StandardSocketOptions/SO_REUSEADDR false)
    (.bind socket-chan addr)
    (.deleteOnExit (File. (.getPath addr)))
    socket-chan))

(defn unix-client-socket
  "Returns a client filesystem socket bound to the given path."
  [^String path]
  (let [addr (AFUNIXSocketAddress/of (File. path))
        socket-chan (AFUNIXSocketChannel/open)]
    (.connect socket-chan addr)
    socket-chan))

(defn get-socket-address-path
  "Given a socket address instance, return its filesystem path as a string."
  [^AFUNIXSocketAddress socket-addr]
  (.getPath socket-addr))
