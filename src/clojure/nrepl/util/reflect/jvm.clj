(ns nrepl.util.reflect.jvm
  "internal host seam"
  {:no-doc true}
  (:require
   [clojure.java.io :as io])
  (:import
   (java.io File)
   (java.lang.ref Reference)
   (java.lang.reflect Field Member Modifier)
   (java.nio.file Files)
   (java.util HashMap Map)
   (java.util.function Function Predicate)
   (java.util.jar JarEntry JarFile)
   (java.util.stream Collectors)))

(defn resolve-class
  [ns sym]
  (let [v (ns-resolve ns sym)]
    (when (class? v)
      v)))

(defn class-name
  [^Class c]
  (.getName c))

(defn simple-name
  [^Class c]
  (.getSimpleName c))

(defn package-name
  [^Class c]
  (some-> (.getPackage c) .getName))

(defn- member-map
  [^Member m kind]
  {:name (.getName m)
   :kind kind
   :static? (Modifier/isStatic (.getModifiers m))
   :declaring-class (.getDeclaringClass m)
   :member m})

(defn members
  [^Class c]
  (concat (map #(member-map % :method) (.getMethods c))
          (map #(member-map % :field) (.getFields c))
          (map #(assoc (member-map % :constructor) :name "new")
               (.getConstructors c))))

(defn deftype-field?
  [member ^Class c]
  (and (= :field (:kind member))
       (.isAssignableFrom clojure.lang.IType c)))

(defn- classpath-strings
  []
  (into []
        (keep #(System/getProperty %))
        ["sun.boot.class.path" "java.ext.dirs" "java.class.path" "fake.class.path"]))

(defn cache-key
  []
  (reduce hash-combine 0 (classpath-strings)))

(defn- classpath
  "Returns a sequence of File objects of the elements on the classpath."
  []
  (mapcat #(.split ^String % File/pathSeparator) (classpath-strings)))

(defn- file-seq-nonr
  "A tree seq on java.io.Files, doesn't resolve symlinked directories to avoid\n  infinite sequence resulting from recursive symlinked directories."
  [dir]
  (tree-seq (fn [^File f] (and (.isDirectory f) (not (Files/isSymbolicLink (.toPath f)))))
            (fn [^File d] (seq (.listFiles d)))
            dir))

(defn- list-files
  "Given a path (either a jar file, directory with classes or directory with\n  paths) returns all files under that path."
  [^String path scan-jars?]
  (cond (.endsWith path "/*") (for [^File jar (.listFiles (File. path))
                                    :when (.endsWith (.getName jar) ".jar")
                                    file (list-files (.getPath jar) scan-jars?)]
                                file)
        (.endsWith path ".jar")
        (if scan-jars?
          (try (-> (.stream (JarFile. path))
                   (.filter (reify
                              Predicate
                              (test [_ entry] (not (.isDirectory ^JarEntry entry)))))
                   (.map (reify
                           Function
                           (apply [_ entry] (.getName ^JarEntry entry))))
                   (.collect (Collectors/toList)))
               (catch Exception _))
          ())
        (= path "") ()
        (.exists (File. path))
        (let [root (File. path)
              root-path (.toPath root)]
          (for [^File file (file-seq-nonr root)
                :when (not (.isDirectory file))]
            (let [filename (str (.relativize root-path (.toPath file)))]
              (cond-> filename
                (not= File/separator "/") (.replace File/separator "/")
                (.startsWith filename "/") (.substring filename 1)))))))

(defmacro ^:private list-jdk9-base-classfiles
  "Because on JDK9+ the classfiles are stored not in rt.jar on classpath, but in\n  modules, we have to do extra work to extract them."
  []
  (when (resolve-class *ns* 'java.lang.module.ModuleFinder)
    `(-> (.findAll (java.lang.module.ModuleFinder/ofSystem))
         (.stream)
         (.flatMap (reify Function
                     (apply [_ mref#]
                       (.list (.open ^java.lang.module.ModuleReference mref#)))))
         (.collect (Collectors/toList)))))

(defn classpath-files
  "Given a list of files on the classpath, returns the list of all files,\n  including those located inside jar files."
  []
  (let [seen (HashMap.)
        seen? (fn [x] (.putIfAbsent seen x x))]
    (-> []
        (into (comp (map #(list-files % true)) cat (remove seen?)) (classpath))
        (into (remove seen?) (list-jdk9-base-classfiles)))))

(def ^:private keywords-table
  (delay (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
           (.setAccessible field true)
           (.get field nil))))

(defn all-keywords
  []
  (keep (fn [^Reference r] (.get r))
        (.values ^Map @keywords-table)))

(defn resource-path
  [s]
  (some-> (io/resource s) str))
