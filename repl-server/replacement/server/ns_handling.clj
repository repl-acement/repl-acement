(ns replacement.server.ns-handling
  (:require [clojure.core.async :as a]
            [clojure.tools.namespace.parse :as ns-parse]
            [clojure.tools.namespace.find :as ns-find]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.java.classpath :as classpath]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [replacement.server.async-prepl :as async-prepl]
            [replacement.specs.user :as user-specs])
  (:import (java.io StringReader File PushbackReader)
           (java.util.jar JarFile)))

(set! *warn-on-reflection* true)
(set! *default-data-reader-fn* tagged-literal)

(defn get-deps [form]
  (when (ns-parse/ns-decl? form)
    (ns-parse/deps-from-ns-decl form)))

(defn ns-deps
  ([ns-name classpath-data]
   (ns-deps 0 ns-name classpath-data))
  ([level ns-name classpath-data]
   (reduce
     (fn [results dep]
       (let [[level] results
             next-level (inc level)]
         (conj results [next-level dep (ns-deps next-level dep classpath-data)])))
     [level] (-> ns-name classpath-data :decl-ns get-deps))))

(defn ns-string->ns-decl
  "Obtain the ns declaration from a given ns as an unevaluated form."
  [s]
  (with-open [reader (PushbackReader. (StringReader. s))]
    (ns-parse/read-ns-decl reader)))

;; borrowed from c.t.n.file
(defmacro ^:private ignore-reader-exception
  "If body throws an exception caused by a syntax error (from
  tools.reader), returns nil. Rethrows other exceptions."
  [& body]
  `(try ~@body
        (catch Exception e#
          (if (= :reader-exception (:type (ex-data e#)))
            nil
            (throw e#)))))

(defn- read-stream*
  ([rdr]
   (read-stream* rdr nil))
  ([rdr read-opts]
   (let [EOF (Object.)
         opts (assoc (or read-opts ns-parse/clj-read-opts) :eof EOF)]
     (ignore-reader-exception
       (loop [content []]
         (let [form (reader/read opts rdr)]
           (if (identical? EOF form)
             content
             (recur (conj content form)))))))))

(defn read-stream
  [stream read-opts]
  (with-open [rdr (-> stream
                      (io/reader)
                      (PushbackReader.))]
    (read-stream* rdr read-opts)))

(defn read-jarfile-entry
  ([jarfile entry-name]
   (read-jarfile-entry jarfile entry-name nil))
  ([^JarFile jarfile ^String entry-name platform]
   (let [{:keys [read-opts]} (or platform ns-find/clj)
         text (->> (.getEntry jarfile entry-name)
                   (.getInputStream jarfile)
                   (slurp))
         stream (->> (.getEntry jarfile entry-name)
                     (.getInputStream jarfile))]
     {:text  text
      :forms (read-stream stream read-opts)})))

(defn read-dir-entry
  ([file]
   (read-dir-entry file nil))
  ([^File file platform]
   (let [{:keys [read-opts]} (or platform ns-find/clj)
         text (slurp file)]
     {:text  text
      :forms (read-stream file read-opts)})))

(defn jar+ns-decls [jarfile]
  "Produce mapping from ns name to source and from source to ns name for entries in the given JAR"
  (reduce
    (fn [ns-decls source-location]
      (let [decl-ns (not-empty (ns-find/read-ns-decl-from-jarfile-entry jarfile source-location))
            name-ns (and decl-ns (ns-parse/name-from-ns-decl decl-ns))
            text+forms (and name-ns (binding [*ns* name-ns]
                                      (read-jarfile-entry jarfile source-location)))
            coords {:source-location source-location
                    :source-type     :jar-entry
                    :jar-file        jarfile
                    :decl-ns         decl-ns
                    :name-ns         name-ns}]
        (cond-> ns-decls
                (and decl-ns name-ns) (assoc name-ns (merge coords text+forms)
                                             source-location {:decl-ns     decl-ns
                                                              :source-type :jar-entry
                                                              :name-ns     name-ns}))))
    {} (ns-find/sources-in-jar jarfile)))

(defn jar-data
  [jars]
  (reduce
    (fn [ns-decls jar]
      (let [jarfile (JarFile. ^File jar)
            decls (not-empty (jar+ns-decls jarfile))]
        (cond-> (merge ns-decls decls)
                decls (assoc jar (jar+ns-decls jarfile)))))
    {} jars))

(defn dir-files+ns-decls
  [dir]
  "Produce mapping from ns name to source and from source to ns
  name for files below the given directory"
  (reduce
    (fn [ns-decls source-location]
      (let [decl-ns (not-empty (ns-file/read-file-ns-decl source-location))
            name-ns (and decl-ns (ns-parse/name-from-ns-decl decl-ns))
            text+forms (and name-ns (binding [*ns* name-ns]
                                      (read-dir-entry source-location)))
            coords {:source-location source-location
                    :source-type     :file
                    :dir             dir
                    :decl-ns         decl-ns
                    :name-ns         name-ns}]
        (cond-> ns-decls
                decl-ns (assoc name-ns (merge coords text+forms)
                               source-location {:decl-ns     decl-ns
                                                :source-type :file
                                                :name-ns     name-ns}))))
    {} (ns-find/find-sources-in-dir dir)))

(defn dir-data
  [dirs]
  (reduce (fn [ns-decls dir]
            (let [decls (not-empty (dir-files+ns-decls dir))]
              (cond-> (merge ns-decls decls)
                      decls (assoc dir decls))))
          {} dirs))

(defn require-fail?
  "Attempt to require the ns, will usually work if it's on the CLASSPATH,
  will return true if the prepl raises an exception"
  [prepl-opts user ns-x]
  (boolean (some-> prepl-opts
                   (async-prepl/sync-eval-forms {:forms [(require ns-x)]
                                                 :user  user})
                   not-empty
                   first
                   :exception)))

(defn trace-deps
  [nses {:keys [dep-fn apply-all] :or {dep-fn    prn
                                       apply-all false}}]
  (let [seen (atom #{})
        dep-node-count (atom 0)]
    (clojure.walk/postwalk
      (fn [x]
        (when-let [sym-name (and (vector? x) (symbol? (second x)) (second x))]
          (swap! dep-node-count inc)
          (if-not (@seen sym-name)
            (do (swap! seen conj sym-name)
                (dep-fn sym-name))
            (if apply-all
              (dep-fn sym-name))))
        x)
      nses)
    {:seen      (count @seen)
     :dep-nodes @dep-node-count}))

(defn eval-deps!
  [an-ns classpath-data prepl-opts user]
  (letfn [(dep-fn [ns-sym]
            (when (require-fail? prepl-opts user ns-sym)
              (let [{:keys [name-ns forms]} (classpath-data ns-sym)]
                (async-prepl/sync-eval-forms prepl-opts {:forms   forms
                                                         :name-ns name-ns
                                                         :user    user}))))]
    (let [nses (ns-deps an-ns classpath-data)]
      (trace-deps nses {:dep-fn dep-fn}))))

(defn eval-ns!
  [an-ns classpath-data prepl-opts user]
  (prn :fail? (require-fail? prepl-opts user an-ns))
  (when (require-fail? prepl-opts user an-ns)
    (let [{:keys [name-ns forms] :as ns-stuff} (classpath-data an-ns)]
      (prn :ns-stuff ns-stuff)
      (eval-deps! an-ns classpath-data prepl-opts user)
      (async-prepl/sync-eval-forms prepl-opts {:forms   forms
                                               :name-ns name-ns
                                               :user    user}))))

(def system-user (user-specs/->user "system" "0"))


(comment

  ;; TODO NEXT
  ;; need to test loading stuff that is not on the CLASSPATH but requires
  ;; stuff that is on the CLASSPATH eg nses from the DB

  ;; DONE

  ;; Obtain data from nses on the CLASSPATH
  (def classpath-data
    (let [cp (classpath/classpath)
          cp-jars (filter classpath/jar-file? cp)
          cp-dirs (filter #(-> (.isDirectory ^File %)) cp)]
      (merge (jar-data cp-jars)
             (dir-data cp-dirs))))

  (defn eval-with-prepl
    [prepl-opts form]
    (async-prepl/sync-eval-forms prepl-opts
                                 {:forms [form]
                                  :user  "ray"}))

  (def example-ns 'clojure.core.async)

  (def prepl-chan (a/chan (a/buffer 8092)))
  (def prepl-opts (atom (async-prepl/init-prepl {:out-ch prepl-chan})))

  ;; Can Eval an ns
  (time (eval-ns! example-ns classpath-data @prepl-opts system-user))

  ;; Can Eval deps of an NS if needed
  (time (eval-deps! example-ns classpath-data @prepl-opts system-user))


  ;; end comment
  )

;;;; Q: is each path safe to be evaluated in parallel?

;; add a pipeline function to make a small change

;;;; eg to add #trace from flow-storm

;; simulate changes from the editor
;;;; eg to add a new outbound call to a function in this ns
;;;; eg to add a new outbound call to a function in a required ns
;;;; eg to add a new outbound call to a function in a new ns
;;;; eg to add a new outbound call to a function in new library ns

;; reload the changes
;;;; just reload all initially and lose state
;;;; need an in memory / DB version of ns-repl/refresh-all

;;;; Add datafy / nav to the code AST once the DB graph is designed

