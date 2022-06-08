(ns replacement.protocol.text-parsing
  #?(:cljs (:require [replacement.protocol.data :as spec-data]
                     [cljs.spec.alpha :as s]
                     [cljs.tools.reader.reader-types :as readers]
                     [cljs.tools.reader :as reader])
     :clj  (:require [replacement.protocol.data :as spec-data]
                     [clojure.spec.alpha :as s]))
  #?(:clj (:import (clojure.lang LineNumberingPushbackReader)
                   (java.io StringReader))))

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
   i.e. substituting true for :unknown if necessary."
  [& body]
  `(binding [*read-eval* (if (= :unknown *read-eval*) true *read-eval*)]
     ~@body))

(defn- push-back-reader
  [s]
  #?(:clj  (LineNumberingPushbackReader. (StringReader. s))
     :cljs (readers/source-logging-push-back-reader s)))

(defn string-reader
  [reader EOF]
  #?(:clj  (read+string {:eof EOF :read-cond :allow} reader)
     :cljs (reader/read+string {:eof EOF :read-cond :allow} reader)))

(defn text->edn-forms
  "Produce a sequentially ordered collection of edn forms, read from the given text.
  Throws on reader errors."
  [s]
  (let [EOF :EOF
        reader (push-back-reader s)]
    (reduce (fn [forms [form _]]
              (if (= form EOF)
                (reduced forms)
                (conj forms form)))
            [] (repeatedly #(with-read-known (string-reader reader EOF))))))

(defn form->spec-formed
  "Obtain the conformed and unformed versions of the given form or explain-data for its non-conformance."
  [form]
  (let [pre-check (s/valid? ::spec-data/form form)
        conformed (and pre-check (s/conform ::spec-data/form form))]
    (cond-> {}
            pre-check (assoc :form form
                             :conformed conformed
                             :unformed (s/unform ::spec-data/form conformed))
            (not pre-check) (assoc :explain (s/explain-data ::spec-data/form form)))))

(defn whole-ns->spec-form-data
  "Produce a list of maps with conformed and unformed versions or explain-data for the given forms."
  [forms]
  (map form->spec-formed forms))

(defn ns-reference-data
  [ns-name [type {:keys [ns-args] :as data}]]
  (let [var-name (:ns-name ns-args)]
    [ns-name var-name type data]))

(defn def-reference-data
  [ns-name [type {:keys [var-name] :as data}]]
  [ns-name var-name type data])

(defn defn-reference-data
  [ns-name [type {:keys [defn-args] :as data}]]
  (let [var-name (:fn-name defn-args)]
    [ns-name var-name type data]))

(defn- add-reference-data*
  [conformed-forms]
  (let [ns-name (-> conformed-forms first second (get-in [:ns-args :ns-name]))]
    [ns-name (mapv
               (fn [form]
                 (cond
                   (= :ns (first form)) (ns-reference-data ns-name form)
                   (= :def (first form)) (def-reference-data ns-name form)
                   (= :defn (first form)) (defn-reference-data ns-name form)
                   :else form))
               conformed-forms)]))

(defn add-reference-data
  [conformed-list]
  (let [ref-data (->> conformed-list
                      (map #(:conformed %))
                      (add-reference-data*))]
    ref-data))

(def project-sample
  {:name     "Politeness"
   :deps-etc :map-acceptable-to-tools.deps})

(def hello-sample "(ns replacement.greet
\"Hello World.\"
  (:require \n
    [cljs.spec.alpha :as s]
    [zprint.core :as zp :refer [zprint-file-str]]))

(def app \"repl-acement\")

(defn hello-world
  \"Welcome to repl-acement\"
  []
  \"Hello world\")

(defn greeting
  \"Have more interesting ways to greet\"
  {:api-version \"0.1.0\"}
  ([]
  (greeting \"you\"))
  ([name]
  {:pre [(string? name)]}
  (str \"Hello\" name))
  {:multi-arity-meta :valid-but-rarely-used})")

(def goodbye-sample "(ns replacement.goodbye
\"Goodbye Cruel World.\"
  (:require \n
    [cljs.spec.alpha :as s]
    [zprint.core :as zp :refer [zprint-file-str]]))

(def app \"repl-acement\")

(defn goodbye-world
  \"Welcome to repl-acement\"
  []
  \"Goodbye cruel world\")

(defn goodbye
  \"Have more interesting ways to bid adieu\"
  {:api-version \"0.1.0\"}
  ([]
  (goodbye \"you\"))
  ([name]
  {:pre [(string? name)]}
  (str \"Goodbye\" name))
  {:multi-arity-meta :valid-but-rarely-used})")