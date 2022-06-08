(ns replacement.forms.parser.parse
  (:require [cljs.tools.reader.reader-types :as readers]
            [cljs.tools.reader :as reader]
            [cljs.spec.alpha :as s]
            [replacement.structure.form-specs :as form-specs]))

(defn text->edn-forms
  "Produce a sequentially ordered collection of edn forms, read from the given text."
  [text]
  (let [EOF    :EOF
        reader (readers/source-logging-push-back-reader text)]
    (reduce (fn [forms [form _]]
              (if (= form EOF)
                (reduced forms)
                (conj forms form)))
            [] (repeatedly #(reader/read+string {:eof EOF :read-cond :allow} reader)))))

(defn form->spec-formed
  "Obtain the conformed and unformed versions of the given form or explain-data for its non-conformance."
  [form]
  (let [pre-check (s/valid? ::form-specs/form form)
        conformed (and pre-check (s/conform ::form-specs/form form))]
    (cond-> {}
            pre-check (assoc :form form
                             :conformed conformed
                             :unformed (s/unform ::form-specs/form conformed))
            (not pre-check) (assoc :explain (s/explain-data ::form-specs/form form)))))

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

(def sample "(ns replacement.greet
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