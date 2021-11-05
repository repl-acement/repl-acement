(ns replacement.forms.parser.parse
  (:require [cljs.tools.reader.reader-types :as readers]
            [cljs.tools.reader :as reader]
            [cljs.spec.alpha :as s]
            [replacement.structure.form-specs :as form-specs]))

(defn whole-ns
  [ns-text]
  (let [EOF    :EOF
        reader (readers/source-logging-push-back-reader ns-text)]
    (reduce (fn [forms [form _]]
              (if (= form EOF)
                (reduced forms)
                (conj forms form)))
            [] (repeatedly #(reader/read+string {:eof EOF :read-cond :allow} reader)))))

(defn parse-vars
  [forms]
  (map #(s/conform ::form-specs/form %) forms))

(defn enrich-def
  [ns-name [type {:keys [var-name] :as data}]]
  [ns-name var-name type data])

(defn enrich-defn
  [ns-name [type {:keys [defn-args] :as data}]]
  (let [var-name (:fn-name defn-args)]
    [ns-name var-name type data]))

(defn- enrich*
  [{:keys [ns-args]} conformed-forms]
  (let [ns-name (:ns-name ns-args)]
    [ns-name (mapv
               (fn [form]
                 (cond
                   (= :def (first form)) (enrich-def ns-name form)
                   (= :defn (first form)) (enrich-defn ns-name form)
                   :else form))
               conformed-forms)]))

(defn enrich
  [conformed-list]
  (enrich* (-> conformed-list first last)
           (rest conformed-list)))

(def sample "(ns repl.ace.ment)

(def themes {:day   :light
             :night :dark})

(def theme (atom :day))

(defn set-theme
  [new-theme]
  (swap! theme new-theme))

(defn apply-theme
  ([output]
   (apply-theme output :terminal))
  ([output device]
   (str \" To be implemented \" output \" for \" device)))")

