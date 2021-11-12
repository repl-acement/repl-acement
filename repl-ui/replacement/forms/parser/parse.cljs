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
  (map (fn [form]
         (let [conformed (s/conform ::form-specs/form form)
               unformed  (s/unform ::form-specs/form conformed)]
           {:conformed conformed
            :unformed  unformed}))
       forms))

(defn enrich-def
  [ns-name [type {:keys [var-name] :as data}]]
  [ns-name var-name type data])

(defn enrich-defn
  [ns-name [type {:keys [defn-args] :as data}]]
  (let [var-name (:fn-name defn-args)]
    [ns-name var-name type data]))

(defn- enrich*
  [conformed-forms]
  (let [ns-name (-> conformed-forms first second (get-in [:ns-args :ns-name]))]
    [ns-name (mapv
               (fn [form]
                 (cond
                   (= :def (first form)) (enrich-def ns-name form)
                   (= :defn (first form)) (enrich-defn ns-name form)
                   :else form))
               (rest conformed-forms))]))

(defn enrich
  [conformed-list]
  (->> conformed-list
       (map #(:conformed %))
       (enrich*)))

(def sample "(ns repl.ace.ment)

(def themes {:day   :light
             :night :dark})

(def theme (atom :day))

(defn set-theme
  \"Set the default theme\"
  [new-theme]
  {:pre [(themes new-theme)]}
  (swap! theme new-theme))

(defn apply-theme
  {:api-version \"0.1.0\"
   :stub true}
  [output device]
  (str \" To be implemented \" output \" for \" device))")

