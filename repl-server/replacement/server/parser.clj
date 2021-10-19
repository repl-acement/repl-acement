(ns replacement.server.parser
  (:require [clojure.spec.alpha :as spec]
            [clojure.core.specs.alpha :as core-specs]
            [replacement.server.async-prepl :as ap]))

(spec/def ::ns-form
  (spec/cat
    :ns-sym (spec/and symbol? #(= 'ns %))
    :ns-args ::core-specs/ns-form))

(spec/def ::defn
  (spec/cat
    :defn-type (spec/and symbol? #(or (= 'defn %)
                                      (= 'defn- %)))
    :defn-args ::core-specs/defn-args))

(spec/def ::def
  (spec/cat
    :def (spec/and symbol? #(= 'def %))
    :var-name symbol?
    :docstring (spec/? string?)
    :init-expr (spec/? any?)))

(spec/def ::form
  (spec/or :ns ::ns-form
           :def ::def
           :defn ::defn
           :expr list?))

(defn add-metadata
  [ns-name var-name]
  (let [ns-thing (the-ns (symbol ns-name))]
    (binding [*ns* ns-thing]
      (meta (resolve (symbol var-name))))))

(defn enrich-var
  [ns-name var-name [type data]]
  (let [enriched-data (add-metadata ns-name var-name)]
    [type (assoc data :metadata enriched-data)]))

(defn enrich-def
  [ns-name [type {:keys [var-name] :as data}]]
  (let [var-name var-name]
    (enrich-var ns-name var-name [type data])))

(defn enrich-defn
  [ns-name [type {:keys [defn-args] :as data}]]
  (let [var-name (:fn-name defn-args)]
    (enrich-var ns-name var-name [type data])))

(defn enrich
  [{:keys [ns-args]} conformed-forms]
  (let [ns-name (:ns-name ns-args)]
    (mapv
      (fn [form]
        (cond
          (= :def (first form)) (enrich-def ns-name form)
          (= :defn (first form)) (enrich-defn ns-name form)
          :else form))
      conformed-forms)))

(comment

  (let [forms (->> "repl-server/replacement/server/async_prepl.clj"
                   slurp
                   ap/message->forms
                   (map #(spec/conform ::form %)))]
    (enrich (-> forms first last)
            (rest forms)))

  )

