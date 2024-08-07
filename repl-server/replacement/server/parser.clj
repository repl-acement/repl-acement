(ns replacement.server.parser
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as core-specs]
            [replacement.protocol.events :as events-specs]
            [replacement.protocol.text-parsing :as text-parsing]
            [replacement.server.async-prepl :as ap]))

(set! *warn-on-reflection* true)

(s/def ::ns-form
  (s/cat
    :ns-sym ::events-specs/ns-sym
    :ns-args ::core-specs/ns-form))

(s/def ::defn
  (s/cat
    :defn-type ::events-specs/defn-sym
    :defn-args ::core-specs/defn-args))

(s/def ::def
  (s/cat
    :def ::events-specs/def-sym
    :var-name symbol?
    :docstring (s/? string?)
    :init-expr (s/? any?)))

(s/def ::form
  (s/or :ns ::ns-form
        :def ::def
        :defn ::defn
        :expr list?))

(s/def ::core-specs/seq-binding-form
  (s/and vector?
         (s/conformer identity vec)
         (s/cat :elems (s/* ::core-specs/binding-form)
                :rest (s/? (s/cat :amp #{'&} :form ::core-specs/binding-form))
                :as (s/? (s/cat :as #{:as} :sym ::core-specs/local-name)))))

(defn arg-list-unformer [a]
  (vec
    (if (and (coll? (last a)) (= '& (first (last a))))
      (concat (drop-last a) (last a))
      a)))

(s/def ::core-specs/param-list
  (s/and
    vector?
    (s/conformer identity arg-list-unformer)
    (s/cat :args (s/* ::core-specs/binding-form)
           :varargs (s/? (s/cat :amp #{'&} :form ::core-specs/binding-form)))))

(defn- arity-data
  [params+body]
  (let [params+body-value (s/unform ::core-specs/params+body params+body)
        param-list (first params+body-value)
        prepost? (map? (second params+body-value))
        prepost (when prepost? (second params+body-value))
        body (last params+body-value)]
    {:param-list param-list
     :body       body
     :prepost    prepost}))

(defn split-defn-args
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        single-arity? (= :arity-1 (first fn-tail))
        arity-data (if single-arity?
                     (arity-data (-> fn-tail last))
                     (map arity-data (-> fn-tail last :bodies)))]
    (merge conformed-defn-args
           {:single-arity? single-arity?
            :arity-data    arity-data})))

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
  (enrich-var ns-name var-name [type data]))

(defn enrich-defn
  [ns-name [type {:keys [defn-args] :as data}]]
  (let [var-name (:fn-name defn-args)]
    (enrich-var ns-name var-name [type data])))

(defn enrich                                                ;; use defmulti
  [{:keys [ns-args]} conformed-forms]
  (let [ns-name (:ns-name ns-args)]
    (mapv (fn [form]
            (condp = (first form)
              :def (enrich-def ns-name form)
              :defn (enrich-defn ns-name form)
              form))
          conformed-forms)))

(comment

  (let [forms (->> "examples/bare-bones-project/io/parennial/user.clj"
                   slurp
                   ap/message->forms
                   text-parsing/whole-ns->spec-form-data)]
    #_(enrich (-> forms first last)
            (rest forms)))

  )

