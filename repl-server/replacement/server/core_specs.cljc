(ns replacement.server.core-specs
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]))

(s/def ::ns-form
  (s/cat
    :ns-sym (s/and symbol? #(= 'ns %))
    :ns-args ::specs/ns-form))

(s/def ::defn
  (s/cat
    :defn-type (s/and symbol? #(or (= 'defn %)
                                   (= 'defn- %)))
    :defn-args ::specs/defn-args))

(s/def ::def
  (s/cat
    :def (s/and symbol? #(= 'def %))
    :var-name symbol?
    :docstring (s/? string?)
    :init-expr (s/? any?)))

(s/def ::form
  (s/or :ns ::ns-form
        :def ::def
        :defn ::defn
        :expr list?))

;; from https://github.com/viebel/defntly/blob/main/src/defntly/specs.cljc

;; There is a problem with this defn-args spec: unform and conform are not fully inlined (unform returns lists instead of vectors).
;; The cool thing is that we can monkey patch :defn-args so that unform and conform are fully inlined.

(s/def ::specs/binding-form
  (s/or :local-symbol ::specs/local-name
        :seq-destructure ::specs/seq-binding-form
        :map-destructure ::specs/map-binding-form))

(s/def ::specs/seq-binding-form
  (s/and vector?
         (s/conformer identity vec)
         (s/cat :elems (s/* ::specs/binding-form)
                :rest (s/? (s/cat :amp #{'&} :form ::specs/binding-form))
                :as (s/? (s/cat :as #{:as} :sym ::specs/local-name)))))

(defn arg-list-unformer [a]
  (vec
    (if (and (coll? (last a)) (= '& (first (last a))))
      (concat (drop-last a) (last a))
      a)))

(s/def ::specs/param-list
  (s/and
    vector?
    (s/conformer identity arg-list-unformer)
    (s/cat :args (s/* ::specs/binding-form)
           :varargs (s/? (s/cat :amp #{'&} :form ::specs/binding-form)))))
