(ns replacement.protocol.patched-core-specs
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]))

;; There is a problem with the defn-args spec: unform and conform are not fully inlined which
;; means that unform returns lists instead of vectors.

;; We can use code from @viebel to monkey patch :defn-args so that unform and conform are fully inlined.

;; from https://github.com/viebel/defntly/blob/main/src/defntly/specs.cljc

;; Fundamental issue is here https://clojure.atlassian.net/browse/CLJ-2021 and seems to be waiting on spec2.

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
