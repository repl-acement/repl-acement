(ns replacement.structure.form-specs
  (:require [clojure.spec.alpha :as s]
            [replacement.structure.core-fn-specs :as core-specs]
            [replacement.structure.patched-specs]))

(s/def ::ns-form
  (s/cat
    :ns-sym (s/and symbol? #(= 'ns %))
    :ns-args ::core-specs/ns-form))

(s/def ::defn
  (s/cat
    :defn-type (s/and symbol? #(or (= 'defn %)
                                   (= 'defn- %)))
    :defn-args ::core-specs/defn-args))

(s/def ::def
  (s/cat
    :def (s/and symbol? #(= 'def %))
    :var-name symbol?
    :docstring (s/? string?)
    :init-expr (s/+ any?)))

(s/def ::form
  (s/or :ns ::ns-form
        :def ::def
        :defn ::defn
        :expr list?))
