(ns replacement.protocol.data
  (:require [clojure.spec.alpha :as s]
            #?(:clj  [clojure.core.specs.alpha :as core-specs]
               :cljs [replacement.protocol.cljs-fn-specs :as core-specs])
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as string]
            [replacement.protocol.patched-core-specs])
  #?(:clj (:import (java.util UUID))))

(comment
  "Check all calls"
  (stest/instrument))

(s/def ::id uuid?)

(s/def ::def-sym
  (s/and symbol?
         (partial = 'def)))

(s/def ::defn-sym
  (s/and symbol?
         #(or (= 'defn %)
              (= 'defn- %))))

(s/def ::ns-sym
  (s/and symbol?
         (partial = 'ns)))

(s/def ::minimal-string
  (s/and string? #(not (string/blank? %))))

(s/def ::name symbol?)

(s/def ::ns-name symbol?)

(s/def ::type
  (s/or :ns ::ns-sym
        :def ::def-sym
        :defn ::defn-sym
        :other symbol?))

(s/def ::ns-form
  (s/cat
    :ns ::ns-sym
    :ns-args ::core-specs/ns-form))

(s/def ::defn-form
  (s/cat
    :defn-type ::defn-sym
    :defn-args ::core-specs/defn-args))

(s/def ::def-form
  (s/cat
    :def ::def-sym
    :var-name symbol?
    :docstring (s/? string?)
    :init-expr (s/? any?)))

(s/def ::form
  (s/or :ns ::ns-form
        :def ::defn-form
        :defn ::defn-form
        :expr list?))

(s/def ::text ::minimal-string)

(s/def ::conformed (s/nilable map?))
(s/def ::explain map?)
(s/def ::unformed (s/nilable ::form))

(s/def ::form-data
  (s/keys :req-un [::text ::conformed ::unformed]
          :opt-un [::explain]))









