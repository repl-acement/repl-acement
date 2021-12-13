(ns replacement.protocol.events
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [replacement.protocol.data :as data])
  #?(:clj (:import (java.util UUID))))

(comment
  "Check all calls"
  (stest/instrument))

(s/def ::form-save
  (s/keys :req [::data/id ::data/type ::data/name ::data/ns-name ::data/form-data]))
;; TODO ... ::data/name should match the name in the conformed form

(def form-save-example0
  (let [the-form  '(defn xy [x y {:keys [a b]}] (+ x y (/ a b)))
        conformed (s/conform ::data/defn-form the-form)
        unformed  (s/unform ::data/defn-form conformed)]
    {::data/id        #?(:clj  (UUID/randomUUID)
                         :cljs (random-uuid))
     ::data/type      'defn
     ::data/name      'xy
     ::data/ns-name   'user
     ::data/form-data {:text      (pr-str the-form)
                       :conformed conformed
                       :unformed  unformed}}))

(def form-save-example1
  (let [the-form      '(defn xy (+ x y))
        pre-conformed (s/conform ::data/defn-form the-form)
        conformed     (when (s/invalid? pre-conformed) pre-conformed)
        explain       (when-not conformed
                        (s/explain-data ::data/defn-form the-form))
        unformed      (when conformed
                        (s/unform ::data/defn-form conformed))]
    {::data/id        #?(:clj  (UUID/randomUUID)
                         :cljs (random-uuid))
     ::data/type      'defn
     ::data/name      'xy
     ::data/ns-name   'user
     ::data/form-data {:text      (pr-str the-form)
                       :conformed conformed
                       :explain   explain
                       :unformed  unformed}}))




