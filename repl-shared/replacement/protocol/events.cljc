(ns replacement.protocol.events
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [replacement.protocol.data :as data])
  #?(:clj (:import (java.util UUID))))

(comment
  "Check all calls"
  (stest/instrument))

(s/def ::form-save
  (s/keys :req [::data/id ::data/form-of ::data/form-data]))

(def form-save-example
  (let [the-form  '(defn xy [x y] (+ x y))
        conformed (s/conform ::data/defn-form the-form)
        unformed  (s/unform ::data/defn-form conformed)]
    {::data/id        #?(:clj  (UUID/randomUUID)
                         :cljs (random-uuid))
     ::data/form-of   'defn
     ::data/form-data {:form-text (pr-str the-form)
                       :conformed conformed
                       :unformed  unformed}}))




