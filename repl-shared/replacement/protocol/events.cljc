(ns replacement.protocol.events
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [replacement.protocol.data :as data])
  #?(:clj (:import (java.util UUID))))

(comment
  "Check all calls"
  (stest/instrument))

(defn- pprint
  [x]
  #?(:clj  (clojure.pprint/pprint x)
     :cljs (cljs.pprint/pprint x)))

(s/def ::form-event-data
  (s/keys :req [::data/id ::data/type ::data/var-name ::data/ns-name ::data/form-data]))
;; Stronger spec:
;; the ::data/name spec should ensure that is matches the name in the conformed form
;; or vice-versa

(defn conformed-data->event-data
  [ns-name type spec form]
  (let [pre-conformed (s/conform spec form)
        conformed     (when-not (s/invalid? pre-conformed) pre-conformed)
        unformed      (when-not (s/invalid? pre-conformed) (s/unform spec conformed))
        explain       (when (s/invalid? pre-conformed) (s/explain-data spec form))]
    {::data/id        (random-uuid)
     ::data/type      (:type conformed)
     ::data/var-name  (get-in conformed [:defn-args :fn-name])
     ::data/ns-name   ns-name
     ::data/form-data {:text      (pr-str form)
                       :conformed conformed
                       :explain   explain
                       :unformed  unformed}}))

(def form-save-example0
  (let [ns-name       'user
        the-form      '(defn xy [x y {:keys [a b]}] (+ x y (/ a b)))
        pre-conformed (s/conform ::data/defn-form the-form)
        conformed     (when-not (s/invalid? pre-conformed) pre-conformed)
        explain       (when (s/invalid? pre-conformed) (s/explain-data ::data/defn-form the-form))
        unformed      (when-not (s/invalid? pre-conformed) (s/unform ::data/defn-form conformed))]
    {::data/id        (random-uuid)
     ::data/type      (:type conformed)
     ::data/var-name  (get-in conformed [:defn-args :fn-name])
     ::data/ns-name   ns-name
     ::data/form-data {:text      (pr-str the-form)
                       :conformed conformed
                       :explain   explain
                       :unformed  unformed}}))

(defn ns-reference-data
  [ns-name [type {:keys [ns-args] :as data}]]
  {::data/ns-name   ns-name
   ::data/var-name  (:ns-name ns-args)
   ::data/type      type
   ::data/form-data data})

(defn def-reference-data
  [ns-name [type {:keys [var-name] :as data}]]
  {::data/ns-name   ns-name
   ::data/var-name  var-name
   ::data/type      type
   ::data/form-data data})

(defn defn-reference-data
  [ns-name [type {:keys [defn-args] :as data}]]
  {::data/ns-name   ns-name
   ::data/var-name  (:fn-name defn-args)
   ::data/type      type
   ::data/form-data data})

(defn unsupported-reference-data
  [ns-name data]
  {::data/ns-name   ns-name
   ::data/var-name  :unsupported
   ::data/type      :unsupported
   ::data/form-data data})

(def reference-type-data
  "Table of mechanisms to enrich conformed data per type"
  {:ns          {:spec   ::data/ns-form
                 :ref-fn ns-reference-data}
   :def         {:spec   ::data/def-form
                 :ref-fn def-reference-data}
   :defn        {:spec   ::data/defn-form
                 :ref-fn defn-reference-data}
   :unsupported {:spec   nil
                 :ref-fn unsupported-reference-data}})

(defn- add-conforming-reference-data*
  [an-ns-name {:keys [conformed] :as data}]
  (let [a-type (first conformed)
        {:keys [ref-fn]} (or (get reference-type-data a-type)
                             (get reference-type-data :unsupported))]
    (merge data (ref-fn an-ns-name conformed))))

(defn- add-reference-data*
  [an-ns-name conformed-ns-forms]
  (mapv (partial add-conforming-reference-data* an-ns-name)
        conformed-ns-forms))

(defn- conforming-ns-data
  [conformed-ns-forms]
  (->> conformed-ns-forms
       (filter #(and (:conformed %)
                     (= :ns (first (:conformed %)))))
       first))

(defn add-reference-data
  [conformed-ns-forms]
  (when-let [ns-data (conforming-ns-data conformed-ns-forms)]
    (let [an-ns-name (-> ns-data :conformed last :ns-args :ns-name)
          ref-data   (add-reference-data* an-ns-name conformed-ns-forms)]
      ref-data)))

(defn- ref-data->ref-id-data
  [{:keys [ns ref-name ref-type] :as ref-data}]
  (let [ref-id (random-uuid)]
    {ref-id    ref-data
     :id-index {ref-id {:ns   ns
                        :type ref-type
                        :name ref-name}}}))

(defn index-forms
  "Produce an ID for each form and have a map with that ID for the form data.
  Produce a vector of IDs to ensure order is maintained. The first entry can be
  relied upon as the ns declaration"
  [forms]
  {:pre [(seq forms)]}
  ;; check first is-ns?
  (reduce (fn [[_ns-index index kvs] form]
            (let [uuid   (random-uuid)
                  idx    (vec (conj index uuid))
                  ns-map (hash-map (first idx) idx)
                  kv-map (merge kvs (hash-map uuid form))]
              [ns-map idx kv-map]))
          [] forms))


(comment

  ;; Examples
  (def conforming-defn '(defn xy [x y {:keys [a b]}] (+ x y (/ a b))))
  (def non-conforming-defn '(defn xy (+ x y (/ a b))))

  )

