(ns replacement.protocol.events
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [replacement.protocol.data :as data]
            [replacement.protocol.hashing :as hashing])
  #?(:clj (:import (java.util UUID))))

(comment
  "Check all calls"
  (stest/instrument))

(defn pprint
  [x]
  #?(:clj  (clojure.pprint/pprint x)
     :cljs (cljs.pprint/pprint x)))

(defn destruct-form-id
  [form-id]
  (let [uuid #?(:clj (UUID/fromString (namespace form-id))
                :cljs (uuid (namespace form-id)))
        digest (name form-id)]
    [uuid digest]))

(defn destruct-form-name
  [form-name]
  (let [the-namespace (symbol (namespace form-name))
        form-digest (keyword (name form-name))
        digest (name form-digest)
        the-form-name (symbol (namespace form-digest))]
    [the-namespace the-form-name digest]))

(s/def ::form-event-data
  (s/keys :req [::data/id ::data/type ::data/var-name ::data/ns-name ::data/form-data]))
;; Stronger spec:
;; the ::data/name spec should ensure that is matches the name in the conformed form
;; or vice-versa

(defn conformed-data->event-data
  [ns-name type spec form]
  (let [pre-conformed (s/conform spec form)
        conformed (when-not (s/invalid? pre-conformed) pre-conformed)
        unformed (when-not (s/invalid? pre-conformed) (s/unform spec conformed))
        explain (when (s/invalid? pre-conformed) (s/explain-data spec form))]
    {::data/id        (random-uuid)
     ::data/type      (:type conformed)
     ::data/var-name  (get-in conformed [:defn-args :fn-name])
     ::data/ns-name   ns-name
     ::data/form-data {:text      (pr-str form)
                       :conformed conformed
                       :explain   explain
                       :unformed  unformed}}))

(def form-save-example0
  (let [ns-name 'user
        the-form '(defn xy [x y {:keys [a b]}] (+ x y (/ a b)))
        pre-conformed (s/conform ::data/defn-form the-form)
        conformed (when-not (s/invalid? pre-conformed) pre-conformed)
        explain (when (s/invalid? pre-conformed) (s/explain-data ::data/defn-form the-form))
        unformed (when-not (s/invalid? pre-conformed) (s/unform ::data/defn-form conformed))]
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
  "Add name, types and other extractable reference data to maps of form data."
  [conforming-form-data]
  (when-let [ns-data (conforming-ns-data conforming-form-data)]
    (let [an-ns-name (-> ns-data :conformed last :ns-args :ns-name)
          ref-data (add-reference-data* an-ns-name conforming-form-data)]
      ref-data)))

;; For the demo - have at least two nses to show
;; each ns has an ID in the (re-frame for now) DB
;; ns ID is at the top level of the DB
;; also at the top level two maps
;; ns-name->ns-id and ns-id->ns-name

(defn add-ns
  "Add a new NS to the DB or return the DB if it already exists"
  [{:keys [ns-name->ns-id ns-id->ns-name] :as db} an-ns]
  (let [the-ns-name (::data/var-name an-ns)]
    (if (db (get ns-name->ns-id the-ns-name))
      ;; Nothing to index. Write another function to update the version.
      db
      ;; Produce the index.
      (let [id (random-uuid)
            indexed-ns (assoc an-ns :id id)
            ns-name->ns-id' (assoc ns-name->ns-id the-ns-name id)
            ns-id->ns-name' (assoc ns-id->ns-name id the-ns-name)]
        [id (assoc db id indexed-ns
                      :ns-name->ns-id ns-name->ns-id'
                      :ns-id->ns-name ns-id->ns-name')]))))

(defn update-ns-data2
  "Add"
  []
  )

(defn add-ns-data2
  "Add"
  []
  )

(defn update-ns2
  "Update the digest of an existing ns.
  [id latest-digest]"
  [])

(defn existing-ns?
  "Check whether the given form is a conforming ns and exists in the DB with an identifier"
  [{:keys [ns-name->ns-id]} {::data/keys [var-name] :as an-ns}]
  (boolean (when (and var-name (s/valid? ::data/ns-form (:form an-ns)))
             (get ns-name->ns-id var-name false))))

(defn update-ns-index
  "The index comprises: [name id latest-digest] structured as
  {ns-name -> {:id id :digest digest}
   id -> {:ns-name ns-name :digest digest}}
  Name is the string name of the ns and the id is a UUID
  The digest is not guaranteed to be unique so cannot be a primary identifier"
  [{:keys [ns-name->ns-id ns-id->ns-name] :as db} an-ns]
  (let [the-ns-name (::data/var-name an-ns)
        id (random-uuid)
        digest (hashing/digest an-ns)]
    (assoc db id an-ns
              :ns-name->ns-id (assoc ns-name->ns-id the-ns-name {:id id :digest digest})
              :ns-id->ns-name (assoc ns-id->ns-name id {:ns-name the-ns-name :digest digest}))))

(defn update-ns-digest
  "Update the ns and ns-indexes with the hash of the digests of the vars within the ns"
  [{:keys [ns-name->ns-id ns-id->ns-name ns-name->forms] :as db} ns-name]
  (let [ns-id (:id (get ns-name->ns-id ns-name))
        form-digests (get ns-name->forms ns-name)
        digest (hashing/digest form-digests)]
    (assoc db :ns-name->ns-id (assoc ns-name->ns-id ns-name {:id ns-id :digest digest})
              :ns-id->ns-name (assoc ns-id->ns-name ns-id {:ns-name ns-name :digest digest}))))

(defn existing-form?
  "Check whether the given form exists in the DB with an identifier"
  [{:keys [form-name->id] :as db} {::data/keys [var-name]}]
  (boolean (get-in db [form-name->id var-name])))

(defn add-form-to-ordered-index
  "An ordered vector of forms is maintained with their latest digest.
  The new form is appended to the list."
  ;; TODO -- make a function to insert a form into a position in the list once needed
  [{:keys [form-name->id] :as db} {::data/keys [ns-name var-name]}]
  (let [qualified-form-name (keyword (name ns-name) (name var-name))
        form-id (get form-name->id qualified-form-name)]
    (update-in db [:ns-name->forms ns-name] conj form-id)))

(defn add-form-to-indexes
  "The index comprises: [name id latest-digest] structured as
  {:qualified-form-name -> :id/digest
   id/digest -> :qualified-form-name/digest}
  The digest is not guaranteed to be unique so cannot be a primary identifier."
  [db form-data]
  (let [id (random-uuid)
        {::data/keys [ns-name var-name]} form-data
        qualified-form-keyword (keyword (name ns-name) (name var-name))
        digest (hashing/digest (:conformed form-data))
        id+digest-keyword (keyword (str id) digest)
        name+digest-keyword (keyword (str (name ns-name) "/" (str (name var-name) "/" digest)))
        id-entry (hash-map qualified-form-keyword id+digest-keyword)
        name-entry (hash-map id+digest-keyword name+digest-keyword)]
    (-> db
        (assoc id+digest-keyword form-data)
        (update :form-name->id merge id-entry)
        (update :form-id->name merge name-entry)
        (add-form-to-ordered-index form-data))))

(defn add+index-ns
  "Add conforming ns forms to the DB and place the new ns in the index of all ns names and IDs.
  If the ns already exists, return the DB."
  [db ns-forms]
  (let [ns-form (first ns-forms)]
    (if (existing-ns? db ns-form)
      db
      (let [the-ns-name (::data/var-name ns-form)
            ns-index-updated-db (-> (update-ns-index db ns-form)
                                    (add-form-to-indexes ns-form)
                                    (update-ns-digest the-ns-name))]
        (reduce (fn [result {::data/keys [ns-name] :as form-data}]
                  (let [form-index-updated-db (add-form-to-indexes result form-data)]
                    (update-ns-digest form-index-updated-db ns-name)))
                ns-index-updated-db (rest ns-forms))))))

; TODO
; - add a project which contains
;   { :name "Blah"
;     :digest "sum of ns digests"}
;     :nses {:name->id {the-ns-name {:id uuid :digest "123"}}
;           :id->name {uuid  {:name the-ns-name :digest "123"}}}
;
;------- ^^^ the way forward


;; TODO persistent version ie not using random-uuid.... ^^^ use that
(defn index-ns-forms
  "Produce an ID for each form and have a map with that ID for the form data.
  Produce a vector of IDs to ensure order is maintained. The first entry can be
  relied upon as the ns declaration"
  [db forms]
  {:pre [(seq forms)]}
  (let [[ns-id db'] (add-ns db (first forms))
        ns+forms (reduce (fn [result {:keys [::data/var-name] :as form}]
                           (let [form-id (random-uuid)]
                             (-> result
                                 (update-in [:forms :forms-index] (comp vec conj) form-id)
                                 (update-in [:forms :form-id->form] merge (hash-map form-id form))
                                 (update-in [:forms :form-id->name] merge (hash-map form-id var-name))
                                 (update-in [:forms :form-name->id] merge (hash-map var-name form-id)))))
                         (get db' ns-id) (rest forms))]
    (assoc db' ns-id ns+forms)

    ;; replace vvv with ^^^
    ;; check first is-ns?
    (let [result (reduce (fn [[_ns-index index kvs] form]
                           (let [uuid (random-uuid)
                                 idx (vec (conj index uuid))
                                 ns-map (hash-map (first idx) idx)
                                 kv-map (merge kvs (hash-map uuid form))]
                             [ns-map idx kv-map]))
                         [] forms)]

      (pprint [:x forms])
      (pprint [:y result])
      result)))


;; TODO lookup ns versions via deps.edn classpath data
;; associate each ns with the version of the source JAR file


(comment

  ;; Examples
  (def conforming-defn '(defn xy [x y {:keys [a b]}] (+ x y (/ a b))))
  (def non-conforming-defn '(defn xy (+ x y (/ a b))))

  )

