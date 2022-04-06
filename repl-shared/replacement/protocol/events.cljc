(ns replacement.protocol.events
	(:require [clojure.spec.alpha :as s]
						[clojure.spec.test.alpha :as stest]
						[replacement.protocol.data :as data]))

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
	"Add name, types and other extractable reference data to maps of form data."
	[conforming-form-data]
	(when-let [ns-data (conforming-ns-data conforming-form-data)]
		(let [an-ns-name (-> ns-data :conformed last :ns-args :ns-name)
					ref-data   (add-reference-data* an-ns-name conforming-form-data)]
			ref-data)))

(defn- ref-data->ref-id-data
	[{:keys [ns ref-name ref-type] :as ref-data}]
	(let [ref-id (random-uuid)]
		{ref-id    ref-data
		 :id-index {ref-id {:ns   ns
												:type ref-type
												:name ref-name}}}))


;; For the demo - have at least two nses to show
;; each ns has an ID in the (re-frame for now) DB
;; ns ID is at the top level of the DB
;; also at the top level two maps
;; ns-name->ns-id and ns-id->ns-name
;;

(defn add-ns
	"Add a new NS to the DB or return the DB if it already exists"
	[{:keys [ns-name->ns-id ns-id->ns-name] :as db} an-ns]
	(let [the-ns-name (::data/var-name an-ns)]
		(if (db (get ns-name->ns-id the-ns-name))
			db                                                       ;; Nothing to index. Write another function to update the version.
			(let [id              (random-uuid)
						indexed-ns      (assoc an-ns :id id)
						ns-name->ns-id' (assoc ns-name->ns-id the-ns-name id)
						ns-id->ns-name' (assoc ns-id->ns-name id the-ns-name)]
				[id (assoc db id indexed-ns
											:ns-name->ns-id ns-name->ns-id'
											:ns-id->ns-name ns-id->ns-name')]))))

;(defn- new-ns-data
;	[db ns-data-str]
;	(let [[index-map index ns-forms] (->> ns-data-str
;																				(form-parser/text->edn-forms)
;																				(form-parser/whole-ns->spec-form-data)
;																				(events-spec/add-reference-data)
;																				(events-spec/index-ns-forms db))

;; TODO persistent version ie not using random-uuid
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
		(assoc db' ns-id ns+forms))

	;; replace vvv with ^^^
	;; check first is-ns?
	(reduce (fn [[_ns-index index kvs] form]
						(let [uuid   (random-uuid)
									idx    (vec (conj index uuid))
									ns-map (hash-map (first idx) idx)
									kv-map (merge kvs (hash-map uuid form))]
							[ns-map idx kv-map]))
					[] forms))


;; TODO lookup ns versions via deps.edn classpath data
;; associate each ns with the version of the source JAR file


(comment

	;; Examples
	(def conforming-defn '(defn xy [x y {:keys [a b]}] (+ x y (/ a b))))
	(def non-conforming-defn '(defn xy (+ x y (/ a b))))

	)

