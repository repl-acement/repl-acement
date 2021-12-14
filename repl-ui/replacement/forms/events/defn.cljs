(ns replacement.forms.events.defn
  "A `defn` form describing a function is a sequence of parts.
  The first parts (name, docstring and meta) are fixed, only `name` is mandatory.
  The next and last part (the tail) is a sequence of a list of parts that can be repeated.
  Iff there is only 1 element in the tail sequence, the parts are stripped out of the list
  and merged with the function sequence. In all other cases, each of the tail items are
  retained in their own list.

  This namespace is concerned with breaking the defn forms down using `spec/conform` and
  putting them back together with `unform`. Editing by a human or a function can happen
  in between."
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [cljs.spec.alpha :as s]
    [nextjournal.clojure-mode.extensions.formatting :as format]
    [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.protocol.cljs-fn-specs :as core-fn-specs]
    [replacement.protocol.data :as data-specs]
    [replacement.protocol.patched-core-specs]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.structure.wiring :as wiring]
    [zprint.core :refer [zprint-file-str]]
    [clojure.edn :as edn]
    [clojure.walk :as walk]))

; --- seem common
(defn extract-tx-text
  [^js tx]
  (->> (.-newDoc tx) (.toJSON) js->cljs (apply str)))

(defn extract-cm-text
  [^js cm]
  (->> (-> cm .-state .-doc) (.toJSON) js->cljs (apply str)))

(defn- update-cm!
  ([cm tx]
   (.update cm #js [tx])))

(defn- replacement-tx
  [cm text]
  (let [cm-state   (-> cm .-state)
        doc-length (-> cm-state .-doc .-length)]
    (.update cm-state (clj->js {:changes {:from 0 :to doc-length :insert text}}))))

(defn fix-width-format
  ([text]
   (fix-width-format text 60))
  ([text width]
   (zprint-file-str text ::fix-width-format {:width width})))

(defn- format-tx
  [text cm]
  (->> text fix-width-format (replacement-tx cm)))

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (assoc db comp-name {:cm cm :name comp-name})))

; --- seem common [ END ]

(defn- unformed-arity-data
  [params+body]
  (let [params+body-value (s/unform ::core-fn-specs/params+body params+body)
        params-value      (first params+body-value)
        pre-post?         (map? (second params+body-value))
        pre-post          (when pre-post? (second params+body-value))
        body-value        (if pre-post? (drop 2 params+body-value)
                                        (drop 1 params+body-value))]
    {:params-value params-value
     :body         body-value
     :pre-post-map pre-post}))

(defn conformed-arity-data
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        single-arity? (= :arity-1 (first fn-tail))
        extra-meta    (when-not single-arity? (get-in fn-tail [:arity-n :arity-map]))]
    {:single-arity? single-arity?
     :extra-meta    extra-meta}))

(defn split-defn-args
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        {:keys [single-arity?] :as props} (conformed-arity-data conformed-defn-args)
        arity-data (if single-arity?
                     (-> fn-tail last unformed-arity-data vector)
                     (map unformed-arity-data (-> fn-tail last :bodies)))]
    (merge props {:defn-args  conformed-defn-args
                  :arity-data arity-data})))

(defn update-cms!
  [changes]
  (doall (map (fn [{:keys [cm tx]}] (update-cm! cm tx)) changes)))

;; Data to automate conformed form data to form specific properties
;; TODO drop these
(def common-parts [:defn.name :defn.docstring :defn.meta])
(def ^:private part->props-map (->> common-parts
                                    (map #(hash-map %1 %2) [:fn-name :docstring :meta])
                                    (apply merge)))

(s/def ::optional-string (s/nilable string?))
(s/def ::optional-map (s/nilable map?))
(s/def ::body (s/+ any?))

(defn- set-pre-post*
  [index index-count new-value node]
  (let [return-node (cond
                      ;; There is an existing prepost property at the index
                      (and (vector? node)
                           (= (first node) :prepost+body)
                           (= index @index-count))
                      [:prepost+body (assoc (last node) :prepost new-value)]

                      ;; There is not an existing prepost property at the index
                      (and (vector? node)
                           (= (first node) :body)
                           (= (first (last node)) :body)
                           (= index @index-count))
                      [:body [:prepost+body (assoc {} :prepost new-value :body (last (last node)))]]

                      :else node)]
    (when (and (map? node) (:params node))
      (swap! index-count inc))
    return-node))

(defn set-pre-post
  [conformed-data new-value index]
  (let [index       (or index 0)
        index-count (atom 0)]
    (walk/postwalk
      (partial set-pre-post* index index-count new-value)
      conformed-data)))

(defn set-params
  [conformed-data new-value index]
  (let [index       (or index 0)
        index-count (atom 0)]
    (walk/postwalk
      (fn [node]
        (let [return-node (if (and (map? node)
                                   (= index @index-count)
                                   (:params node))
                            (assoc-in node [:params] new-value)
                            node)]
          (when (and (map? node) (:params node))
            (swap! index-count inc))
          return-node))
      conformed-data)))

(defn- set-body*
  [index index-count new-value node]
  (let [return-node (cond
                      ;; There is an existing prepost property at the index
                      (and (vector? node)
                           (= (first node) :prepost+body)
                           (= index @index-count))
                      [:prepost+body (assoc (last node) :body new-value)]

                      ;; There is not an existing prepost property at the index
                      (and (vector? node)
                           (= (first node) :body)
                           (= (first (last node)) :body)
                           (= index @index-count))
                      [:body [:body new-value]]

                      :else node)]
    (when (and (map? node) (:params node))
      (swap! index-count inc))
    return-node))

(defn set-body
  [conformed-data new-value index]
  (let [index       (or index 0)
        index-count (atom 0)]
    (walk/postwalk
      (partial set-body* index index-count new-value)
      conformed-data)))

(def parts {:defn.name      {:name :fn-name
                             :spec simple-symbol?
                             :path #(assoc-in %1 [:defn-args :fn-name] %2)}
            :defn.docstring {:name :docstring
                             :spec ::optional-string
                             :path #(assoc-in %1 [:defn-args :docstring] %2)}
            :defn.meta      {:name :meta
                             :spec ::optional-map
                             :path #(assoc-in %1 [:defn-args :meta] %2)}
            :defn.params    {:name   :params-value
                             :spec   ::core-fn-specs/param-list
                             :arity? true
                             :path   #(set-params %1 %2 %3)}
            :defn.pre-post  {:name   :pre-post-map
                             :spec   ::optional-map
                             :arity? true
                             :path   #(set-pre-post %1 %2 %3)}
            :defn.body      {:name   :body
                             :spec   ::body
                             :arity? true
                             :path   #(set-body %1 %2 %3)}})

(def ^:private props-map->part (clojure.set/map-invert part->props-map))

(def arity-parts [:defn.params :defn.pre-post :defn.body])
(def ^:private arity->props-map (->> arity-parts
                                     (map #(hash-map %1 %2) [:params-value :pre-post-map :body])
                                     (apply merge)))

(def ^:private single-arity-spec ::core-fn-specs/params+body)

(s/def ::multi-arity-specs (s/cat :bodies (s/+ (s/spec ::core-fn-specs/params+body))
                                  :attr-map (s/? map?)))

(def ^:private multi-arity-spec ::multi-arity-specs)

(def ^:private props-map->arity (clojure.set/map-invert arity->props-map))

(def multi-arity-attrs [:extra-meta])
(def ^:private multi-arity->attrs-map {:extra-meta :attr-map})
(def ^:private attrs-map->multi-arity (clojure.set/map-invert multi-arity->attrs-map))

(defn update-arity-defn-args
  "Updates the value of `property-name` to `new-value` in the `conformed-data` map"
  ([conformed-data property-name new-value]
   (update-arity-defn-args conformed-data property-name new-value 0))
  ([conformed-data property-name new-value arity-index]
   (let [arg-key (props-map->part property-name)]
     (assoc-in conformed-data [:defn-args arg-key] new-value))))

;; TODO - extract to whole-ns?
(defn- conformed-data->properties
  "Function to automate conformed form data to form specific properties"
  [data props-map]
  (-> (reduce-kv (fn [data k v]
                   (-> data
                       (dissoc k)
                       (assoc v (data k))))
                 data props-map)
      (select-keys (vals props-map))))

(defn update-cm-states
  [db data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [property-name (wiring/cm-name->comp-name cm-key)
          data          (get data property-name)
          text          (if (seq? data)
                          (apply str (interpose "\n" (map pr-str data)))
                          (pr-str data))
          formatted     (fix-width-format text)
          tx            (replacement-tx cm formatted)]
      (conj cms {:cm cm :tx tx}))
    cms))

(defn- fixed-items-update-cms!
  "Update the parts of this form that are fixed when it is conformed, including any that are nillable."
  [{:keys [defn-args] :as db} triggering-cm-key]
  (let [properties       (conformed-data->properties defn-args part->props-map)
        cm-keys          (->> common-parts
                              (map wiring/comp-name->cm-name)
                              (filter #(not= triggering-cm-key %)))
        cms-with-changes (reduce (partial update-cm-states db properties) [] cm-keys)]
    (update-cms! cms-with-changes)))

(defn- attrs-update-cms!
  "Update the extra attributes for multi-arity forms, if present"
  [{:keys [defn-args] :as db} triggering-cm-key]
  (let [arity-data       (-> defn-args :fn-tail last)
        properties       {:extra-meta (:attr-map arity-data)}
        cm-keys          (->> multi-arity-attrs
                              (map wiring/comp-name->cm-name)
                              (filter #(not= triggering-cm-key %)))
        cms-with-changes (reduce (partial update-cm-states db properties) [] cm-keys)]
    (update-cms! cms-with-changes)))

(defn- arity-update-cms!
  "Update the parts of this form that are variable when it is conformed, including any that are nillable."
  [db triggering-cm-key an-arity-data]
  (let [properties       (conformed-data->properties an-arity-data arity->props-map)
        cm-keys          (->> arity-parts
                              (map wiring/comp-name->cm-name)
                              (filter #(not= triggering-cm-key %)))
        cms-with-changes (reduce (partial update-cm-states db properties) [] cm-keys)]
    (update-cms! cms-with-changes)))

(reg-event-db
  ::arity-update-cms
  (fn [{:keys [arity-data] :as db} [_ index]]
    (arity-update-cms! db :none (nth arity-data index))
    (assoc db :arity-index index)))

(defn- text->spec-data
  [text]
  (let [data          (rdr/read-string text)
        conformed     (s/conform ::data-specs/defn-form data)
        explain-data  (and (= s/invalid? conformed) (s/explain-data ::data-specs/defn-form data))
        unformed      (or (= s/invalid? conformed) (s/unform ::data-specs/defn-form conformed))
        fn-properties {:defn.text         (-> unformed pr-str fix-width-format)
                       :defn.conformed    conformed
                       :defn.explain-data explain-data
                       :defn.unformed     unformed}
        fn-data       (split-defn-args (:defn-args conformed))]
    (merge fn-properties fn-data)))

(defn- conformed->spec-data
  [conformed]
  (let [unformed      (when-not (= s/invalid? conformed)
                        (prn ::conformed->spec-data0)
                        (s/unform ::data-specs/defn-form conformed))
        fn-properties {:defn.text         (-> unformed pr-str fix-width-format)
                       :defn.conformed    conformed
                       :defn.explain-data nil
                       :defn.unformed     unformed}
        fn-data       (split-defn-args (:defn-args conformed))]
    (merge fn-properties fn-data)))

;; TODO - incorrect approach, CMs should be re-composed according to the way that they were de-composed
;; eg each part should have the path into the data that should be updated and then `unform` is trivial
;; in short - this should not be needed
(defn- cm-keys->text
  [db cm-keys]
  (->> (reduce (fn [text k]
                 (->> (get-in db [k :cm])
                      (extract-cm-text)
                      (conj text)))
               [] cm-keys)
       (interpose " ")
       (apply str)))

;; TODO: see above: should not be needed
(defn- common-parts-text
  [db]
  (->> (map wiring/comp-name->cm-name common-parts)
       (cm-keys->text db)))

;; TODO: see above: should not be needed
(defn- arity-text
  [db index per-arity-data]
  (let [defn-data (conformed-data->properties per-arity-data arity->props-map)]
    (->> (keys defn-data)
         (map (partial wiring/indexed-comp-name->cm-name index))
         (cm-keys->text db))))

;; TODO - as below, this should not be needed
(reg-fx
  ::fn-whole-update
  (fn [[cm whole-text]]
    (let [tx (->> whole-text fix-width-format (replacement-tx cm))]
      (update-cm! cm tx))))

;; TODO: this should be from defn.text that is built from unform
;; and such a change should be signalled per active CM
;; in short, this function is not needed
(defn- whole-form-updated
  "Scan over all the active code mirrors that can provide updates
  and create the new form to reflect any updates"
  [db]
  (let [defn-type   (get-in db [:defn.conformed :defn-type])
        fixed-parts (common-parts-text db)
        arity-parts (map-indexed (fn [index data]
                                   (arity-text db index data))
                                 (:arity-data db))
        parenned    (if (= 1 (count arity-parts))
                      arity-parts
                      (map #(str "(" % ")") arity-parts))
        form-text   (apply str fixed-parts " " parenned)]
    (str "(" defn-type " " form-text ")")))

;; TODO - as above, this should not be needed
(reg-event-fx
  ::set-part-in-whole
  (fn [{:keys [db]} [_]]
    (let [cm         (get-in db [:defn.form.cm :cm])
          whole-text (whole-form-updated db)
          updates    (text->spec-data whole-text)]
      {:db               (merge db updates)
       ::fn-whole-update [cm whole-text]})))

(defn update-from-parts
  "Updates the value of `property-name` to `new-value` in the `conformed-data` map. If `new-value` is
  not conforming, the update is nil and the spec explain-data is provided"
  [conformed-data property-name new-value arity-index]
  (let [{:keys [spec path arity?]} (get parts property-name)
        input          (edn/read-string new-value)
        conformed-part (s/conform spec input)
        update         (when-not (s/invalid? conformed-part)
                         (if arity?
                           (path conformed-data conformed-part arity-index)
                           (path conformed-data conformed-part)))
        explain        (when (s/invalid? conformed-part)
                         (s/explain-data spec input))]
    (cljs.pprint/pprint [:input input :conformed-part conformed-part :conformed-data conformed-data :update update])
    {:value   new-value
     :input   input
     :update  update
     :explain explain}))

(reg-event-db
  ::part-edit
  (fn [db [_ part-cm-name tx]]
    ;; Always update the transacting code-mirror
    (-> (get-in db [part-cm-name :cm]) (update-cm! tx))
    (if-not (js->cljs (.-docChanged tx))
      db
      (let [{:keys [arity-index visible-form-id]} db
            {:keys [ref-conformed]} (get db visible-form-id)
            part-name (wiring/cm-name->comp-name part-cm-name)
            text      (extract-tx-text tx)
            updates   (update-from-parts ref-conformed part-name text arity-index)]
        (prn :cm-name part-cm-name :tx (extract-tx-text tx) :arity-index arity-index)
        (if-not (:update updates)
          db
          (let [db' (merge db (conformed->spec-data (:update updates)))
                cm  (get-in db' [:defn.form.cm :cm])]
            (update-cm! cm (format-tx (:defn.text db') cm))
            db'))))))

(defn parts-update!
  [{:keys [arity-data] :as db} source-cm-key]
  (prn :parts-update :from-key source-cm-key)
  (fixed-items-update-cms! db source-cm-key)
  (arity-update-cms! db source-cm-key (first arity-data))
  (attrs-update-cms! db source-cm-key))

(reg-event-db
  ::set-whole-form
  (fn [db [_ var-id]]
    (when-let [cm (get-in db [:defn.form.cm :cm])]
      (let [{:keys [ref-conformed ref-name]} (db var-id)
            visibility {:visible-form-id var-id :the-defn-form ref-name}
            db'        (merge db visibility (conformed->spec-data ref-conformed))]
        (update-cm! cm (format-tx (:defn.text db') cm))
        (and (:defn.conformed db') (parts-update! db' :defn.form.cm))
        db'))))

(reg-event-db
  ::whole-form-tx
  (fn [db [_ cm-name tx]]
    (-> (get-in db [cm-name :cm]) (update-cm! tx))
    (let [text (extract-tx-text tx)
          db'  (merge db (text->spec-data text))]
      (and (:defn.conformed db') (parts-update! db' :defn.form.cm))
      db')))

;; Organize it to match the life-cycle:

; ✓ event to set the whole view of a specific form-id
; ✓ --> fn to write the parts CMs to set the parts view of the whole
; ✓ event to transact changes (keystrokes) on the whole form
; ✓ --> fn to ripple out change to appropriate part
; ✓ events to transact changes (keystrokes) on any of the form parts
; x --> fn to reflect back the part change to whole
; x event to persist changes when the form under inspection is changed

;; TODO - set some warnings if not conformed



