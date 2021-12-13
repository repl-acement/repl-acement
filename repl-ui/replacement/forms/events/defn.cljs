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
    [zprint.core :refer [zprint-file-str]]))

(defn extract-tx-text
  [^js tx]
  (->> (.-newDoc tx) (.toJSON) js->cljs (apply str)))

(defn extract-cm-text
  [^js cm]
  (->> (-> cm .-state .-doc) (.toJSON) js->cljs (apply str)))

(defn- update-cm!
  ([cm tx]
   (update-cm! cm tx nil))
  ([cm tx event-args]
   (.update cm #js [tx])
   (when event-args
     (re-frame/dispatch event-args))))

(defn- replacement-tx
  [cm text]
  (let [cm-state   (-> cm .-state)
        doc-length (-> cm-state .-doc .-length)]
    (.update cm-state (clj->js {:changes {:from 0 :to doc-length :insert text}}))))

(defn- format-tx
  [text cm]
  (->> (zprint-file-str text ::fn-whole-update {:width 60})
       (replacement-tx cm)))

(reg-fx
  ::fn-part-update
  (fn [[cm tx changed?]]
    (if changed?
      (update-cm! cm tx [::set-part-in-whole])
      (update-cm! cm tx))))

(reg-event-fx
  ::part-edit
  (fn [{:keys [db]} [_ part-cm-name tx]]
    (let [cm       (get-in db [part-cm-name :cm])
          changed? (js->cljs (.-docChanged tx))]
      {:db              db
       ::fn-part-update [cm tx changed?]})))

(reg-fx
  ::fn-whole-edit
  (fn [[cm tx changed?]]
    (if changed?
      (update-cm! cm tx [::transact-whole-form (extract-tx-text tx)])
      (update-cm! cm tx))))

(reg-event-fx
  ::fn-whole-form-tx
  (fn [{:keys [db]} [_ cm-name tx]]
    (let [cm       (get-in db [cm-name :cm])
          changed? (js->cljs (.-docChanged tx))]
      {:db             db
       ::fn-whole-edit [cm tx changed?]})))

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (assoc db comp-name {:cm cm :name comp-name})))

(defn- arity-data
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

(defn split-defn-args
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        single-arity? (= :arity-1 (first fn-tail))
        arity-data    (if single-arity?
                        (-> fn-tail last arity-data vector)
                        (map arity-data (-> fn-tail last :bodies)))
        extra-meta    (when-not single-arity? (get-in fn-tail [:arity-n :arity-map]))]
    {:defn-args     conformed-defn-args
     :single-arity? single-arity?
     :arity-data    arity-data
     :extra-meta    extra-meta}))

(defn update-cms!
  [changes]
  (doall (map (fn [{:keys [cm tx]}] (update-cm! cm tx)) changes)))

;; Data to automate conformed form data to form specific properties
(def common-parts [:defn.name :defn.docstring :defn.meta])
(def ^:private part->props-map (->> common-parts
                                    (map #(hash-map %1 %2) [:fn-name :docstring :meta])
                                    (apply merge)))
(def ^:private part-spec ::core-fn-specs/defn-args)

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

(defn update-fixed-defn-args
  "Updates the value of `property-name` to `new-value` in the `conformed-data` map"
  [conformed-data property-name new-value]
  (let [arg-key (props-map->part property-name)]
    (assoc-in conformed-data [:defn-args arg-key] new-value)))

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
          formatted     (zprint-file-str text ::update-cm-states)
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
        fn-properties {:defn.text         (-> unformed pr-str (zprint-file-str ::text->spec-data))
                       :defn.conformed    conformed
                       :defn.explain-data explain-data
                       :defn.unformed     unformed}
        fn-data       (split-defn-args (:defn-args conformed))]
    (merge fn-properties fn-data)))

(defn- conformed->spec-data
  [conformed]
  (let [unformed      (or (= s/invalid? conformed) (s/unform ::data-specs/defn-form conformed))
        fn-properties {:defn.text         (-> unformed pr-str (zprint-file-str ::conformed->spec-data))
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
    (let [tx (->> (zprint-file-str whole-text ::fn-whole-update)
                  (replacement-tx cm))]
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

(defn parts-update!
  [{:keys [arity-data] :as db} source-cm-key]
  (fixed-items-update-cms! db source-cm-key)
  (arity-update-cms! db source-cm-key (first arity-data))
  (attrs-update-cms! db source-cm-key))

(reg-event-db
  ::transact-whole-form
  (fn [db [_ whole-form-text]]
    (let [updates (text->spec-data whole-form-text)
          db'     (merge db updates)]
      (parts-update! db' :defn.form.cm)
      db')))

(reg-event-db
  ::set-view
  (fn [db [_ var-id]]
    (when-let [cm (get-in db [:defn.form.cm :cm])]
      (let [{:keys [ref-conformed ref-name]} (db var-id)
            visibility {:visible-form-id var-id :the-defn-form ref-name}
            defaults   {:meta nil :docstring nil}           ;; lift these so others can use?
            db'        (merge db visibility defaults (conformed->spec-data ref-conformed))]
        (update-cm! cm (format-tx (:defn.text db') cm))
        (parts-update! db' :defn.form.cm)
        (cljs.pprint/pprint [:conformed (:defn.conformed db')])
        db'))))

;; Organize it to match the life-cycle:

; ✓ event to set the whole view of a specific form-id
; ✓ --> fn to write the parts CMs to set the parts view of the whole
; ✓ event to transact changes (keystrokes) on the whole form
; ✓ --> fn to ripple out change to appropriate part
; ✓ events to transact changes (keystrokes) on any of the form parts
; x --> fn to reflect back the part change to whole
; x event to persist changes when the form under inspection is changed




