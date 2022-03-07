(ns replacement.forms.events.def
  "A `def` form creates a global variable and can have an expression to set the initial value.

  This namespace is concerned with breaking the def forms down using `spec/conform` and
  putting them back together with `unform`. Editing by a human or a function may happen
  in between providing that it remains `unform`-able. This is relaxed for humans but not
  for functions.

  It is organized to match the life-cycle:
    ✓ event to set the whole view of a specific form-id
    ✓ --> fn to write the parts CMs to set the parts view of the whole
    ✓ event to transact changes (keystrokes) on the whole form
    ✓ --> fn to ripple out change to appropriate parts
    ✓ events to transact changes (keystrokes) on any of the form parts
    ✓ --> fn to reflect back the part change to whole"
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [cljs.spec.alpha :as s]
    [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.forms.events.common :as common]
    [replacement.protocol.data :as data-specs]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.structure.wiring :as wiring]
    [zprint.core :refer [zprint-file-str]]))

(defn extract-tx-text
  [^js tx]
  (->> (.-newDoc tx) (.toJSON) js->cljs (apply str)))

(defn extract-cm-text
  [^js cm]
  (->> (-> cm .-state .-doc) (.toJSON) js->cljs (apply str)))

(defn- update-cm
  ([cm tx]
   (update-cm cm tx nil))
  ([cm tx event-args]
   (.update cm #js [tx])
   (when event-args
     (re-frame/dispatch event-args))))

(defn- replacement-tx
  [cm text]
  (let [cm-state   (-> cm .-state)
        doc-length (-> cm-state .-doc .-length)]
    (.update cm-state (clj->js {:changes {:from 0 :to doc-length :insert text}}))))

(reg-fx
  ::fn-part-update
  (fn [[cm tx changed?]]
    (if changed?
      (update-cm cm tx [::set-part-in-whole])
      (update-cm cm tx))))

(reg-event-fx
  ::part-edit
  (fn [{:keys [db]} [_ part-cm-name tx]]
    (let [cm       (get-in db [part-cm-name :cm])
          changed? (js->cljs (.-docChanged tx))]
      {:db              db
       ::fn-part-update [cm tx changed?]})))

(reg-fx
  ::whole-edit
  (fn [[cm tx changed?]]
    (if changed?
      (update-cm cm tx [::transact-whole-form (extract-tx-text tx)])
      (update-cm cm tx))))

(reg-event-fx
  ::def-whole-form-tx
  (fn [{:keys [db]} [_ cm-name tx]]
    (let [cm       (get-in db [cm-name :cm])
          changed? (js->cljs (.-docChanged tx))]
      {:db          db
       ::whole-edit [cm tx changed?]})))

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (assoc db comp-name {:cm cm :name comp-name})))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (map (fn [{:keys [cm tx]}]
                  (update-cm cm tx))
                changes))))

(def parts [:def.name :def.docstring :def.init])
(def part->props-map (apply merge (map #(hash-map %1 %2) [:var-name :docstring :init-expr] parts)))

(defn- def-data->properties
  [def-data]
  (reduce-kv (fn [data k v]
               (-> data
                   (dissoc k)
                   (assoc v (data k))))
             def-data part->props-map))

(defn update-cm-states
  [db def-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [def-property-name (wiring/cm-name->comp-name cm-key)
          new-text          (pr-str (get def-data def-property-name))
          tx                (replacement-tx cm new-text)]
      (conj cms {:cm cm :tx tx}))
    cms))

(reg-event-fx
  ::fixed-items-update-cms
  (fn [{:keys [db]} [_]]
    (let [cm-keys          (map wiring/comp-name->cm-name parts)
          def-data         (def-data->properties db)
          cms-with-changes (reduce (partial update-cm-states db def-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(defn- text->spec-data
  [text]
  (let [data         (rdr/read-string text)
        conformed    (s/conform ::data-specs/def-form data)
        explain-data (and (s/invalid? conformed) (s/explain-data ::data-specs/def-form data))
        unformed     (or (s/invalid? conformed) (s/unform ::data-specs/def-form conformed))]
    {:def.text         (-> unformed pr-str common/fix-width-format)
     :def.conformed    conformed
     :def.explain-data explain-data
     :def.unformed     unformed}))

(defn- conformed->spec-data
  [conformed]
  (let [unformed (when-not (s/invalid? conformed)
                   (s/unform ::data-specs/def-form conformed))]
    {:def.text         (-> unformed pr-str common/fix-width-format)
     :def.conformed    conformed
     :def.explain-data (when (s/invalid? conformed)
                         (s/explain-data ::data-specs/def-form conformed))
     :def.unformed     unformed}))

(defn- conformed-form->spec-data
  [{:keys [conformed]}]
  (println :conformed conformed)
  (let [unformed (when-not (s/invalid? conformed)
                   (s/unform ::data-specs/form conformed))]
    {:text         (-> unformed pr-str common/fix-width-format)
     :conformed    conformed
     :explain-data (when (s/invalid? conformed) (s/explain-data ::data-specs/form conformed))
     :unformed     unformed}))

(defn get-var-data
  [current-ns var-id]
  (let [form (get-in current-ns [:forms var-id])]
    {:def-data (conformed-form->spec-data form)
     :def-name (::data-specs/var-name form)}))

(defn- cm-keys->text
  [db cm-keys]
  (->> (reduce (fn [text k]
                 (->> (get-in db [k :cm])
                      (extract-cm-text)
                      (conj text)))
               [] cm-keys)
       (interpose " ")
       (apply str)))

(defn- common-parts-text
  [db]
  (->> (map wiring/comp-name->cm-name parts)
       (cm-keys->text db)))

(reg-fx
  ::whole-update
  (fn [[cm whole-text]]
    (->> (zprint-file-str whole-text ::whole-update)
         (replacement-tx cm)
         (update-cm cm))))

;; TODO: this should be from def.text that is built from unform
;; and such a change should be signalled per active CM
;; in short, this function is not needed
(defn- whole-form-updated
  "Scan over all the active code mirrors that can provide updates
  and create the new form to reflect any updates"
  [db]
  (let [fixed-parts (common-parts-text db)
        form-text   (apply str fixed-parts)]
    (str "(def " form-text ")")))

(reg-event-fx
  ::set-part-in-whole
  (fn [{:keys [db]} [_]]
    (let [cm         (get-in db [:def.form.cm :cm])
          whole-text (whole-form-updated db)
          updates    (text->spec-data whole-text)]
      {:db            (merge db updates)
       ::whole-update [cm whole-text]})))

(reg-fx
  ::parts-update
  (fn []
    (re-frame/dispatch [::fixed-items-update-cms])))

(reg-event-fx
  ::transact-whole-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (let [updates (text->spec-data whole-form-text)]
      {:db            (merge db updates)
       ::parts-update updates})))

(reg-fx
  ::view-update
  (fn [[cm whole-text]]
    (let [tx (->> (zprint-file-str whole-text ::view-update)
                  (replacement-tx cm))]
      (update-cm cm tx)
      (re-frame/dispatch [::fixed-items-update-cms]))))

(reg-event-db
  ::set-form
  (fn [db [_ var-id]]
    (when-let [cm (get-in db [:def.form.cm :cm])]
      (let [var-data   (get-var-data (:current-ns db) var-id)
            visibility {:visible-form-id var-id}
            db'        (merge db var-data visibility)]
        (->> db' :def.text (common/format-tx cm) (common/update-cm! cm))
        #_(when (:def.conformed db') (parts-update! db' :def.form.cm))
        db'))))

(reg-event-fx
  ::set-view
  (fn [{:keys [db]} [_ var-id]]
    (when-let [cm (get-in db [:def.form.cm :cm])]
      (let [var-data   (get-var-data (:current-ns db) var-id)
            visibility {:visible-form-id var-id}
            new-view   (merge var-data visibility)]
        (cljs.pprint/pprint [::set-view (merge var-data visibility)])

        ;; TODO Next ...update the style to set-form as above
        ;; convert the subscriptions / subscribers to use the new shape of the data

        (let [var-data       (db var-id)
              conformed-data (:ref-conformed var-data)
              updates        (conformed->spec-data conformed-data)]
          {:db           (merge db {:visible-form-id var-id} conformed-data updates new-view)
           ::view-update [cm (:def.text updates)]})))))


;; transform this to use protocol data



