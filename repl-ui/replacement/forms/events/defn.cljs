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
  ::fn-whole-edit
  (fn [[cm tx changed?]]
    (if changed?
      (update-cm cm tx [::transact-whole-defn-form (extract-tx-text tx)])
      (update-cm cm tx))))

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
        pp?               (map? (second params+body-value))
        pp                (when pp? (second params+body-value))
        body-value        (if pp? (drop 2 params+body-value)
                                  (drop 1 params+body-value))]
    {:params-value params-value
     :body         body-value
     :pre-post-map pp}))

(defn split-defn-args
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        single-arity? (= :arity-1 (first fn-tail))
        arity-data    (if single-arity?
                        (-> fn-tail last arity-data vector)
                        (map arity-data (-> fn-tail last :bodies)))]
    {:defn-args     conformed-defn-args
     :single-arity? single-arity?
     :arity-data    arity-data}))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (map (fn [{:keys [cm tx]}] (update-cm cm tx)) changes))))

;; Data to automate conformed form data to form specific properties
(def common-parts [:defn.name :defn.docstring :defn.meta])
(def ^:private part->props-map (->> common-parts
                                    (map #(hash-map %1 %2) [:fn-name :docstring :meta])
                                    (apply merge)))

(def arity-parts [:defn.params :defn.pre-post :defn.body])
(def ^:private arity->props-map (->> arity-parts
                                     (map #(hash-map %1 %2) [:params-value :pre-post-map :body])
                                     (apply merge)))

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
  [db defn-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [defn-property-name (wiring/cm-name->comp-name cm-key)
          data               (get defn-data defn-property-name)
          text               (if (seq? data)
                               (apply str (interpose "\n" (map pr-str data)))
                               (pr-str data))
          formatted          (zprint-file-str text ::update-cm-states)
          tx                 (replacement-tx cm formatted)]
      (conj cms {:cm cm :tx tx}))
    cms))

(reg-event-fx
  ::fn-fixed-items-update-cms
  (fn [{:keys [db]} [_]]
    (let [defn-args        (:defn-args db)
          cm-keys          (map wiring/comp-name->cm-name common-parts)
          defn-data        (conformed-data->properties defn-args part->props-map)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(reg-event-fx
  ::fn-arity-n-update-cms
  (fn [{:keys [db]} [_ per-arity-data index]]
    (let [cm-keys          (map (partial wiring/indexed-comp-name->cm-name index) arity-parts)
          defn-data        (conformed-data->properties per-arity-data arity->props-map)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(defn- text->spec-data
  [text]
  (prn :text->spec-data :text text)
  (let [data          (rdr/read-string text)
        conformed     (s/conform ::data-specs/defn-form data)
        explain-data  (and (= s/invalid? conformed) (s/explain-data ::data-specs/defn-form data))
        unformed      (or (= s/invalid? conformed) (s/unform ::data-specs/defn-form conformed))
        fn-properties {:defn.text         text
                       :defn.conformed    conformed
                       :defn.explain-data explain-data
                       :defn.unformed     unformed}
        fn-data       (split-defn-args (:defn-args conformed))]
    (merge fn-properties fn-data)))

(defn- conformed->spec-data
  [conformed]
  (let [unformed      (or (= s/invalid? conformed) (s/unform ::data-specs/defn-form conformed))
        fn-properties {:defn.text         (pr-str unformed)
                       :defn.conformed    conformed
                       :defn.explain-data nil
                       :defn.unformed     unformed}
        fn-data       (split-defn-args (:defn-args conformed))]
    (merge fn-properties fn-data)))

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
  (->> (map wiring/comp-name->cm-name common-parts)
       (cm-keys->text db)))

(defn- arity-text
  [db index per-arity-data]
  (let [defn-data (conformed-data->properties per-arity-data arity->props-map)]
    (->> (keys defn-data)
         (map (partial wiring/indexed-comp-name->cm-name index))
         (cm-keys->text db))))

(reg-fx
  ::fn-whole-update
  (fn [[cm whole-text]]
    (let [tx (->> (zprint-file-str whole-text ::fn-whole-update)
                  (replacement-tx cm))]
      (update-cm cm tx))))

(defn- whole-form-updated
  "Scan over all of the active code mirrors that can provide updates
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

(reg-event-fx
  ::set-part-in-whole
  (fn [{:keys [db]} [_]]
    (let [cm         (get-in db [:defn.form.cm :cm])
          whole-text (whole-form-updated db)
          updates    (text->spec-data whole-text)]
      (prn ::set-part-in-whole :updates updates)
      {:db               (merge db updates)
       ::fn-whole-update [cm whole-text]})))

(reg-fx
  ::fn-parts-update
  (fn [{:keys [arity-data]}]
    (re-frame/dispatch [::fn-fixed-items-update-cms])
    (doall (map-indexed (fn [index data]
                          (re-frame/dispatch [::fn-arity-n-update-cms data index]))
                        arity-data))))

(reg-event-fx
  ::transact-whole-defn-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (let [updates (text->spec-data whole-form-text)]
      (prn ::transact-whole-defn-form :updates updates)
      {:db               (merge db updates)
       ::fn-parts-update updates})))

(defn- dispatch-parts-updates
  [arity-data]
  (re-frame/dispatch [::fn-fixed-items-update-cms])
  (doall (map-indexed (fn [index data]
                        (re-frame/dispatch [::fn-arity-n-update-cms data index]))
                      arity-data)))

(reg-fx
  ::fn-view-update
  (fn [[cm whole-form-text {:keys [arity-data]}]]
    (let [tx (->> (zprint-file-str whole-form-text ::fn-view-update)
                  (replacement-tx cm))]
      (update-cm cm tx)
      (dispatch-parts-updates arity-data))))

(reg-event-fx
  ::set-view
  (fn [{:keys [db]} [_ var-id]]
    (when-let [cm (get-in db [:defn.form.cm :cm])]
      (let [var-data       (db var-id)
            conformed-data (:ref-conformed var-data)
            var-name       (:ref-name var-data)
            visibility     {:the-defn-form   var-name
                            :visible-form-id var-id}
            defaults       {:meta nil :docstring nil}       ;; lift these so others can use?
            updates        (merge defaults (conformed->spec-data conformed-data))]
        {:db              (merge db visibility updates)
         ::fn-view-update [cm (:defn.text updates) updates]}))))

;; Organize it to match the life-cycle:

; event to set the whole view of a specific form-id
; event set the parts view of the whole
; event to transact changes (keystrokes) on the whole form
; --> event to ripple out change to appropriate part
; events to transact changes (keystrokes) on any of the form parts
; --> event to reflect back the part change to whole
; event to persist changes when form is changed




