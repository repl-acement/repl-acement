(ns replacement.forms.events.def
  "A `def` form creates a global variable and can have an expression to set the initial value.

  This namespace is concerned with breaking def forms down using `spec/conform` and putting
  them back together with `unform`. Editing by a human or a function can happen in between."
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [cljs.spec.alpha :as s]
    [nextjournal.clojure-mode.extensions.formatting :as format]
    [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.structure.core-fn-specs :as core-fn-specs]
    [replacement.structure.form-specs :as form-specs]
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

;; Good idea or just use the CLJ spec names?
(def def-parts [::def.name ::def.docstring ::def.init-expr])

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

(defn- arity-data
  [params+body]
  (let [params+body-value (s/unform ::core-fn-specs/params+body params+body)
        params-value      (first params+body-value)
        pp?               (map? (second params+body-value))
        pp                (when pp? (second params+body-value))
        body-value        (last params+body-value)]
    {:params-value params-value
     :body         body-value
     :pre-post-map pp}))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (map (fn [{:keys [cm tx]}]
                  (update-cm cm tx))
                changes))))

(defn- def-data->properties
  [def-data]
  {:def.name      (:var-name def-data)
   :def.docstring (:docstring def-data)
   :def.init-expr (:init-expr def-data)})

(defn update-cm-states
  [db def-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [def-property-name (wiring/cm-name->comp-name cm-key)
          new-text          (pr-str (get def-data def-property-name))
          tx                (replacement-tx cm new-text)]
      (conj cms {:cm cm :tx tx}))
    cms))

(reg-event-fx
  ::def-fixed-items-update-cms
  (fn [{:keys [db]} [_]]
    (let [cm-keys          (map wiring/comp-name->cm-name def-parts)
          def-data         (def-data->properties db)
          cms-with-changes (reduce (partial update-cm-states db def-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(defn- text->spec-data
  [text]
  (let [data         (rdr/read-string text)
        conformed    (s/conform ::form-specs/def data)
        explain-data (and (= s/invalid? conformed) (s/explain-data ::form-specs/def data))
        unformed     (or (= s/invalid? conformed) (s/unform ::form-specs/def conformed))]
    {:def.text         text
     :def.conformed    conformed
     :def.explain-data explain-data
     :def.unformed     unformed}))

(defn- conformed-def->spec-data
  [conformed]
  (let [unformed (when-not (= s/invalid? conformed)
                   (s/unform ::form-specs/def conformed))]
    {:def.text         (pr-str unformed)
     :def.conformed    conformed
     :def.explain-data nil
     :def.unformed     unformed}))

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
  (->> (map wiring/comp-name->cm-name def-parts)
       (cm-keys->text db)))

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
  (let [fixed-parts (common-parts-text db)
        form-text   (apply str fixed-parts)]
    (str "(def " form-text ")")))

(reg-event-fx
  ::set-part-in-whole
  (fn [{:keys [db]} [_]]
    (let [cm         (get-in db [:def.form.cm :cm])
          whole-text (whole-form-updated db)
          updates    (text->spec-data whole-text)]
      {:db               (merge db updates)
       ::fn-whole-update [cm whole-text]})))

(reg-fx
  ::fn-parts-update
  (fn [{:keys [arity-data]}]
    (re-frame/dispatch [::def-fixed-items-update-cms])
    (doall (map-indexed (fn [index data]
                          (re-frame/dispatch [::fn-arity-n-update-cms data index]))
                        arity-data))))

(reg-event-fx
  ::transact-whole-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (let [updates (text->spec-data whole-form-text)]
      {:db               (merge db updates)
       ::fn-parts-update updates})))

(reg-fx
  ::fn-view-update
  (fn [[cm whole-text]]
    (let [tx (->> (zprint-file-str whole-text ::fn-view-update)
                  (replacement-tx cm))]
      (update-cm cm tx))
    (re-frame/dispatch [::def-fixed-items-update-cms])))

(reg-event-fx
  ::set-def-view
  (fn [{:keys [db]} [_ var-id]]
    (let [cm             (get-in db [:def.form.cm :cm])
          var-data       (db var-id)
          conformed-data (:ref-conformed var-data)
          var-name       (:ref-name var-data)
          updates        (conformed-def->spec-data conformed-data)]
      {:db              (merge db {:the-def-form    var-name
                                   :visible-form-id var-id} conformed-data updates)
       ::fn-view-update [cm (:def.text updates)]})))

