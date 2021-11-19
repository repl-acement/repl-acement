(ns replacement.forms.events.ns
  "An `ns` form scopes the vars within it. The

  This namespace is concerned with breaking def forms down using `spec/conform` and putting
  them back together with `unform`. Editing by a human or a function can happen in between."
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [cljs.spec.alpha :as s]
    [nextjournal.clojure-mode.extensions.formatting :as format]
    [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.forms.events.common :as common]
    [replacement.protocol.cljs-fn-specs :as core-fn-specs]
    [replacement.protocol.data :as data-specs]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.structure.wiring :as wiring]
    [zprint.core :refer [zprint-file-str]]))


(reg-fx
  ::fn-part-update
  (fn [[cm tx changed?]]
    (if changed?
      (common/update-cm cm tx [::set-part-in-whole])
      (common/update-cm cm tx))))

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
      (common/update-cm cm tx [::transact-whole-form (common/extract-tx-text tx)])
      (common/update-cm cm tx))))

(reg-event-fx
  ::whole-form-tx
  (fn [{:keys [db]} [_ cm-name tx]]
    (let [cm       (get-in db [cm-name :cm])
          changed? (js->cljs (.-docChanged tx))]
      {:db          db
       ::whole-edit [cm tx changed?]})))

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (cljs.pprint/pprint [:set-cm+name :cm cm :comp-name comp-name])
    (assoc db comp-name {:cm cm :name comp-name})))

(defn- lib-data
  [conformed-libspec]
  (let [libspec-value (s/unform ::core-fn-specs/libspec conformed-libspec)]
    (cljs.pprint/pprint [:lib-data :value libspec-value])
    {:lib-data libspec-value}))

(defn- require-data
  [conformed-require]
  (let [require-value (s/unform ::core-fn-specs/ns-require conformed-require)]
    (cljs.pprint/pprint [:require-data :value require-value])
    {:require-value require-value}))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (map (fn [{:keys [cm tx]}] (common/update-cm cm tx)) changes))))

(defn- ns-require->properties
  [ns-data]
  {:ns.lib     (:lib ns-data)
   :ns.options (:options ns-data)})

(defn- ns-basic-data->properties
  [ns-data]
  {:ns.name      (:ns-name ns-data)
   :ns.docstring (:docstring ns-data)
   :ns.clauses   (:ns-clauses ns-data)})

(defn update-cm-states
  [db defn-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [defn-property-name (wiring/cm-name->comp-name cm-key)
          new-text           (pr-str (get defn-data defn-property-name))
          tx                 (common/replacement-tx cm new-text)]
      (conj cms {:cm cm :tx tx}))
    cms))

(def parts [:ns.name :ns.docstring :ns.clauses])

(reg-event-fx
  ::def-fixed-items-update-cms
  (fn [{:keys [db]} [_]]
    (prn :def-fixed-items-update-cms :called)
    (let [cm-keys          (map wiring/comp-name->cm-name parts)
          defn-data        (ns-basic-data->properties db)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(defn- text->spec-data
  [text]
  (let [data         (rdr/read-string text)
        conformed    (s/conform ::data-specs/ns-form data)
        explain-data (and (= s/invalid? conformed) (s/explain-data ::data-specs/ns-form data))
        unformed     (or (= s/invalid? conformed) (s/unform ::data-specs/ns-form conformed))]
    {:ns.text         text
     :ns.conformed    conformed
     :ns.explain-data explain-data
     :ns.unformed     unformed}))

(defn- conformed-ns->spec-data
  [conformed]
  (let [unformed (when-not (= s/invalid? conformed)
                   (s/unform ::data-specs/ns-form conformed))]
    {:ns.text         (pr-str unformed)
     :ns.conformed    conformed
     :ns.explain-data nil
     :ns.unformed     unformed}))

(defn- cm-keys->text
  [db cm-keys]
  (->> (reduce (fn [text k]
                 (->> (get-in db [k :cm])
                      (common/extract-cm-text)
                      (conj text)))
               [] cm-keys)
       (interpose " ")
       (apply str)))

(defn- common-parts-text
  [db]
  (->> (map wiring/comp-name->cm-name parts)
       (cm-keys->text db)))

(reg-fx
  ::fn-whole-update
  (fn [[cm whole-text]]
    (let [tx (->> (zprint-file-str whole-text ::fn-whole-update)
                  (common/replacement-tx cm))]
      (common/update-cm cm tx))))

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
    (let [cm         (get-in db [:defn.form.cm :cm])
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
                  (common/replacement-tx cm))]
      (common/update-cm cm tx))
    (re-frame/dispatch [::def-fixed-items-update-cms])))

(reg-event-fx
  ::set-view
  (fn [{:keys [db]} [_ var-id]]
    (let [cm             (get-in db [:ns.form.cm :cm])
          var-data       (db var-id)
          conformed-data (:ref-conformed var-data)
          var-name       (:ref-name var-data)
          updates        (conformed-ns->spec-data conformed-data)]
      {:db              (merge db {:the-ns-form     var-name
                                   :visible-form-id var-id} updates)
       ::fn-view-update [cm (:ns.text updates)]})))

