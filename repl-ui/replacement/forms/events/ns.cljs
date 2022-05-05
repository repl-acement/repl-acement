(ns replacement.forms.events.ns
  "An `ns` form scopes the vars within it. The

  This namespace is concerned with breaking def forms down using `spec/conform` and putting
  them back together with `unform`. Editing by a human or a function can happen in between."
  (:require
    [cljs.tools.reader.edn :as rdr]
    [cljs.spec.alpha :as s]
    [clojure.core.async]
    [clojure.string :as string]
    [clojure.walk :as walk]
    [nextjournal.clojure-mode.extensions.formatting :as format]
    [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.forms.events.common :as common]
    [replacement.protocol.cljs-fn-specs :as core-fn-specs]
    [replacement.protocol.data :as data-specs]
    [replacement.structure.wiring :as wiring]
    [replacement.ui.helpers :refer [js->cljs]]
    [zprint.core :refer [zprint-file-str]]))


(reg-fx
  ::fn-part-update
  (fn [[cm tx changed?]]
    (if changed?
      (common/update-cm! cm tx [::set-part-in-whole])
      (common/update-cm! cm tx))))

(reg-event-fx
  ::part-edit
  (fn [{:keys [db]} [_ part-cm-name tx]]
    (let [cm (get-in db [part-cm-name :cm])
          changed? (js->cljs (.-docChanged tx))]
      {:db              db
       ::fn-part-update [cm tx changed?]})))

(reg-fx
  ::whole-edit
  (fn [[cm tx changed?]]
    (if changed?
      (common/update-cm! cm tx [::transact-whole-form (common/extract-tx-text tx)])
      (common/update-cm! cm tx))))

(reg-event-fx
  ::whole-form-tx
  (fn [{:keys [db]} [_ cm-name tx]]
    (let [cm (get-in db [cm-name :cm])
          changed? (js->cljs (.-docChanged tx))]
      {:db          db
       ::whole-edit [cm tx changed?]})))

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (assoc db comp-name {:cm cm :name comp-name})))

(defn- unformed-lib
  [conformed-libspec]
  (s/unform ::core-fn-specs/libspec conformed-libspec))

(defn- unformed-require
  [conformed-require]
  (s/unform ::core-fn-specs/ns-require conformed-require))

(defn shortened-ns-name
  [s]
  (let [parts (string/split s #"\.")
        drop-junk (if (seq (some #{"alpha" "core"} (vector (last parts))))
                    (butlast parts)
                    parts)]
    (if (= 1 (count drop-junk))
      (str (first drop-junk))
      (-> (->> (map first (butlast drop-junk))
               (interpose ".")
               (apply str))
          (str "." (last drop-junk))))))

(defn- ns-require->properties
  [{:keys [lib options] :as x}]
  {:lib    lib
   :alias  (:as options)
   :refers (:refer options)})

(defn- ns-basic-data->properties
  [{:keys [ns-name docstring]}]
  {:name      ns-name
   :docstring docstring})

(defn- split-ns
  [conformed-ns-data]
  (let [ns-data (ns-basic-data->properties (:ns-args conformed-ns-data))
        libs (atom [])
        _walked (walk/postwalk
                  (fn [node]
                    (when (and (vector? node) (= :libspec (first node)))
                      (let [lib (last node)
                            unformed (unformed-lib lib)]
                        (swap! libs conj {:conformed (last node)
                                          :unformed  unformed
                                          :text      (pr-str unformed)
                                          :require   (ns-require->properties (last lib))})))
                    node)
                  conformed-ns-data)]
    (merge ns-data {:require-libs @libs})))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (map (fn [{:keys [cm tx]}] (common/update-cm! cm tx)) changes))))

(defn update-cm-states
  [db defn-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [defn-property-name (wiring/cm-name->comp-name cm-key)
          new-text (pr-str (get defn-data defn-property-name))
          tx (common/replacement-tx cm new-text)]
      (conj cms {:cm cm :tx tx}))
    cms))

(def parts [:name :docstring])
(def clauses [:refer-clojure :require :require-macros :import :use :use-macros])

(reg-event-fx
  ::items-update-cms
  (fn [{:keys [db]} [_]]
    (let [cm-keys (map wiring/comp-name->cm-name parts)
          defn-data (:ns.parts db)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(defn- text->spec-data
  [text]
  (let [data (rdr/read-string text)
        conformed (s/conform ::data-specs/ns-form data)
        explain-data (and (= s/invalid? conformed) (s/explain-data ::data-specs/ns-form data))
        unformed (or (= s/invalid? conformed) (s/unform ::data-specs/ns-form conformed))]
    {:ns.text         text
     :ns.conformed    conformed
     :ns.explain-data explain-data
     :ns.unformed     unformed}))

(defn conformed-form->spec-data
  [{:keys [conformed]}]
  (let [unformed (when-not (s/invalid? conformed)
                   (s/unform ::data-specs/form conformed))]
    {:text         (-> unformed pr-str common/fix-width-format)
     :conformed    conformed
     :explain-data (when (s/invalid? conformed) (s/explain-data ::data-specs/form conformed))
     :unformed     unformed}))

(defn conformed->spec-data
  [conformed]
  (let [unformed (when-not (= s/invalid? conformed)
                   (s/unform ::data-specs/ns-form conformed))]
    {:ns.text         (pr-str unformed)
     :ns.conformed    conformed
     :ns.explain-data nil
     :ns.unformed     unformed
     :ns.parts        (split-ns conformed)}))

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
  ::whole-update
  (fn [[cm whole-text]]
    (let [tx (->> (zprint-file-str whole-text ::whole-update)
                  (common/replacement-tx cm))]
      (common/update-cm! cm tx))))

(defn- whole-form-updated
  "Scan over all of the active code mirrors that can provide updates
  and create the new form to reflect any updates"
  [db]
  (let [fixed-parts (common-parts-text db)
        form-text (apply str fixed-parts)]
    (str "(ns " form-text ")")))

(reg-event-fx
  ::set-part-in-whole
  (fn [{:keys [db]} [_]]
    (let [cm (get-in db [:defn.form.cm :cm])
          whole-text (whole-form-updated db)
          updates (text->spec-data whole-text)]
      {:db            (merge db updates)
       ::whole-update [cm whole-text]})))

(reg-fx
  ::parts-update
  (fn [{:keys [arity-data]}]
    (re-frame/dispatch [::items-update-cms])
    (doall (map-indexed (fn [index data]
                          (re-frame/dispatch [::fn-arity-n-update-cms data index]))
                        arity-data))))

(reg-event-fx
  ::transact-whole-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (let [updates (text->spec-data whole-form-text)]
      {:db            (merge db updates)
       ::parts-update updates})))

(reg-fx
  ::view-update
  (fn [[cm whole-text]]
    (let [tx (->> (zprint-file-str whole-text "whatever" {:width 60})
                  (common/replacement-tx cm))]
      (common/update-cm! cm tx))
    (re-frame/dispatch [::items-update-cms])))

(reg-event-fx
  ::set-view
  (fn [{:keys [db]} [_ var-id]]
    (when-let [cm (get-in db [:ns.form.cm :cm])]
      (let [var-data (db var-id)
            conformed-data (:ref-conformed var-data)
            var-name (:ref-name var-data)
            updates (conformed->spec-data conformed-data)]
        {:db           (merge db {:the-ns-form     var-name
                                  :visible-form-id var-id} updates)
         ::view-update [cm (:ns.text updates)]}))))

