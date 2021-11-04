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
    [clojure.spec.alpha :as s]
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
(def defn-common-parts [::defn.name ::defn.docstring ::defn.meta])
(def defn-arity-parts [::defn.params ::defn.prepost ::defn.body])
(def defn-all-parts (concat defn-common-parts defn-arity-parts))

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
        body-value        (last params+body-value)]
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
    (merge conformed-defn-args
           {:single-arity? single-arity?
            :arity-data    arity-data})))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (map (fn [{:keys [cm tx]}] (update-cm cm tx)) changes))))

(defn- arity-data->properties
  [arity-data]
  {:defn.params  (:params-value arity-data)
   :defn.prepost (:pre-post-map arity-data)
   :defn.body    (:body arity-data)})

(defn- defn-data->properties
  [defn-data]
  {:defn.name      (:fn-name defn-data)
   :defn.docstring (:docstring defn-data)
   :defn.meta      (:meta defn-data)})

(defn update-cm-states
  [db defn-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [defn-property-name (wiring/cm-name->comp-name cm-key)
          new-text           (pr-str (get defn-data defn-property-name))
          tx                 (replacement-tx cm new-text)]
      (conj cms {:cm cm :tx tx}))
    cms))

(reg-event-fx
  ::fn-fixed-items-update-cms
  (fn [{:keys [db]} [_]]
    (let [cm-keys          (map wiring/comp-name->cm-name defn-common-parts)
          defn-data        (defn-data->properties db)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(reg-event-fx
  ::fn-arity-n-update-cms
  (fn [{:keys [db]} [_ per-arity-data index]]
    (let [cm-keys          (map (partial wiring/indexed-comp-name->cm-name index) defn-arity-parts)
          defn-data        (arity-data->properties per-arity-data)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(defn- text->spec-data
  [text]
  (let [data          (rdr/read-string text)
        conformed     (s/conform ::form-specs/defn data)
        explain-data  (and (= s/invalid? conformed) (s/explain-data ::form-specs/defn data))
        unformed      (or (= s/invalid? conformed) (s/unform ::form-specs/defn conformed))
        fn-properties {:defn.text         text
                       :defn.conformed    conformed
                       :defn.explain-data explain-data
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
  (->> (map wiring/comp-name->cm-name defn-common-parts)
       (cm-keys->text db)))

(defn- arity-text
  [db index per-arity-data]
  (let [defn-data (arity-data->properties per-arity-data)]
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
          updates    (text->spec-data whole-text)
          formatted  (zprint-file-str whole-text ::set-part-in-whole)]
      {:db               (merge db updates)
       ::fn-whole-update [cm formatted]})))

(reg-fx
  ::fn-parts-update
  (fn [{:keys [arity-data]}]
    (re-frame/dispatch [::fn-fixed-items-update-cms])
    (doall (map-indexed (fn [index data]
                          (re-frame/dispatch [::fn-arity-n-update-cms data index]))
                        arity-data))))

(reg-event-db
  ::set-fn-id
  (fn [db [_ whole-form-text]]
    (let [{:keys [defn-args]} (->> whole-form-text
                                   (rdr/read-string)
                                   (s/conform ::form-specs/defn))
          {:keys [fn-name]} (split-defn-args defn-args)]
      (assoc db :fn-id (gensym fn-name)))))

(reg-event-fx
  ::transact-whole-defn-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (let [updates (text->spec-data whole-form-text)]
      {:db               (merge db updates)
       ::fn-parts-update updates})))
