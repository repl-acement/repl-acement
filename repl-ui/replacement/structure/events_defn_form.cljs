(ns replacement.structure.events-defn-form
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
    [replacement.structure.fn-specs :as fn-specs]
    [replacement.structure.core-specs :as core-specs]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.ui.wiring :as wiring]
    [zprint.core :refer [zprint-file-str]]))

(defn- text->conformed+unformed
  [text defined-spec]
  (let [data              (rdr/read-string text)
        conformed         (s/conform defined-spec data)
        conformed-explain (and (= s/invalid? conformed) (s/explain-data defined-spec data))
        unformed          (or (= s/invalid? conformed) (s/unform defined-spec conformed))]
    {:text              text
     :data              data
     :conformed         conformed
     :conformed-explain conformed-explain
     :unformed          unformed}))


(defn extract-tx-text
  [^js tx]
  (->> (.-newDoc tx) (.toJSON) js->cljs (apply str)))

(defn extract-cm-text
  [^js cm]
  (->> (-> cm .-state .-doc) (.toJSON) js->cljs (apply str)))

(defn- update-cm [cm tx & event-args]
  (.update cm #js [tx])
  (when (seq event-args)
    (re-frame/dispatch (vec event-args))))

(defn- replacement-tx
  [cm text]
  (let [cm-state   (-> cm .-state)
        doc-length (-> cm-state .-doc .-length)]
    (.update cm-state (clj->js {:changes {:from 0 :to doc-length :insert text}}))))

(reg-fx
  ::code-mirror-update-view
  (fn [[{:keys [cm]} tx]]
    (update-cm cm tx)))

(reg-event-fx
  ::code-mirror-tx
  (fn [{:keys [db]} [_ tx]]
    (let [{:keys [code-mirror-view]} db]
      {:db                       (assoc db :tx tx)
       ::code-mirror-update-view [code-mirror-view tx]})))

;; Good idea or just use the CLJ spec names?
(def defn-common-parts [::defn.name ::defn.docstring ::defn.meta])
(def defn-arity-parts [::defn.params ::defn.prepost ::defn.body])
(def defn-all-parts (concat defn-common-parts defn-arity-parts))

(reg-fx
  ::fn-part-update
  (fn [[cm tx]]
    (update-cm cm tx [::set-form-part])))

(reg-event-fx
  ::part-edit
  (fn [{:keys [db]} [_ part-cm-name tx]]
    (let [cm (get-in db [part-cm-name :cm])]
      {:db              db
       ::fn-part-update [cm tx]})))

(reg-fx
  ::fn-whole-edit
  (fn [[{:keys [cm]} tx]]
    (update-cm cm tx [::transact-whole-defn-form (extract-tx-text tx)])))

(reg-event-fx
  ::fn-whole-form-tx
  (fn [{:keys [db]} [_ cm-name tx]]
    ;(prn :fn-whole-form-tx :tx-text (extract-tx-text tx))
    (let [cm-map (get db cm-name)]
      {:db             db
       ::fn-whole-edit [cm-map tx]})))

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (assoc db comp-name {:cm cm :name comp-name})))

(reg-event-db
  ::set-result-code-mirror-view
  (fn [db [_ view]]
    (assoc db :result-code-mirror-view view)))

(defn- arity-data
  [params+body]
  (let [params+body-value (s/unform ::fn-specs/params+body params+body)
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
    (doall (map (fn [{:keys [cm tx]}]
                  (update-cm cm tx)) changes))))

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
          tx                 ^js (replacement-tx cm new-text)]
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

(reg-fx
  ::fn-whole-update
  (fn [[form-cm-map whole-text]]
    (let [cm        (:cm form-cm-map)
          formatted (zprint-file-str whole-text ::set-form-part)
          tx        ^js (replacement-tx cm formatted)]
      (update-cm cm tx))))

(defn- parts-text
  [db cm-keys]
  (->> (reduce (fn [text k]
                 (when-let [cm (get-in db [k :cm])]
                   (conj text (extract-cm-text cm))))
               [] cm-keys)
       (interpose " ")
       (apply str)))

(defn- common-parts-text
  [db]
  (parts-text db (map wiring/comp-name->cm-name defn-common-parts)))

(defn- arity-text
  [db index per-arity-data]
  (let [defn-data (arity-data->properties per-arity-data)
        cm-keys   (map (partial wiring/indexed-comp-name->cm-name index) (keys defn-data))]
    (parts-text db cm-keys)))

;; Add definition type (defn- or defn)
(reg-event-fx
  ::set-form-part
  (fn [{:keys [db]} [_]]
    (let [arity-data (:arity-data db)
          defn-type  (get-in db [:defn.conformed :defn-type])
          text       (apply str
                            "(" defn-type
                            " " (common-parts-text db)
                            " " (map-indexed (fn [index data]
                                               (arity-text db index data))
                                             arity-data)
                            ")")
          formatted  (zprint-file-str text ::set-form-part)
          whole-cm   (:defn.form.cm db)]
      {:db               db
       ::fn-whole-update [whole-cm formatted]})))

(reg-fx
  ::fn-parts-update
  (fn [{:keys [arity-data]}]
    (re-frame/dispatch [::fn-fixed-items-update-cms])
    (doall (map-indexed (fn [index data]
                          (re-frame/dispatch [::fn-arity-n-update-cms data index]))
                        (if (map? arity-data) [arity-data] arity-data)))))

(defn- text->conformed
  [text]
  (->> text (rdr/read-string)
       (s/conform ::core-specs/defn)))

(reg-event-db
  ::set-fn-id
  (fn [db [_ whole-form-text]]
    (let [{:keys [defn-args]} (text->conformed whole-form-text)
          {:keys [fn-name]} (split-defn-args defn-args)]
      (assoc db :fn-id (gensym fn-name)))))

(reg-event-fx
  ::transact-whole-defn-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (let [conformed     (text->conformed whole-form-text)
          unformed      (s/unform ::core-specs/defn conformed)
          fn-properties {:defn.text      whole-form-text
                         :defn.conformed conformed
                         :defn.unformed  unformed}
          fn-data       (split-defn-args (:defn-args conformed))
          updates       (merge fn-properties fn-data)]
      {:db               (merge db updates)
       ::fn-parts-update updates})))
