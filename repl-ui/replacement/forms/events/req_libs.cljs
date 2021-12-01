(ns replacement.forms.events.req-libs
  "An `require` form scopes the vars within it. The

  This namespace is concerned with breaking require forms down using `spec/conform` and putting
  them back together with `unform`. Editing by a human or a function can happen in between."
  (:require
    [clojure.core.async]
    [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.forms.events.common :as common]
    [replacement.structure.wiring :as wiring]
    [replacement.ui.helpers :refer [js->cljs]]
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

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (assoc db comp-name {:cm cm :name comp-name})))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (map (fn [{:keys [cm tx]}] (common/update-cm cm tx)) changes))))

(defn update-cm-states
  [db defn-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [defn-property-name (wiring/cm-name->comp-name cm-key)
          new-text           (pr-str (get defn-data defn-property-name))
          tx                 (common/replacement-tx cm new-text)]
      (conj cms {:cm cm :tx tx}))
    cms))

(def parts [:lib :alias :refers])

(reg-event-fx
  ::items-update-cms
  (fn [{:keys [db]} [_]]
    (prn ::items-update-cms :parts (keys (:req-lib.parts db)))
    (let [cm-keys          (map wiring/comp-name->cm-name parts)
          defn-data        (:req-lib.parts db)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))
