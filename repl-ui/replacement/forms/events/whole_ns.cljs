(ns replacement.forms.events.whole-ns
  "After reading in a namespace, events are created for managing the view"
  (:require
    [clojure.core.async]
    [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.forms.events.defn :as defn-events]
    [replacement.forms.events.def :as def-events]
    [replacement.forms.events.ns :as ns-events]
    [replacement.xform.defn :as xform-defn]
    [replacement.ui.helpers :refer [js->cljs]]
    [re-frame.core :as re-frame]))

(reg-event-db
  ::current-form-type
  (fn [db [_ form-type]]
    (assoc db :current-form-type form-type)))

(reg-fx
  ::set-form-view
  (fn [{:keys [type id]}]
    (condp = type
      :def (re-frame/dispatch [::def-events/set-view id])
      :defn (re-frame/dispatch [::defn-events/set-form id])
      :ns (re-frame/dispatch [::ns-events/set-view id]))))

(defn- form-data->view-data
  [{:keys                   [type]
    {:keys [ref-conformed]} :form :as input}]
  (cljs.pprint/pprint [:input input])
  (let [form-data (condp = type
                    :def (def-events/conformed-form->spec-data {:conformed ref-conformed})
                    ;; TODO
                    :defn (defn-events/conformed->spec-data ref-conformed)
                    :ns (ns-events/conformed->spec-data ref-conformed))]
    (merge input {:form-data form-data})))

(defn- parse-form-data
  [id {:keys [ref-type ref-name ref-conformed]} type]
  (let [form-data (condp = type
                    :def (def-events/conformed->spec-data ref-conformed)
                    :defn (defn-events/conformed->spec-data ref-conformed)
                    :ns (ns-events/conformed->spec-data ref-conformed))]
    {:id   id
     :type ref-type
     :name ref-name
     :form form-data}))

(reg-event-fx
  ::current-form-data
  (fn [{:keys [db]} [_ {:keys [id type]}]]
    (let [form           (db id)
          form-data      (parse-form-data id form type)
          view-form-data (form-data->view-data (get-in db [:current-ns id]))]
      {:db             (assoc db :current-form-data form-data
                                 :view-form-data view-form-data)
       ::set-form-view form-data})))

(reg-event-fx
  ::set-current-form
  (fn [{:keys [db]} [_ id]]
    (let [form      (db id)
          form-data (parse-form-data id form type)]
      (prn :form-data form-data)
      {:db             (assoc db :current-form-data form-data)
       ::set-form-view form-data})))


(reg-event-fx
  ::swap-structured-view
  (fn [{:keys [db]} [_]]
    {:db             (assoc db :structured-view? (not (get db :structured-view?)))
     ::set-form-view (:current-form-data db)}))

(reg-event-fx
  ::set-view
  (fn [{:keys [db]} [_]]
    {:db             db
     ::set-form-view (:current-form-data db)}))

(reg-event-db
  ::update-ref-data
  (fn [{:keys [id-index] :as db} [_ id ref-data]]
    (let [index-entry (select-keys ref-data [:ns :ref-name])]
      (assoc db id ref-data
                :id-index (merge id-index index-entry)))))

(defn apply-join
  [forms-from-ns joins]
  (->> (doall (map (fn [[form-id {:keys [ns type]}]]
                     (reduce (fn [matching-forms {:keys [ns-regex form-types]}]
                               (if (and (re-find ns-regex (str ns))
                                        (some form-types [type]))
                                 (conj matching-forms form-id)
                                 matching-forms))
                             [] joins))
                   forms-from-ns))
       (flatten)))


(defn apply-defn-transforms
  [[defn-form-id defn-form] {:keys [actions meta]}]
  (let [xformed           (reduce (fn [transformed-form action]
                                    (xform-defn/xform-conformed-defn-data transformed-form action meta))
                                  (:ref-conformed defn-form) (:apply actions))
        updated-defn-form (assoc defn-form :ref-conformed xformed)]
    (re-frame/dispatch [::update-form-by-id defn-form-id updated-defn-form])))

(reg-event-db
  ::update-form-by-id
  (fn [db [_ id updated-form]]
    (-> (assoc db id updated-form)
        (update-in [:form-history id] conj updated-form))))

(reg-fx
  ::defn-transforms-on
  (fn [db]
    (let [the-ns-name      (:the-ns-name db)
          ns-forms         (filter #(= (:ns (val %)) the-ns-name) (:id-index db))
          active-xforms    (reduce (fn [active-xforms xform-key]
                                     (let [xform (get-in db [:xforms xform-key])]
                                       (if (:enabled? xform)
                                         (conj active-xforms xform)
                                         active-xforms)))
                                   [] (keys (get-in db [:xforms])))
          joined-forms+ids (->> (doall (map (fn [active-xform]
                                              (apply-join ns-forms (:joins active-xform)))
                                            active-xforms))
                                (flatten)
                                (map (fn [form-id]
                                       [form-id (db form-id)])))]
      (doall (map (fn [xform]
                    (doall (map #(apply-defn-transforms % xform) joined-forms+ids)))
                  active-xforms)))
    (re-frame/dispatch [::defn-events/set-form (:visible-form-id db)])))

(reg-fx
  ::defn-transforms-off
  (fn [db]))

;; TODO -- rollback history to remove the transforms when params are toggled
;; TODO -- retain other edits! Simply reverting only works for demo purposes
(reg-event-fx
  ::defn-transforms-toggle
  (fn [{:keys [db]} [_ xform-key enabled?]]
    (let [updated-db (assoc-in db [:xforms xform-key :enabled?] enabled?)]
      (merge {:db updated-db}
             (if enabled?
               {::defn-transforms-on updated-db}
               {::defn-transforms-off updated-db})))))