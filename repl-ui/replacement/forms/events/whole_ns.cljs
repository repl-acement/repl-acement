(ns replacement.forms.events.whole-ns
  "After reading in a namespace, events are created for managing the view"
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [clojure.spec.alpha :as s]
    [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.structure.core-fn-specs :as core-fn-specs]
    [replacement.structure.form-specs :as form-specs]
    [replacement.xform.defn :as xform-defn]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.structure.wiring :as wiring]
    [zprint.core :refer [zprint-file-str]]
    [re-frame.core :as re-frame]))

(defn- ref-data->ref-id-data
  [{:keys [ns ref-name ref-type] :as ref-data}]
  (let [ref-id (str (random-uuid))]
    {ref-id    ref-data
     :id-index {ref-id {:ns   ns
                        :type ref-type
                        :name ref-name}}}))

(reg-event-db
  ::update-ref-data
  (fn [{:keys [id-index] :as db} [_ id ref-data]]
    (let [index-entry (select-keys ref-data [:ns :ref-name])]
      (assoc db id ref-data
                :id-index (merge id-index index-entry)))))

(reg-event-db
  ::ns-forms
  (fn [{:keys [id-index] :as db} [_ forms]]
    (let [the-ns-name  (first forms)
          ns-refs      (second forms)
          form-data    {:ns-forms    forms
                        :the-ns-name the-ns-name}
          ref-id-data  (map (fn [[ref-ns ref-name ref-type ref-conformed]]
                              (ref-data->ref-id-data {:ns            ref-ns
                                                      :ref-name      ref-name
                                                      :ref-type      ref-type
                                                      :ref-conformed ref-conformed}))
                            ns-refs)
          index-update (apply merge (map :id-index ref-id-data))]
      (cljs.pprint/pprint [:ns-forms :index-update index-update])
      (apply merge db form-data
             {:id-index (merge id-index index-update)}
             (map #(dissoc % :id-index) ref-id-data)))))

(defn apply-join
  [forms-from-ns joins]
  (doall (-> (map (fn [[form-id {:keys [ns type]}]]
                    (reduce (fn [matching-forms {:keys [ns-regex form-types]}]
                              (if (and (re-find ns-regex (str ns))
                                       (some form-types [type]))
                                (conj matching-forms form-id)
                                matching-forms))
                            [] joins))
                  forms-from-ns)
             (flatten))))


(defn apply-defn-transforms [[defn-form-id defn-form] {:keys [actions meta]}]
  (let [xformed (reduce (fn [transformed-form action]
                          (xform-defn/xform-conformed-defn-data transformed-form action meta))
                        (:ref-conformed defn-form) (:apply actions))
        updated-defn-form (assoc defn-form :ref-conformed xformed)]
    (re-frame/dispatch [::update-form-by-id defn-form-id updated-defn-form])))

(reg-event-db
  ::update-form-by-id
  (fn [db [_ id updated-form]]
    (-> (assoc db id updated-form)
        (update-in [:form-history id] conj updated-form))))

(reg-event-db
  ::defn-transforms-toggle
  (fn [db [_ xform-key enabled?]]
    ;; TODO -- rollback history to remove the transforms when params are toggled
    ;; can get more subtle but just reverting works for now
    (let [the-ns-name      (:the-ns-name db)
          ns-forms         (filter #(= (:ns (val %)) the-ns-name) (:id-index db))
          updated-db       (assoc-in db [:xforms xform-key :enabled?] enabled?)
          active-xforms    (reduce (fn [active-xforms xform-key]
                                     (let [xform (get-in updated-db [:xforms xform-key])]
                                       (if (:enabled? xform)
                                         (conj active-xforms xform)
                                         active-xforms)))
                                   [] (keys (get-in updated-db [:xforms])))
          joined-forms+ids (->> (doall (map #(apply-join ns-forms (:joins %)) active-xforms))
                                (flatten)
                                (map (fn [form-id]
                                       [form-id (db form-id)])))]
      (doall (map (fn [xform]
                    (doall (map #(apply-defn-transforms % xform) joined-forms+ids)))
                  active-xforms))
      updated-db)))
