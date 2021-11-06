(ns replacement.forms.events.whole-ns
  "After reading in a namespace, events are created for managing the view"
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [clojure.spec.alpha :as s]
    [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.structure.core-fn-specs :as core-fn-specs]
    [replacement.structure.form-specs :as form-specs]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.structure.wiring :as wiring]
    [zprint.core :refer [zprint-file-str]]))

(defn- ref-data->ref-id-data
  [{:keys [ns ref-name] :as ref-data}]
  (let [ref-id (str (random-uuid))]
    {ref-id    ref-data
     :id-index {ref-id {:ns   ns
                        :name ref-name}}}))

(reg-event-db
  ::update-ref-data
  (fn [{:keys [id-index] :as db} [_ id ref-data]]
    (let [index-entry (select-keys ref-data [:ns :ref-name])]
      (assoc db id ref-data
                :id-index (merge id-index index-entry)))))

;; TODO - retain order of forms within the NS
(reg-event-db
  ::ns-forms
  (fn [{:keys [id-index] :as db} [_ forms]]
    (let [the-ns-name  (first forms)
          ns-refs      (second forms)
          form-data    {:ns-forms    forms
                        :the-ns-name the-ns-name}
          ref-id-data  (map (fn [[_ ref-name ref-type ref-conformed]]
                              (ref-data->ref-id-data {:ns            the-ns-name
                                                      :ref-name      ref-name
                                                      :ref-type      ref-type
                                                      :ref-conformed ref-conformed}))
                            ns-refs)
          index-update (apply merge (map :id-index ref-id-data))]
      (apply merge db form-data
             {:id-index (merge id-index index-update)}
             (map #(dissoc % :id-index) ref-id-data)))))

(reg-event-db
  ::visible-form
  (fn [db [_ var-id]]

    (prn :id var-id :data (get db var-id))))

