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
  [ref-name ref-type ref-data]
  (let [ref-id (str (random-uuid))]
    {ref-id {:ref-name ref-name
             :ref-type ref-type
             :ref-data ref-data}
     :x-ref {ref-id ref-name}}))

;;TO FIX
(reg-event-db
  ::ns-forms
  (fn [db [_ forms]]
    (let [the-ns-name (first forms)
          ns-refs     (second forms)
          ref-id-data (map (fn [[_ ref-name ref-type ref-data]]
                             (ref-data->ref-id-data ref-name ref-type ref-data))
                           ns-refs)
          ordered-refs (mapv :x-ref ref-id-data)]
      (cljs.pprint/pprint [:refs  ordered-refs])
      (apply merge db {:ns-forms    forms
                       :the-ns-name the-ns-name} ref-id-data))))

