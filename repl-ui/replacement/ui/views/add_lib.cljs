(ns replacement.ui.views.add-lib
  (:require
    [re-frame.core :refer [subscribe dispatch]]
    [re-com.core :refer [v-box box button gap line border label p
                         input-text input-textarea modal-panel title radio-button]]
    [reagent.core :as reagent]
    [replacement.ui.events :as events]
    [replacement.ui.subs :as subs]))

(defn lib-type
  [lib-data]
  [v-box :gap "20px"
   :children [(doall (for [maven? [:maven :git]]            ;; Notice the ugly "doall"
                       ^{:key maven?}                       ;; key should be unique among siblings
                       [radio-button
                        :label (name maven?)
                        :value maven?
                        :model (if (:maven @lib-data) :maven :git)
                        :on-change #(swap! lib-data assoc :maven (= :maven %))]))]])

(defn dep-name
  [lib-data]
  [v-box :gap "10px" :children
   [[label :label "Dependency Name"]
    [input-text
     :width "350px"
     :model (:name @lib-data)
     :on-change #(swap! lib-data assoc :name %)]]])

(defn maven-dep
  [lib-data]
  [v-box :gap "10px" :children
   [[label :label "Maven Version"]
    [input-text
     :width "350px"
     :model (:version @lib-data)
     :on-change #(swap! lib-data assoc :version %)]]])

(defn git-dep
  [lib-data]
  [v-box :gap "10px" :children
   [[label :label "Repository URL"]
    [input-text
     :width "350px"
     :model (:url @lib-data)
     :on-change #(swap! lib-data assoc :url %)]
    [label :label "Commit SHA"]
    [input-text
     :width "350px"
     :model (:sha @lib-data)
     :on-change #(swap! lib-data assoc :sha %)]]])

(defn add-lib-form
  [lib-data process-ok]
  (fn []
    [border
     :border "1px solid #eee"
     :child [v-box
             :gap "30px" :padding "10px"
             :height "450px"
             :children
             [[title :label "Add a dependency to the REPL" :level :level2]
              [v-box
               :gap "10px"
               :children [[lib-type lib-data]
                          [dep-name lib-data]
                          (if (:maven @lib-data)
                            [maven-dep lib-data]
                            [git-dep lib-data])
                          [gap :size "30px"]
                          [button :label "Add" :on-click process-ok]]]]]]))

(defn add-lib-panel
  []
  (let [show-add-lib? (subscribe [::subs/show-add-lib-panel])]
    (fn []
      (when @show-add-lib?
        (let [lib-data      (reagent/atom {:name    "vvvvalvalval/supdate" ;; https://github.com/vvvvalvalval/supdate
                                           :version "0.2.3"
                                           :url     "https://github.com/vvvvalvalval/supdate.git"
                                           :sha     "c7afc460b68a32d2494f98a55d438b67dd677a2b"
                                           :maven   true})
              add-lib-event (fn [] (dispatch [::events/add-lib @lib-data]))]
          [modal-panel
           :backdrop-color "lightgray"
           :backdrop-on-click #(dispatch [::events/show-add-lib-panel false])
           :backdrop-opacity 0.7
           :child [add-lib-form lib-data add-lib-event]])))))
