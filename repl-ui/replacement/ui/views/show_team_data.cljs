(ns replacement.ui.views.show-team-data
  (:require
    [re-frame.core :refer [subscribe dispatch]]
    [re-com.core :refer [v-box h-box box button gap line border label p
                         input-text input-textarea modal-panel title radio-button]]
    [replacement.ui.events :as events]
    [replacement.ui.subs :as subs]))

(defn team-data-modal
  [{:keys [team-name team-secret]}]
  [border
   :border "1px solid #eee"
   :child [v-box
           :width "400px"
           :size "auto"
           :padding "10px"
           :children
           [[v-box
             :gap "10px"
             :children
             [[title :label "Team data, so others can join" :level :level2]
              [gap :size "30px"]
              [title :label "Team name:" :level :level3]
              [label
               :width "100%"
               ;; Add copy to clipboard function
               :label team-name]
              [title :label "Team secret:" :level :level3]
              [label
               :width "100%"
               ;; Add copy to clipboard function
               :label team-secret]
              [gap :size "30px"]
              [h-box
               :size "auto"
               :children
               [[button :label "Done" :class "btn-success"
                 :on-click #(dispatch [::events/show-team-data false])]
                [gap :size "100px"]
                [:img {:alt   "reptile: the shared online Clojure REPL"
                       :width "75px" :height "75px"
                       :src   "/images/reptile-logo-yellow-transparent.png"}]]]]]]]])

(defn team-data-panel
  []
  (let [team-data       (subscribe [::subs/team-data])
        show-team-data? (subscribe [::subs/show-team-data])]
    (fn []
      (when @show-team-data?
        (println :team-data @team-data)
        [modal-panel
         :backdrop-color "lightgray"
         :backdrop-on-click #(dispatch [::events/show-team-data false])
         :backdrop-opacity 0.7
         :child [team-data-modal @team-data]]))))
