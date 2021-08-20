(ns repl.repl.views.login
  (:require
    [reagent.core :as reagent]
    [re-frame.core :as re-frame]
    [re-com.core :refer [h-box v-box button gap border label input-text
                         modal-panel title radio-button]]
    [repl.repl.events :as events]
    [repl.repl.subs :as subs]
    [repl.repl.user :as user]))

(defn login-form
  [form-data process-ok]
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
             [[title :label "Login to repl" :level :level2]
              [gap :size "30px"]
              [title :label "User name:" :level :level3]
              [input-text
               :width "100%"
               :model (:user @form-data)
               :placeholder "Your name"
               :on-change #(swap! form-data assoc ::user/name %)]
              [title :label "Team name:" :level :level3]
              [input-text
               :width "100%"
               :model (:team-name @form-data)
               :on-change #(swap! form-data assoc :team-name %)]
              [title :label "Team secret:" :level :level3]
              [input-text
               :width "100%"
               :model (:secret @form-data)
               :on-change #(swap! form-data assoc :secret %)]
              [gap :size "30px"]
              [h-box
               :size "auto"
               :children
               [[button :label "Access" :class "btn-success"
                 :on-click process-ok]
                [gap :size "100px"]
                [:img {:alt   "Welcome to repl"
                       :width "75px" :height "75px"
                       :src   "/images/reptile-logo-yellow-transparent.png"}]]]]]]]])

;; TODO - fix for Team Server approach
(defn authenticate
  []
  (let [logged-in (re-frame/subscribe [::subs/logged-in])]
    (fn []
      (when-not @logged-in
        (let [form-data  (reagent/atom {:team-name "team-name"
                                        :secret    "team-secret"})
              process-ok (fn [] (re-frame/dispatch
                                  [::events/login @form-data]))]
          [modal-panel
           :backdrop-color "lightblue"
           :backdrop-opacity 0.1
           :child [login-form form-data process-ok]])))))

