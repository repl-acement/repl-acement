(ns replacement.ui.views.editor
  (:require
    [re-frame.core :refer [subscribe dispatch]]
    [re-com.core :refer [h-box v-box box button gap line scroller border label input-text md-circle-icon-button
                         md-icon-button input-textarea h-split v-split popover-anchor-wrapper
                         popover-content-wrapper title flex-child-style p slider]]
    [re-com.splits :refer [hv-split-args-desc]]
    [reagent.core :as reagent]
    [reagent.dom :as rdom]
    [replacement.specs.user :as user]
    [replacement.ui.code-mirror :as code-mirror]
    [replacement.ui.events :as events]
    [replacement.ui.subs :as subs]
    [replacement.ui.views.other-editor :as other-editor]
    [replacement.ui.views.add-lib :as add-lib]
    [replacement.ui.views.show-team-data :as team]
    [replacement.ui.views.eval :as eval-view]
    [replacement.ui.views.status :as status]))

(defonce default-style {:font-family   "Menlo, Lucida Console, Monaco, monospace"
                        :border-radius "3px"
                        :border        "1px solid lightgrey"
                        :padding       "5px 5px 5px 5px"})

(defonce eval-panel-style (merge (flex-child-style "1")
                                 default-style))

(defonce edit-style (assoc default-style :border "2px solid lightgrey"))
(defonce edit-panel-style (merge (flex-child-style "1") edit-style))

(defonce status-style (merge (dissoc default-style :border)
                             {:font-size   "10px"
                              :font-weight "lighter"
                              :color       "lightgrey"}))

(defn notify-edits
  [new-value]
  (dispatch [::events/current-form new-value]))

(defn editor-did-mount
  [extra-key-bindings]
  (fn [this-textarea]
    (let [node        (rdom/dom-node this-textarea)
          options     {:options {:lineWrapping  true
                                 :autofocus     true
                                 :matchBrackets true
                                 :lineNumbers   true
                                 :extraKeys     extra-key-bindings}}
          code-mirror (code-mirror/parinfer node options)]

      (.on code-mirror "change" (fn [cm _co]
                                  (notify-edits (.getValue cm))))

      (dispatch [::events/code-mirror code-mirror]))))

(defn edit-component
  [panel-name extra-key-bindings]
  (reagent/create-class
    {:component-did-mount  (editor-did-mount extra-key-bindings)
     :reagent-render       (code-mirror/text-area panel-name)
     :component-did-update #(-> nil)                        ; noop to prevent reload
     :display-name         "local-editor"}))

(defn- key-binding
  [key-map [button event]]
  (assoc {} (get key-map button) #(dispatch [event])))

(defn extra-key-bindings
  [key-map event-map]
  (apply merge (map (partial key-binding key-map) event-map)))

(def event-bindings
  {:enter ::events/eval
   :right ::events/history-next
   :down  ::events/history-next
   :left  ::events/history-prev
   :up    ::events/history-prev})

(defn edit-panel
  [user]
  (let [key-bindings (subscribe [::subs/key-bindings])]
    (fn []
      (let [editor-name (::user/name user)
            extra-keys  (extra-key-bindings @key-bindings event-bindings)]
        [v-box :size "auto" :children
         [[box :size "auto"
           :style edit-panel-style
           :child [edit-component editor-name extra-keys]]
          [gap :size "5px"]
          [h-box :children
           [[button
             :label (str "Eval")
             :tooltip (str "Shortcut: " (some-> (:enter @key-bindings) name))
             :class "btn-success"
             :on-click #(dispatch [::events/eval])]
            [gap :size "5px"]]]]]))))

(defn others-panel
  [other-users]
  (let [visible-count (count other-users)]
    (if (and visible-count (> visible-count 0))
      [other-editor/other-panels other-users]
      [other-editor/waiting-panel])))

(defn button-row
  []
  [h-box :height "20px"
   :style other-editor/other-editors-style
   :justify :between
   :children
   [[box :align :center :justify :start
     :child
     [md-icon-button :size :smaller :md-icon-name "zmdi-power"
      :tooltip "Logout"
      :on-click #(dispatch [::events/logout])]]
    [h-box :align :center
     :children
     [[add-lib/add-lib-panel]
      [md-icon-button :size :smaller :md-icon-name "zmdi-file-plus"
       :tooltip "Add a dependency"
       :on-click #(dispatch [::events/show-add-lib-panel true])]]]
    [h-box :align :center
     :children
     [[team/team-data-panel]
      [md-icon-button :size :smaller :md-icon-name "zmdi-account-add"
       :tooltip "REPL session invite link"
       :on-click #(dispatch [::events/show-team-data true])]]]
    [box :align :center :justify :start
     :child
     [md-icon-button :size :smaller :md-icon-name "zmdi-accounts-outline" ;; Toggle: zmdi-accounts / zmdi-accounts-outline
      :tooltip "Hide / Show other editors"
      :on-click #(dispatch [::events/toggle-others])]]]])

(defn editor-repl
  [user]
  [h-split :margin "5px" :splitter-size "5px"
   :panel-1 [edit-panel user]
   :panel-2 [v-box :style eval-panel-style
             :children [[eval-view/eval-panel user]]]])

;; BUG - when other users login or logout
;; all evals and current input is lost
(defn main-panels
  [user]
  (let [other-users @(subscribe [::subs/other-users])]
    [v-box :style {:position "absolute"
                   :top      "0px"
                   :bottom   "0px"
                   :width    "100%"}
     :children
     [[button-row]
      (if-not (seq other-users)
        [editor-repl user]
        [v-split :initial-split 20 :splitter-size "5px"
         :panel-1 [others-panel other-users]
         :panel-2 [editor-repl user]])
      [status/status-bar user]]]))
