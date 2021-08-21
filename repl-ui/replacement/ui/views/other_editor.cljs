(ns replacement.ui.views.other-editor
  (:require
    [re-frame.core :as re-frame :refer [subscribe]]
    [re-com.core :refer [box h-box v-box gap label
                         md-icon-button slider flex-child-style]]
    [re-com.splits :refer [hv-split-args-desc]]
    [reagent.core :as reagent]
    [reagent.dom :as rdom]
    [replacement.ui.helpers :as helpers]
    [replacement.ui.events :as events]
    [replacement.ui.code-mirror :as code-mirror]
    [replacement.specs.user :as user]))

(defonce other-editors-style {:padding "20px 20px 20px 10px"})

(defn other-editor-did-mount
  [user]
  (fn [this]
    (let [node        (rdom/dom-node this)
          options     {:options {:lineWrapping true
                                 :readOnly     true}}
          code-mirror (code-mirror/parinfer node options)]
      (re-frame/dispatch [::events/other-user-code-mirror code-mirror user]))))

;; TODO visibility toggle ... we never get here cos react
(defn other-component
  [user]
  (let [editor-name (::user/name user)]
    (reagent/create-class
      {:component-did-mount  (other-editor-did-mount user)
       :reagent-render       (code-mirror/text-area editor-name)
       :component-did-update #(-> nil)                      ; noop to prevent reload
       :display-name         (str "network-editor-" editor-name)})))

; 1 - use the outer / inner pattern
; 2 - outer component to subscribe on visibility

(defonce other-panel-style
         (merge (flex-child-style "1")
                {:font-family "Menlo, Lucida Console, Monaco, monospace"
                 :font-size   "10px"
                 :color       "lightgrey"
                 :border      "0.5px solid lightgrey"
                 :padding     "5px 5px 5px 5px"}))

(defn other-panel
  [user style]
  (let [user-name (::user/name (last user))]
    [v-box :size "auto" :children
     [[box :size "auto" :style other-panel-style
       :child
       [other-component user]]
      [h-box :size "20px" :padding "5px"
       :justify :between :align :center :style style
       :children
       [[label :label user-name]
        [h-box :gap "5px" :children
         [(helpers/icon-button "keyboard")
          ;; TODO make this dynamic to reflect incoming keystrokes
          (rand-nth
            [(helpers/icon-button "comment-more")
             (helpers/icon-button "comment-outline")])]]]]]]))

(defn waiting-panel
  []
  (let [panel-style (select-keys other-panel-style [:font-family :color])
        large       (assoc panel-style :font-size "20px")
        medium      (assoc panel-style :font-size "12px")]
    [v-box :size "auto" :align :center :justify :around
     :style other-panel-style :children
     [[label :style large :label "WHERE ARE THE OTHERS️?"]
      [label :style medium :label "Hint: drag to change the size of the window"]]]))

(defn other-panels
  [other-users]
  (let [border {:border "1px solid lightgrey"}
        gray   (merge {:background-color "lightgrey"
                       :color            :black}
                      border)
        black  (merge {:background-color :black
                       :color            :white}
                      border)]
    [h-box :gap "2px" :size "auto" :children
     (vec (map #(other-panel %1 %2) other-users (cycle [gray black])))]))
