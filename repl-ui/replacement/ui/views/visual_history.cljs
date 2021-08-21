(ns replacement.ui.views.visual-history
  (:require
    [clojure.string :as string]
    [reagent.core :as reagent]
    [re-frame.core :as re-frame]
    [re-com.core :refer [h-box v-box box button gap line scroller border label input-text md-circle-icon-button
                         md-icon-button input-textarea modal-panel h-split v-split title flex-child-style
                         popover-anchor-wrapper popover-content-wrapper radio-button p]]
    [re-com.splits :refer [hv-split-args-desc]]
    [replacement.ui.events :as events]
    [replacement.ui.subs :as subs]))


(defonce label-style {:font-family "Menlo, Lucida Console, Monaco, monospace"
                      :font-size   "11px"
                      :color       "grey"})

(defonce history-style {:padding "5px 5px 0px 10px"})

(defn history-item-component
  [{:keys [history index]}]
  (let [line-count (-> history
                       string/trim-newline
                       string/trim
                       string/split-lines
                       count)]
    [h-box :gap "5px" :align :center
     :children
     [[md-icon-button
       :md-icon-name "zmdi-caret-left-circle"
       :tooltip "Send to the editor"
       :on-click #(re-frame/dispatch [::events/from-history index])]
      [input-text
       :input-type :textarea
       :model history
       :style {:background-color :white
               :resize           :none}
       :rows (max 2 line-count)
       :width "400px"
       :on-change #(-> nil)]
      [gap :size "5px"]]]))

(defn history-browser-component
  [history]
  [v-box :gap "3px" :size "auto"
   :children
   [[label :label "Just Yours or All TBC"]
    [gap :size "10px"]
    [scroller :v-scroll :auto :h-scroll :off
     :height "400px"
     :child
     [v-box :size "auto"
      :children
      [(doall (reverse (map history-item-component history)))]]]]])

(defn browse-history
  []
  (let [history (re-frame/subscribe [::subs/input-history])]
    (fn []
      (when (seq @history)
        (let [showing? (reagent/atom false)]
          [popover-anchor-wrapper
           :showing? showing?
           :position :left-above
           :anchor [h-box :gap "10px"
                    :children
                    [[md-icon-button :tooltip "Browse history"
                      :md-icon-name "zmdi-search"
                      :size :smaller :on-click #(swap! showing? not)]]]
           :popover [popover-content-wrapper
                     :on-cancel #(swap! showing? not)
                     :body [history-browser-component @history]]])))))

; padding order is: top right bottom left

(defn editor-history
  []
  [v-box :gap "15px" :width "15px"
   :align :center :justify :end
   :style {:padding "0px 0px 20px 0px"}
   :children
   [[browse-history]]])