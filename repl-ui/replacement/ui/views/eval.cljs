(ns replacement.ui.views.eval
  (:require
    [re-frame.core :as re-frame]
    [re-com.core :refer [h-box v-box box button gap line border label input-text
                         md-circle-icon-button md-icon-button input-textarea
                         popover-anchor-wrapper popover-content-wrapper modal-panel
                         h-split v-split title flex-child-style radio-button p]]
    [reagent.core :as reagent]
    [reagent.dom :as rdom]
    [replacement.ui.events :as events]
    [replacement.ui.views.visual-history :as visual-history]))

; padding order is: top right bottom left
(defonce eval-panel-style (merge (flex-child-style "1")
                                 {:padding "5px 5px 5px 5px"}))

(defonce eval-component-style (merge (flex-child-style "1")
                                     {:padding "5px 5px 0px 5px"}))


(defn eval-panel
  [panel-name]
  [h-box :size "auto"
   :children
   [[box :style eval-component-style
     :child [label :label panel-name]]
    [visual-history/editor-history]]])
