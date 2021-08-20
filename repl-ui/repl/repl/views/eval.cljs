(ns repl.repl.views.eval
  (:require
    [re-frame.core :as re-frame]
    [re-com.core :refer [h-box v-box box button gap line border label input-text
                         md-circle-icon-button md-icon-button input-textarea
                         popover-anchor-wrapper popover-content-wrapper modal-panel
                         h-split v-split title flex-child-style radio-button p]]
    [reagent.core :as reagent]
    [repl.repl.subs :as subs]
    [reagent.dom :as rdom]
    [repl.repl.events :as events]
    [repl.repl.code-mirror :as code-mirror]
    [repl.repl.views.visual-history :as visual-history]))

; padding order is: top right bottom left
(defonce eval-panel-style (merge (flex-child-style "1")
                                 {:padding "5px 5px 5px 5px"}))

(defonce eval-component-style (merge (flex-child-style "1")
                                     {:padding "5px 5px 0px 5px"}))

(defn eval-did-mount
  []
  (fn [this]
    (letfn [(scrollToBottom [cm]
              (.scrollIntoView cm #js {:line (.lastLine cm)}))]
      (let [node        (rdom/dom-node this)
            options     {:options {:lineWrapping true
                                   :readOnly     true}}
            code-mirror (code-mirror/parinfer node options)]
        (.on code-mirror "change" #(scrollToBottom code-mirror))
        (re-frame/dispatch [::events/eval-code-mirror code-mirror])))))

(defn eval-component
  [panel-name]
  (reagent/create-class
    {:component-did-mount  (eval-did-mount)
     :reagent-render       (code-mirror/text-area (str "eval-" panel-name))
     :component-did-update #(-> nil)                        ; noop to prevent reload
     :display-name         "eval"}))

(defn eval-panel
  [panel-name]
  [h-box :size "auto"
   :children
   [[box :style eval-component-style
     :child [eval-component panel-name]]
    [visual-history/editor-history]]])
