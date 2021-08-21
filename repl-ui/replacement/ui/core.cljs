(ns replacement.ui.core
  (:require
    [re-frame.core :as re-frame]
    [replacement.ui.events :as events]
    [replacement.ui.main-view :as main-view]
    [replacement.ui.subs]
    [reagent.dom :as rdom]))

(defn dev-setup []
  (enable-console-print!))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (let [el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node el)
    (rdom/render [main-view/main-panel] el)))

(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
