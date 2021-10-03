(ns replacement.ui.app
  (:require
    [re-frame.core :as re-frame]
    [replacement.ui.events :as events]
    [replacement.ui.main-view :as main-view]
    [replacement.ui.subs]))

(defn dev-setup []
  (enable-console-print!))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (main-view/render))

(defn ^:dev/after-load init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
