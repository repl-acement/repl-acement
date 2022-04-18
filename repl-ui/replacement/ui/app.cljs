(ns replacement.ui.app
  (:require
    [portal.web :as p]
    [re-frame.core :as re-frame]
    [replacement.ui.events :as events]
    [replacement.ui.page-view :as main-view]
    [replacement.ui.subs]))

(def portal (p/open))

(defn dev-setup []
  (add-tap #'p/submit)
  (enable-console-print!))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (main-view/render))

(defn ^:dev/after-load init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
