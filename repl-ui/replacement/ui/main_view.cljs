(ns replacement.ui.main-view
  (:require
    [re-frame.core :refer [subscribe]]
    [replacement.ui.subs :as subs]
    [replacement.ui.views.login :as login]
    [replacement.ui.views.editor :as editor]))

(defn main-panel
  []
  (let [logged-in-user @(subscribe [::subs/logged-in-user])]
    (if-not logged-in-user
      [login/authenticate]
      [editor/main-panels logged-in-user])))
