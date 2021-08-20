(ns repl.repl.main-view
  (:require
    [re-frame.core :refer [subscribe]]
    [repl.repl.subs :as subs]
    [repl.repl.views.login :as login]
    [repl.repl.views.editor :as editor]))

(defn main-panel
  []
  (let [logged-in-user @(subscribe [::subs/logged-in-user])]
    (if-not logged-in-user
      [login/authenticate]
      [editor/main-panels logged-in-user])))
