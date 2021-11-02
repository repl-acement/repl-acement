(ns replacement.ui.subs
  (:require
    [re-frame.core :refer [reg-sub]]
    [replacement.specs.user :as user]))

(reg-sub
  ::clojure-version
  (fn [db]
    (:clojure-version db)))

(reg-sub
  ::user-keystrokes
  (fn [db [_ user]]
    (get-in db [:current-forms user])))

(reg-sub
  ::eval-results
  (fn [db]
    (:eval-results db)))

(reg-sub
  ::latest-result
  (fn [db]
    (:latest-result db)))

(reg-sub
  ::show-times
  (fn [db]
    (:show-times db)))

(reg-sub
  ::network-status
  (fn [db]
    (:network-status db)))

(reg-sub
  ::name
  (fn [db]
    (:name db)))

(reg-sub
  ::current-form
  (fn [db]
    (:current-form db)))

(reg-sub
  ::completions
  (fn [db]
    (get-in db [:local-repl-editor :completions])))

(reg-sub
  ::user
  (fn [db]
    (::user/user db)))

(reg-sub
  ::logged-in
  (fn [db]
    (:logged-in db)))

(reg-sub
  ::logged-in-user
  (fn [db]
    (::user/user db)))

(reg-sub
  ::user-code-mirror
  (fn [db]
    (get-in db [:local-repl-editor :code-mirror])))

(reg-sub
  ::network-repl-editor
  (fn [db [_ editor-key]]
    (get-in db [:network-repl-editors editor-key])))

(reg-sub
  ::other-users
  (fn [db]
    (when (::user/users db)
      (user/other-users (::user/name (::user/user db))
                        (::user/users db)))))

(reg-sub
  ::other-users-count
  (fn [db]
    (count (user/other-users (::user/name (::user/user db))
                             (::user/users db)))))

(reg-sub
  ::network-repl-editor-keys
  (fn [db]
    (keys (:network-repl-editors db))))

(reg-sub
  ::show-add-lib-panel
  (fn [db]
    (:show-add-lib-panel db)))

(reg-sub
  ::doc-show?
  (fn [db]
    (:doc-show? db)))

(reg-sub
  ::doc-text
  (fn [db]
    (:doc-text db)))

(reg-sub
  ::team-data
  (fn [db]
    (:team-data db)))

(reg-sub
  ::show-team-data
  (fn [db]
    (:show-team-data db)))

(reg-sub
  ::input-history
  (fn [db]
    (:input-history db)))

(reg-sub
  ::os
  (fn [db]
    (:os db)))

(reg-sub
  ::key-bindings
  (fn [db]
    (:key-bindings db)))

(reg-sub
  ::fn-arity-data
  (fn [db]
    (:arity-data db)))