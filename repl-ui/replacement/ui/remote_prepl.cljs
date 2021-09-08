(ns replacement.ui.remote-prepl
  (:require ["@codemirror/view" :as view]
            [applied-science.js-interop :as j]
            [nextjournal.clojure-mode.node :as n]
            [nextjournal.clojure-mode.extensions.eval-region :as eval-region]
            [nextjournal.clojure-mode.util :as u]
            [re-frame.core :refer [dispatch-sync]]
            [replacement.ui.events :as events]))

(defonce res-fn (atom nil))

;; work out how to catch keystrokes
(defn eval-string
  [source]
  (try
    (dispatch-sync [::events/current-form source])
    (dispatch-sync [::events/eval])
    (catch js/Error e
      (prn :ex e)
      (str e))))

(j/defn eval-at-cursor [^:js {:keys [state]}]
        (some->> (eval-region/cursor-node-string state)
                 (eval-string))
        true)

(j/defn eval-top-level [^:js {:keys [state]}]
        (some->> (eval-region/top-level-string state)
                 (eval-string))
        true)

(j/defn eval-cell [^:js {:keys [state]}]
        (-> (str (.-doc state))
            (eval-string))
        true)

(defn keymap* [modifier]
  {:eval-cell
   [{:key "Mod-Enter"
     :doc "Evaluate cell"}]
   :eval-at-cursor
   [{:key (str modifier "-Enter")
     :doc "Evaluates form at cursor"}]
   :eval-top-level
   [{:key (str modifier "-Shift-Enter")
     :doc "Evaluates top-level form at cursor"}]})

(defn extension [{:keys [modifier]}]
  (.of view/keymap
       (j/lit
         [{:key "Mod-Enter"
           :run eval-cell}
          {:key   (str modifier "-Enter")
           :shift eval-top-level
           :run   eval-at-cursor}])))
