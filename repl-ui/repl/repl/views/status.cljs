(ns repl.repl.views.status
  (:require
    [re-frame.core :as re-frame]
    [re-com.core :refer [h-box v-box box button gap line scroller border label input-text md-circle-icon-button
                         md-icon-button input-textarea modal-panel h-split v-split title flex-child-style
                         radio-button p]]
    [repl.repl.subs :as subs]
    [repl.repl.user :as user]
    [clojure.edn :as edn]))

(defonce default-style
         {:font-family "Menlo, Lucida Console, Monaco, monospace"
          :border      "1px solid lightgray"
          :padding     "5px 5px 5px 5px"})

(defonce status-style (merge (dissoc default-style :border)
                         {:font-size   "10px"
                          :font-weight "lighter"
                          :color       "lightgrey"}))

(defn status-bar
  [user]
  (let [network-status @(re-frame/subscribe [::subs/network-status])
        {:keys [major minor incremental qualifier]}  (edn/read-string @(re-frame/subscribe [::subs/clojure-version]))
        clojure-version (str major "." minor "." incremental
                             (and qualifier (str "-" qualifier)))
        network-style  {:color (if network-status
                                 "rgba(127, 191, 63, 0.32)"
                                 "red")}]
    (println :clojure-version clojure-version)
    [v-box :children
     [[line]
      [h-box :size "20px" :style status-style
       :justify :between :align :center
       :children
       [[label :label (str "Login: " (::user/name user))]
        [label :label (str "Clojure version: " clojure-version)]
        [h-box :gap "5px" :children
         [[label :style network-style :label "Connect Status:"]
          (let [icon-suffix (if network-status "-done" "-off")]
            [md-icon-button :md-icon-name (str "zmdi-cloud" icon-suffix)
             :size :smaller :style network-style])]]
        [label :style network-style :label "Font size: Large"]]]]]))
