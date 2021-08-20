(ns repl.repl.helpers
  (:require
    [re-com.core :refer [md-icon-button]]))

(defn js->cljs
  [js-obj]
  (js->clj js-obj :keywordize-keys true))

(defn icon-button
  ([icon-name]
   (icon-button icon-name :smaller))
  ([icon-name icon-size]
   [md-icon-button
    :md-icon-name (str "zmdi-" icon-name)
    :size icon-size]))