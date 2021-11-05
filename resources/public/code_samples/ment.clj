(ns repl.ace.ment)

(def themes {:day   :light
             :night :dark})

(def theme (atom :day))

(defn set-theme
  [new-theme]
  (swap! theme new-theme))

(defn apply-theme
  ([output]
   (apply-theme output :terminal))
  ([output device]
   (str "To be implemented " output " for " device)))