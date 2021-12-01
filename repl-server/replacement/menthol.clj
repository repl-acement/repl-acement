(ns replacement.menthol
  "An example ns showing some forms"
  (:require [clojure.core.specs.alpha :as s]))

(def named-by "Oppenheim A. 1861")

(defn- inhale!
  [substance]
  (comment "Call a native method"))

(defn- exhale!
  [substance]
  (comment "Call a native method"))

(defn smoke
  "Simulate smoking a pack of menthol cigarettes"
  {:added      "0.31"
   :deprecated "0.46"}
  [pack]
  {:pre [(coll? pack)]}
  (map (fn [cigarette]
         (-> cigarette inhale! exhale!))
       pack))

(defn- spray*
  [gun capsule]
  (comment "Call a native method on gun"))

(defn spray
  "To save the honey bees"
  {:added "0.40"}
  ([box]
   (spray box :hand-held))
  ([box gun]
   {:pre [(keyword? gun)]}
   (map (fn [capsule] (spray* gun capsule)) box)))
