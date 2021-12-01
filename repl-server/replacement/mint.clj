(ns replacement.mint
  "Another example ns showing some forms"
  (:require [clojure.core.specs.alpha :as s]))

(def seigniorage "Profits from coin production" 0.50)

(defn strike
  "Make a new coin and put it into circulation"
  {:added "0.1"}
  ([money-supply denomination]
   {:pre [(keyword? denomination)]}
   (conj money-supply denomination)))
