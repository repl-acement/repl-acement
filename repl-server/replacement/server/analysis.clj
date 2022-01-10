(ns replacement.server.analysis
  (:require
    [replacement.specs.messages :as messages]
    [clj-kondo.core :as kondo]))

(set! *warn-on-reflection* true)

(defn- clj-kondo* [form]
  (-> form
      (with-in-str (kondo/run! {:lint ["-"] :config {:output {:analysis true}}}))
      :analysis))

(defn clj-kondo [{::messages/keys [form]}]
  {:analysis/form form
   :analysis/clj-kondo (clj-kondo* form)})


(comment

  (def nses {:funky  (clj-kondo* "(ns funky)\n(defn x [a] (* a a))\n")
             :funky2 (clj-kondo* "(ns funky2\n  (:require [funky :refer [x]]))\n(defn y [z] (x z))")})

  )