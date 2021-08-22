(ns replacement.server.analysis
  (:require
    [replacement.specs.messages :as messages]
    [clj-kondo.core :as clj-kondo]))

(defn- clj-kondo* [form]
  (-> form
      (with-in-str (clj-kondo/run! {:lint ["-"] :config {:output {:analysis true}}}))
      :analysis))

(defn clj-kondo [{::messages/keys [form]}]
  {:analysis/form form
   :analysis/clj-kondo (clj-kondo* form)})
