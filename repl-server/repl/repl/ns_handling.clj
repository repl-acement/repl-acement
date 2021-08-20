(ns repl.repl.ns-handling
  (:require [clojure.tools.namespace.parse :as parse]))

(defn get-deps [form]
  (when (parse/ns-decl? form)
    (parse/deps-from-ns-decl form)))

(defn load-ns [nses name]
  (get nses (keyword name)))


(comment

  ;; Store this in a DB
  (def nses {:funky  "(ns funky)\n(defn x [a] (* a a))\n"
             :funky2 "(ns funky2\n  (:require [funky :refer [x]]))\n(defn y [z] (x z))"})

  (load-ns nses "funky")

  (-> (load-ns nses "funky2") read-string get-deps)

  )