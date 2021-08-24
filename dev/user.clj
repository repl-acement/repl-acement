(ns user)

(println "Welcome to REPL-acement development. Use (dev) for getting started.")

(defn dev
  []
  (require 'dev)
  (in-ns 'dev)
  (println "Use (go) to start and (reset) to reload the code and restart the system."))


(defn go
  []
  (println "Don't you mean (dev) ?"))
