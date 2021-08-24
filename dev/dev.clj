(ns dev
  (:require
    [clojure.tools.namespace.repl :as repl]
    [replacement.server.http :as server])
  (:import (org.eclipse.jetty.server Server)))

(repl/set-refresh-dirs "dev" "repl-server")

(defonce server (atom nil))

(defn go []
  (reset! server (server/start-repl-server "weak-security")))

(defn reset []
  (server/restart!)
  (repl/refresh :after 'dev/go))

