(ns replacement.server.http
  (:require
    [clojure.core.async :as async :refer [chan]]
    [org.httpkit.server :as http-kit]
    [replacement.server.analysis :as analysis]
    [replacement.server.async-prepl :as socket-prepl]
    [replacement.specs.user :as user-specs]
    [replacement.server.web :as web]
    [taoensso.sente :as sente]
    [taoensso.timbre :refer [debugf infof]])
  (:import (clojure.lang DynamicClassLoader)
           (java.awt Desktop HeadlessException)
           (java.net URI))
  (:gen-class))

(set! *warn-on-reflection* true)

;;;; Sente event handlers
; Dispatch on event-id
(defmulti ^:private -event-msg-handler
          "Multimethod to handle Sente `event-msg`s"
          :id)

; Handle event-msgs on a single thread
(defn- event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [ev-msg]
  (-event-msg-handler ev-msg))

; Default/fallback case (no other matching handler)
(defmethod ^:private -event-msg-handler :default
  [{:keys [event ?reply-fn]}]
  (debugf "Unhandled event: %s" event)
  (when ?reply-fn
    (?reply-fn {:unmatched-event-as-echoed-from-from-server event})))

;;;; LOGIN

;; Sente uses Ring by default but we use WS to track users
(defonce ^:private connected-users (atom {}))

(defn >send
  "Send `msg` to each member"
  [msg]
  (let [uids (user-specs/get-uids @connected-users)]
    (doall (map #(web/chsk-send! % msg) uids))))

;; Debug
(add-watch connected-users :connected-users
           (fn [_ _ old new]
             (when (not= old new)
               (println :connected-users new))))

(defonce ^:private socket-connections (atom {}))

(defn forward-results
  [out-ch]
  (async/go-loop []
    (let [prepl-map (async/<! out-ch)]
      (>send [:replacement/eval prepl-map])
      (recur))))

(def prepl-chan (chan))
(def ^:private prepl-opts (atom (socket-prepl/init-prepl {:out-ch prepl-chan})))
(def _forwarding (forward-results prepl-chan))

;; REPL

;; Send keystrokes around the team
(defmethod ^:private -event-msg-handler :replacement/keystrokes
  [{:keys [?data]}]
  (>send [:replacement/keystrokes ?data])
  (>send [:replacement/analysis (analysis/clj-kondo ?data)]))

(defmethod ^:private -event-msg-handler :replacement/eval
  [{:keys [?data]}]
  (socket-prepl/shared-eval @prepl-opts ?data))

(defmethod ^:private -event-msg-handler :replacement/cancel
  [_]
  (socket-prepl/cancel @prepl-opts))

(defn- register-socket [state client-id]
  (assoc state (keyword client-id) {}))

;; TODO: Bring back the notion of a team to act as a barrier to entry

(defn- register-user [login-user]
  (swap! connected-users #(user-specs/+user % login-user))
  (>send [:replacement/users @connected-users]))

(defn- deregister-user [user]
  (swap! connected-users #(user-specs/<-user (::user-specs/name user) %))
  (>send [:replacement/users @connected-users]))

; the dropping thing needs to be re-thought, maybe via core.async timeouts
(defn- register-socket-ping [state client-id]
  (let [kw-client (keyword client-id)]
    (assoc-in state [kw-client :ping] (System/currentTimeMillis))))

(defmethod ^:private -event-msg-handler :chsk/uidport-open
  [{:keys [client-id]}]
  (swap! socket-connections register-socket client-id))

(defmethod ^:private -event-msg-handler :chsk/uidport-close
  [_])

(defmethod ^:private -event-msg-handler :chsk/ws-ping
  [{:keys [client-id]}]
  ; maybe we kill if no ping too
  (swap! socket-connections register-socket-ping client-id))

(defonce ^:private shared-secret (atom nil))

(defn- logout [{:keys [?data]}]
  (deregister-user ?data))

;; TODO what are the barriers?
(defn- login [{:keys [?data ?reply-fn]}]
  ;; check team secret here
  (if (register-user ?data)
    (?reply-fn :login-ok)
    (?reply-fn :login-failed)))

(defmethod ^:private -event-msg-handler :replacement/login
  [ev-msg]
  (login ev-msg))

(defmethod ^:private -event-msg-handler :replacement/logout
  [ev-msg]
  (logout ev-msg))

;; TODO drop the team name messages (that's for the team node)
(defmethod ^:private -event-msg-handler :replacement/team-random-data
  [{:keys [?reply-fn]}]
  (println :team-random-data)
  (?reply-fn {:team-name "apropos" :team-secret @shared-secret}))

;;;; Sente event router (our `event-msg-handler` loop)

(defonce router_ (atom nil))

(defn- stop-router! []
  (when-let [stop-fn @router_] (stop-fn)))

(defn- start-router! []
  (stop-router!)
  (reset! router_ (sente/start-server-chsk-router!
                    web/ch-chsk event-msg-handler)))

;;;; Init stuff

(defonce ^:private web-server_ (atom nil))
(defn- stop-web-server! [] (when-let [stop-fn @web-server_] (stop-fn)))
(defn start-web-server! [& [port]]
  (stop-web-server!)
  (let [port         (or port 0)                            ; 0 => Choose any available port
        ring-handler (var web/main-ring-handler)
        [port stop-fn] (let [stop-fn (http-kit/run-server ring-handler {:port port})]
                         [(:local-port (meta stop-fn)) (fn [] (stop-fn :timeout 100))])
        uri          (format "http://localhost:%s/" port)]

    (infof "Web server is running at `%s`" uri)
    (try
      (.browse (Desktop/getDesktop) (URI. uri))
      (catch HeadlessException _))

    (reset! web-server_ stop-fn)))

(defn stop! [] (stop-router!) (stop-web-server!))

(defn- start!
  ([]
   (start! 56665))
  ([port]
   (start-router!)
   (start-web-server! port)))

(defn restart! []
  (stop!) (start!))

(defn start-repl-server
  ([secret]
   (start-repl-server secret "56665"))
  ([secret port]
   (reset! shared-secret secret)
   (let [port           (Integer/parseInt port)
         current-thread (Thread/currentThread)
         classloader    (.getContextClassLoader current-thread)]
     ; Need DynamicClassLoader to support add-lib
     (.setContextClassLoader current-thread (DynamicClassLoader. classloader))
     (start! port))))

(defn -main [& _args]
  (let [port   (or (System/getenv "PORT") "56665")
        secret (or (System/getenv "TEAM_SECRET") "weak-security")]
    (start-repl-server secret port)))