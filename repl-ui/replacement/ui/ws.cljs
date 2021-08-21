(ns replacement.ui.ws
  (:require
    goog.date.Date
    [re-frame.core :as re-frame]
    [taoensso.encore :as encore :refer [have have?]]
    [taoensso.timbre :as timbre :refer [tracef debugf infof warnf errorf]]
    [taoensso.sente :as sente :refer [cb-success?]]
    [taoensso.sente.packers.transit :as sente-transit]
    [taoensso.timbre :as timbre]))

; --- WS client ---
(declare chsk ch-chsk chsk-send! chsk-state)

(defmulti -event-msg-handler
          "Dispatch on :id from Sente `event-msg`s"
          :id)

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [ev-msg]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler :default
  [{:keys [event]}]
  (println "Unhandled event: %s" event))

(defonce client-uid (atom nil))

(defmethod -event-msg-handler :chsk/state
  [{:keys [?data]}]
  (let [[_ new-state-map] (have vector? ?data)]
    (reset! client-uid (:uid new-state-map))
    (re-frame/dispatch [:replacement.ui.events/client-uid (:uid new-state-map)])
    (re-frame/dispatch [:replacement.ui.events/network-status (:open? new-state-map)])))

(defmethod -event-msg-handler :chsk/recv
  [{:keys [?data]}]
  (let [push-event (first ?data)
        push-data  (first (rest ?data))]
    (cond
      (= push-event :replacement/keystrokes)
      (re-frame/dispatch [:replacement.ui.events/other-user-keystrokes push-data])

      (= push-event :replacement/users)
      (re-frame/dispatch [:replacement.ui.events/users push-data])

      (= push-event :replacement/eval)
      (re-frame/dispatch [:replacement.ui.events/eval-result push-data])

      (= push-event :chsk/ws-ping)
      :noop                                                 ; do reply

      :else
      (println "Unhandled data push: %s" push-event))))

;; The WS connection is established ... get the team name and secret
(defmethod -event-msg-handler :chsk/handshake
  []
  ;; TODO add the user in here if we are logged in
  (re-frame/dispatch [:replacement.ui.events/team-bootstrap]))

(defonce router_ (atom nil))

(defn stop-router! []
  (when-let [stop-f @router_]
    (stop-f)))

(defn start-router! []
  (stop-router!)
  (reset! router_
          (sente/start-client-chsk-router!
            ch-chsk event-msg-handler)))

(defn ->output! [fmt & args]
  (let [msg (apply encore/format fmt args)]
    (timbre/debug msg)))

(->output! "ClojureScript appears to have loaded correctly.")

(def ?csrf-token
  (when-let [el (.getElementById js/document "sente-csrf-token")]
    (.getAttribute el "data-csrf-token")))

(if ?csrf-token
  (->output! "CSRF token detected in HTML, great!")
  (->output! "CSRF token NOT detected in HTML, default Sente config will reject requests"))

(let [packer (sente-transit/get-transit-packer)

      {:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client!
                                             "/chsk"
                                             ?csrf-token
                                             {:type   :auto
                                              :packer packer})]

  (def chsk chsk)

  ; ChannelSocket's receive channel
  (def ch-chsk ch-recv)

  ; ChannelSocket's send API fn
  (def chsk-send! send-fn)

  ; Watchable, read-only atom
  (def chsk-state state))

(defn start! [] (start-router!))

(defonce _start-once (start!))