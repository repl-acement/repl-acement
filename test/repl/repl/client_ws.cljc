(ns repl.repl.client-ws
  (:require
    [taoensso.encore :refer [have have?]]
    [taoensso.timbre :refer [tracef debugf infof warnf errorf]]
    [taoensso.sente :as sente                               ; :refer [cb-success?]
     ]
    [taoensso.sente.packers.transit :as sente-transit]
    [clojure.core.async]))

; --- WS client ---
(declare chsk ch-chsk chsk-send! chsk-state)

(defmulti -event-msg-handler "Multimethod to handle Sente `event-msg`s"
          ; Dispatch on event-id
          :id)

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler
  :default                                                  ; Default/fallback case (no other matching handler)
  [{:keys [event]}]
  (println "Unhandled event: %s" event))

(defmethod -event-msg-handler :chsk/state
  [{:keys [?data]}]
  (let [[_ new-state-map] (have vector? ?data)]
    (if (:first-open? new-state-map)
      (println "Channel socket successfully established!: %s" new-state-map)
      (println "Channel socket state change: %s" new-state-map))))

(defmethod -event-msg-handler :chsk/recv
  [{:keys [?data]}]
  (let [push-event (first ?data)
        push-data  (first (rest ?data))]
    (cond
      ; TODO Fix up
      ;(= push-event :fast-push/keystrokes)
      ;(re-frame/dispatch [:repl.tongue.events/network-repl-editor-form-update push-data])

      ; TODO Fix up
      ;(= push-event :fast-push/editors)
      ;(re-frame/dispatch [:repl.tongue.events/repl-editors push-data])

      ; TODO Fix up
      ;(= push-event :fast-push/eval)
      ;(re-frame/dispatch [:repl.tongue.events/eval-result push-data])

      (= push-event :chsk/ws-ping)
      :noop                                                 ; do reply

      :else (println "Unhandled data push: %s" push-event))))

(defmethod -event-msg-handler :chsk/handshake
  [{:keys [?data]}]
  (println "Handshake: %s" ?data))

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (reset! router_
          (sente/start-client-chsk-router!
            ch-chsk event-msg-handler)))

;; TODO Fix up
(def host "SOMETHING")

(defn x
  []
  (let [;; Serialization format, must use same val for client + band:
        packer (sente-transit/get-transit-packer)           ; Needs Transit dep

        ;{:keys [chsk ch-recv send-fn state]}
        ;(sente/make-channel-socket-client!
        ;  "/chsk"                                           ; Must match band Ring routing URL
        ;  {:type   :auto
        ;   :host   host
        ;   :packer packer})
        ]

    (def chsk chsk)

    ;; ChannelSocket's receive channel
    ;(def ch-chsk ch-recv)
    ;
    ;; ChannelSocket's send API fn
    ;(def chsk-send! send-fn)
    ;
    ;; Watchable, read-only atom
    ;(def chsk-state state)

    ;; Now we have all of this set up we can start the router
    (start-router!)))
