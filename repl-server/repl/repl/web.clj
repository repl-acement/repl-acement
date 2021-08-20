(ns repl.repl.web
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [hiccup.page :as page]
            [ring.middleware.anti-forgery :as anti-forgery]
            [ring.middleware.defaults]
            [taoensso.sente :as sente]
            [taoensso.sente.packers.transit :as sente-transit]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- ws-uid-fn
  "Create a UUID per connection that can be used as a key for pushing data"
  ([]
   (str (UUID/randomUUID)))
  ([_]
   (str (UUID/randomUUID))))

(let [packer      (sente-transit/get-transit-packer)
      chsk-server (sente/make-channel-socket-server! (get-sch-adapter) {:packer     packer
                                                                        :user-id-fn ws-uid-fn})
      {:keys [ch-recv send-fn connected-uids ajax-post-fn ajax-get-or-ws-handshake-fn]} chsk-server]

  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def connected-uids connected-uids))                      ; Watchable, read-only atom

;; Debug
(add-watch connected-uids :connected-uids
           (fn [_ _ old new]
             (when (not= old new)
               (println :connected-uids new))))

(defn landing-pg-handler [_req]
  (page/html5
    {:lang "en"}
    ;; This must be at the top of the HTML to be parsed properly by the client
    [:div#sente-csrf-token {:data-csrf-token (force anti-forgery/*anti-forgery-token*)}]
    [:head
     [:title "REPL REPL - Collaborative Clojure REPL"]
     [:meta {:charset "utf-8"}]
     [:link {:rel "shortcut icon" :href "images/favicon.png" :type "image/png"}]
     (page/include-css "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.5/css/bootstrap.min.css")
     (page/include-css "vendor/css/codemirror.css")
     (page/include-css "vendor/css/show-hint.css")
     (page/include-css "vendor/css/material-design-iconic-font.min.css")
     (page/include-css "vendor/css/re-com.css")
     (page/include-css "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700,400italic")
     (page/include-css "https://fonts.googleapis.com/css?family=Roboto+Condensed:400,300")
     (page/include-js "/js/compiled/app.js")]
    [:body
     [:div#app]
     [:script "repl.repl.core.init();"]]))

(defroutes ring-routes
           (GET "/" ring-req (landing-pg-handler ring-req))
           (GET "/chsk" ring-req (ring-ajax-get-or-ws-handshake ring-req))
           (POST "/chsk" ring-req (ring-ajax-post ring-req))
           (route/resources "/")                            ; Static files, notably public/main.js (our cljs target)
           (route/not-found "<h1>Page not found</h1>"))

(def main-ring-handler
  (ring.middleware.defaults/wrap-defaults
    ring-routes ring.middleware.defaults/site-defaults))
