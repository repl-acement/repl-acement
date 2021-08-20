(ns repl.repl.messages
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.test.alpha :as stest]
            [repl.repl.user :as user]
            [repl.repl.team :as team]
            [repl.repl.general :as general]))

;; TEAM BOOTSTRAP / LOGIN / LOGOUT

;; Small additional barrier to existing scripts / bots
(spec/def ::challenge string?)

(spec/def ::team-data
  (spec/keys :req [::team/name ::team/secret]))

(spec/def ::team-data-request
  (spec/keys :req [::challenge]))

(spec/def ::team-data-reply
  (spec/merge ::repl-eval-request ::team-data))

(spec/def ::login
  (spec/merge ::team-data
              (spec/keys :req [::user/name])))

;; Sent to all team members (incl the new one) on success
(spec/def ::login-event
  (spec/merge ::team-data
              (spec/keys :req [::team/members])))

(spec/def ::logout
  (spec/keys :req [::user/name ::team/name]))

;; Sent to all (remaining) team members
(spec/def ::logout-event ::login-event)

;; Note: The user being logged out should request new team data


;; KEYSTROKES

;; Communicated via a patch
(spec/def ::patch any?)

(spec/def ::keystrokes
  (spec/keys :req [::patch ::user/user]))

;; REPL EVALUATION

(spec/def ::source #{:user :system})
(spec/def ::forms (spec/coll-of ::form))

(spec/def ::remote-eval
  (spec/keys :req [::form ::source ::user/user ::forms]))

(def tags #{:ret :out :err :tap})
(spec/def ::tag (spec/with-gen keyword? #(spec/gen tags)))
(spec/def ::val ::general/string-data)
(spec/def ::ns ::general/string-data)
(spec/def ::ms int?)
(spec/def ::form ::general/string-data)
(def phase-indicators #{:read-source :macro-syntax-check :macroexpansion :compile-syntax-check
                        :compilation :execution :read-eval-result :print-eval-result})
(spec/def ::phase
  (spec/with-gen keyword? #(spec/gen phase-indicators)))

(spec/def ::eval-result
  (spec/keys :req-un [::tag ::val ::user]
             :opt-un [::ms ::ns ::form ::phase]))

;; Sent to all team members
(spec/def ::remote-eval-event
  (spec/merge ::remote-eval
              (spec/keys :req [::eval-result])))


;; ---------------------------------------------------------
;;
;; ---> Shared functions

(defn ->keystrokes
  "Create a map for sharing new keystrokes"
  [patch user]
  {::patch     patch
   ::user/user user})

(spec/fdef ->keystrokes
           :args (spec/cat :patch ::patch
                           :user ::user/user)
           :ret ::keystrokes)

;; Check all calls
(stest/instrument)
