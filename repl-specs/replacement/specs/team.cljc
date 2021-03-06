(ns replacement.specs.team
  (:require
    [clojure.spec.alpha :as spec]
    [replacement.specs.general :as general]
    [replacement.specs.team-names :as team-names]
    [replacement.specs.user :as user]))

;; Constrained list (i.e. a set) of users per team
(spec/def ::members
  (spec/coll-of ::user/user :distinct true))

;; Hopefully relatable name - see repl.repl.factory
(spec/def ::name ::general/string-data)

;; UUID, randomly generated by the server
(spec/def ::secret uuid?)

;; Evaluation environment ... only relevant on the server
(spec/def ::prepl any?)

;; Appropriate data set per team
(spec/def ::team
  (spec/keys :req [::name
                   ::members
                   ::prepl
                   ::secret]))

;; Global set of teams
(spec/def ::teams
  (spec/coll-of ::team :distinct true))


(defn ->team-name
  []
  (team-names/gen-name))


