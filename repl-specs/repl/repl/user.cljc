(ns repl.repl.user
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.spec.test.alpha :as stest]
    [repl.repl.general :as general]))

;; Server defined user ID per WS connection, used for WS server push
(spec/def ::uid ::general/string-data)

;; Name entered by the user, will be unique per team
(spec/def ::name ::general/string-data)

;; User details
(spec/def ::user
  (spec/keys :req [::name ::uid]))

;; User collection - maps ::id = (keyword ::name)
(spec/def ::id keyword?)
(spec/def ::users
  (spec/and (spec/map-of ::id ::user)
            (spec/every (fn [[k v]]
                          (= (keyword (::name v)) k)))))

;; ---------------------------------------------------------
;;
;; ---> Shared functions

(defn ->user
  "Create a user map"
  [name uid]
  {::name name
   ::uid  uid})

(spec/fdef ->user
           :args (spec/cat :name ::name
                           :uid ::uid)
           :ret ::user)

(defn user->users
  "Create a ::users map from `user`"
  [user]
  {(keyword (::name user)) user})

(spec/fdef user->users
           :args (spec/cat :user ::user)
           :ret ::users)

(defn +user
  "Add the `user` map to `users`"
  [users user]
  (merge users (user->users user)))

(spec/fdef +user
           :args (spec/cat :users ::users
                           :user ::user)
           :ret ::users)

(defn <-user
  "Remove user `name` from `users`"
  [name users]
  (dissoc users (keyword name)))

(spec/fdef <-user
           :args (spec/cat :name ::name
                           :users ::users)
           :ret ::users)

(defn find-user
  "Find user `name` in `users`"
  [name users]
  (get users (keyword name)))

(spec/fdef find-user
           :args (spec/cat :name ::name
                           :users ::users)
           :ret (spec/or :user ::user
                         :nil nil?))

(defn other-users
  "All users except `name` in `users`"
  [name users]
  (dissoc users (keyword name)))

(spec/fdef other-users
           :args (spec/cat :name ::name
                           :users ::users)
           :ret ::users)

(defn get-user-by-uid
  "Get the ::user map matching `user-uid` in `users`"
  [user-uid users]
  (first (map (fn [[_ {::keys [uid] :as user}]]
                (when (= user-uid uid) user))
              users)))

(spec/fdef get-user-by-uid
           :args (spec/cat :user-uid ::name
                           :users ::users)
           :ret (spec/or :user ::user
                         :nil nil?))

(defn get-uids
  "Get the uid list from `users`"
  [users]
  (map (fn [[_ {::keys [uid]}]] uid) users))

(spec/fdef get-uids
           :args (spec/cat :users ::users)
           :ret (spec/coll-of ::uid))

;; Check all calls
(stest/instrument)

