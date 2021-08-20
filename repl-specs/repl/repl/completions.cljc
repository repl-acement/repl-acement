(ns repl.repl.completions
  (:require [clojure.spec.alpha :as spec]))

;; Completions

(spec/def ::candidate string?)
(spec/def ::type keyword?)
(spec/def ::ns string?)

;; TODO Flesh out the Docs / fns
(spec/def ::docs any?)
(spec/def ::args any?)

(spec/def ::completions
  (spec/keys :req [::candidate ::type ::ns]
             :opt [::docs ::args]))

