(ns repl.repl.general
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as string]))

(spec/def ::string-data
  (spec/and string?
            #(not (string/blank? %))))

;; Check all calls
(stest/instrument)

