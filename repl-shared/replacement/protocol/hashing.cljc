(ns replacement.protocol.hashing
  #?(:cljs (:require [promesa.core :as p])
     :clj  (:import (java.security MessageDigest)
                    (org.apache.commons.codec.binary Hex))))


(defn string->encoded-data
  [s]
  #?(:clj  s
     :cljs (let [encoder (js/TextEncoder.)]                 ;; Always UTF-8
             (.encode encoder s))))

(defn bytes->hex
  [array-buffer]
  #?(:clj  (Hex/encodeHexString ^bytes array-buffer)
     :cljs (->> (js/Array.from (js/Uint8Array. array-buffer))
                (map #(.padStart (.toString % 16) 2 "0"))
                (apply str))))

(defn digest
  [data]
  (let [s (pr-str data)]
    #?(:clj  (let [md5 (MessageDigest/getInstance "SHA-256")]
               (->> (.getBytes s)
                    (.digest md5)
                    bytes->hex))
       :cljs (p/let [web-crypto (.-crypto.subtle js/window)]
               (p/->> (string->encoded-data s)
                      (.digest web-crypto "SHA-256")
                      (bytes->hex))))))

