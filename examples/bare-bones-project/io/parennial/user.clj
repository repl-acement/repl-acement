(ns io.parennial.user
  (:import (java.security MessageDigest)
           (org.apache.commons.codec.binary Hex)))

(defn bytes->hex
  [array-buffer]
  (Hex/encodeHexString ^bytes array-buffer))

(defn user-id
  [user-name]
  (let [sha (MessageDigest/getInstance "SHA-256")]
    (->> (.getBytes user-name)
         (.digest sha)
         bytes->hex)))
