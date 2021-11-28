(ns replacement.forms.parser.parse
  (:require [cljs.tools.reader.reader-types :as readers]
            [cljs.tools.reader :as reader]
            [cljs.spec.alpha :as s]
            [replacement.structure.form-specs :as form-specs]))

(defn read-whole-ns
  [ns-text]
  (let [EOF    :EOF
        reader (readers/source-logging-push-back-reader ns-text)]
    (reduce (fn [forms [form _]]
              (if (= form EOF)
                (reduced forms)
                (conj forms form)))
            [] (repeatedly #(reader/read+string {:eof EOF :read-cond :allow} reader)))))

(defn whole-ns->forms
  [forms]
  (map (fn [form]
         (let [pre-conformed (s/conform ::form-specs/form form)
               conformed     (if (s/invalid? pre-conformed) ::invalid pre-conformed)
               explain       (if (s/invalid? pre-conformed) (s/explain-data ::form-specs/form form))
               unformed      (when-not (= ::invalid conformed) (s/unform ::form-specs/form conformed))]
           {:conformed conformed
            :explain   explain
            :unformed  unformed}))
       forms))

(defn ns-reference-data
  [ns-name [type {:keys [ns-args] :as data}]]
  (let [var-name (:ns-name ns-args)]
    [ns-name var-name type data]))

(defn def-reference-data
  [ns-name [type {:keys [var-name] :as data}]]
  [ns-name var-name type data])

(defn defn-reference-data
  [ns-name [type {:keys [defn-args] :as data}]]
  (let [var-name (:fn-name defn-args)]
    [ns-name var-name type data]))

(defn- add-reference-data*
  [conformed-forms]
  (let [ns-name (-> conformed-forms first second (get-in [:ns-args :ns-name]))]
    [ns-name (mapv
               (fn [form]
                 (cond
                   (= :ns (first form)) (ns-reference-data ns-name form)
                   (= :def (first form)) (def-reference-data ns-name form)
                   (= :defn (first form)) (defn-reference-data ns-name form)
                   :else form))
               conformed-forms)]))

(defn add-reference-data
  [conformed-list]
  (let [ref-data (->> conformed-list
                      (map #(:conformed %))
                      (add-reference-data*))]
    ;(cljs.pprint/pprint [:add-reference-data ref-data])
    ref-data))

(def sample "(ns repl.ace.menthol
\"Setting themes for the UI.\"
  (:require [cljs.spec.alpha :as s]
            [cljs.tools.reader :as reader]))

(def themes {:day   :light
             :night :dark})

(def theme (atom :day))

(defn set-theme
  \"Set the default theme\"
  [new-theme]
  {:pre [(themes new-theme)]}
  (swap! theme new-theme))

(defn apply-theme
  {:api-version \"0.1.0\"
   :stub true}
  ([output]
   (apply-theme output :terminal))
  ([output device]
   (str \" To be implemented \" output \" for \" device)))")

(def menthol
  "(ns replace.menthol\n  \"An example ns showing some forms\"\n  (:require [clojure.core.specs.alpha :as s]))\n\n(def named-by \"Oppenheim A. 1861\")\n\n(defn- inhale!\n  [substance]\n  (comment \"Call a native method\"))\n\n(defn- exhale!\n  [substance]\n  (comment \"Call a native method\"))\n\n(defn smoke\n  \"Simulate smoking a pack of menthol cigarettes\"\n  {:added      \"0.31\"\n   :deprecated \"0.46\"}\n  [pack]\n  {:pre [(coll? pack)]}\n  (map (fn [cigarette]\n         (-> cigarette inhale! exhale!))\n       pack))\n\n(defn- spray*\n  [gun capsule]\n  (comment \"Call a native method on gun\"))\n\n(defn spray\n  \"To save the honey bees\"\n  {:added \"0.40\"}\n  ([box]\n   (spray box :hand-held))\n  ([box gun]\n   {:pre [(keyword? gun)]}\n   (map (fn [capsule] (spray* gun capsule)) box)))\n")

(def mint
  "(ns replace.mint\n  \"Another example ns showing some forms\"\n  (:require [clojure.core.specs.alpha :as s]))\n\n(def seigniorage \"Profits from coin production\" 0.50)\n\n(defn strike\n  \"Make a new coin and put it into circulation\"\n  {:added \"0.1\"}\n  ([money-supply denomination]\n   {:pre [(keyword? denomination)]}\n   (conj money-supply denomination)))\n")

(def ring-sample-more-forms!!
  "(ns ring.middleware.session.cookie
  \"A session storage engine that stores session data in encrypted cookies.\"
    (:require [ring.middleware.session.store :refer [SessionStore]]
              [ring.util.codec :as codec]
              [clojure.edn :as edn]
              [crypto.random :as random]
              [crypto.equality :as crypto])
    (:import [java.security SecureRandom]
             [javax.crypto Cipher Mac]
             [javax.crypto.spec SecretKeySpec IvParameterSpec]))

  (def ^{:private true
         :doc \"Algorithm to generate a HMAC.\"}
       hmac-algorithm
       \"HmacSHA256\")

  (def ^{:private true
         :doc \"Type of encryption to use.\"}
       crypt-type
       \"AES\")

  (def ^{:private true
         :doc \"Full algorithm to encrypt data with.\"}
       crypt-algorithm
       \"AES/CBC/PKCS5Padding\")

  ;; Ensure cipher-algorithm classes are preloaded

  (Cipher/getInstance crypt-algorithm)

  (defn- hmac
    \"Generates a Base64 HMAC with the supplied key on a string of data.\"
    [key data]
    (let [mac (Mac/getInstance hmac-algorithm)]
      (.init mac (SecretKeySpec. key hmac-algorithm))
      (codec/base64-encode (.doFinal mac data))))

  (defn- encrypt
    \"Encrypt a string with a key.\"
    [key data]
    (let [cipher     (Cipher/getInstance crypt-algorithm)
          secret-key (SecretKeySpec. key crypt-type)
          iv         (random/bytes (.getBlockSize cipher))]
      (.init cipher Cipher/ENCRYPT_MODE secret-key (IvParameterSpec. iv))
      (->> (.doFinal cipher data)
      (concat iv)
      (byte-array))))

  (defn- decrypt
    \"Decrypt an array of bytes with a key.\"
    [key data]
    (let [cipher     (Cipher/getInstance crypt-algorithm)
          secret-key (SecretKeySpec. key crypt-type)
          [iv data]  (split-at (.getBlockSize cipher) data)
          iv-spec    (IvParameterSpec. (byte-array iv))]
      (.init cipher Cipher/DECRYPT_MODE secret-key iv-spec)
      (String. (.doFinal cipher (byte-array data)))))

  (defn- print-string-secret-key-deprecation
    []
    (binding [*out* *err*]
      (println \"WARNING: The secret key for the session cookie store should be a\"
               \"byte array.\\nString secret keys have been deprecated.\")))

  (defn- get-secret-key
    \"Get a valid secret key from a map of options, or create a random one from
    scratch.\"
    [options]
    (if-let [secret-key (:key options)]
      (if (string? secret-key)
        (do (print-string-secret-key-deprecation)
            (.getBytes ^String secret-key))
            secret-key)
        (random/bytes 16)))

  (defn- deserialize [x options]
    (edn/read-string (select-keys options [:readers]) x))

  defn- ^String serialize [x options]
    {:post [(= x (deserialize % options))]}
    (pr-str x))

  (defn- seal
    \"Seal a Clojure data structure into an encrypted and HMACed string.\"
    [key data options]
    (let [data (encrypt key (.getBytes (serialize data options)))]
      (str (codec/base64-encode data) \"--\" (hmac key data))))

  (defn- unseal
    \"Retrieve a sealed Clojure data structure from a string\"
    [key ^String string options]
    (let [[data mac] (.split string \"--\")
           data (codec/base64-decode data)]
      (if (crypto/eq? mac (hmac key data))
        (deserialize (decrypt key data) options))))

  (deftype CookieStore [secret-key options]
    SessionStore
    (read-session [_ data]
      (if data (unseal secret-key data options)))
        (write-session [_ _ data]
          (seal secret-key data options))
        (delete-session [_ _]
          (seal secret-key {} options)))

  (ns-unmap *ns* '->CookieStore)

  (defn- valid-secret-key? [key]
    (and (= (type (byte-array 0)) (type key))
         (= (count key) 16)))

  (defn cookie-store
    \"Creates an encrypted cookie storage engine. Accepts the following options:
      :key - The secret key to encrypt the session cookie. Must be a byte array of
             exactly 16 bytes. If no key is provided then a random key will be
             generated. Note that in that case a server restart will invalidate all
             existing session cookies.
      :readers - A map of data readers used to read the serialized edn from the
                 cookie. For writing, ensure that each data type has a key in the
                 clojure.core/print-method or clojure.core/print-dup multimethods.\"
    ([] (cookie-store {}))
    ([options]
     (let [key (get-secret-key options)]
       (assert (valid-secret-key? key) \"the secret key must be exactly 16 bytes\")
       (CookieStore. key options))))")


