(ns replacement.server.async-prepl
  (:require
    [clojure.core.async :as a]
    [clojure.core.server :refer [start-server]]
    [edamame.core :refer [parse-string parse-string-all]])
  (:import (java.io StringReader OutputStreamWriter BufferedReader InputStreamReader)
           (clojure.lang LineNumberingPushbackReader DynamicClassLoader)
           (org.apache.commons.codec.binary Hex)
           (java.security MessageDigest)
           (java.net ServerSocket Socket)))

(set! *warn-on-reflection* true)
(set! *default-data-reader-fn* tagged-literal)

(defn write-form!
  "Side-effecting: writes `form` for evaluation in the PREPL using the `writer`. Returns the form."
  [writer form]
  (binding [*out* writer
            *flush-on-newline* true
            *warn-on-reflection* true
            *print-meta* true]
    (prn form)
    form))

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
   i.e. substituting true for :unknown if necessary."
  [& body]
  `(binding [*read-eval* (if (= :unknown *read-eval*) true *read-eval*)]
     ~@body))

(defn bytes->hex
  [^bytes ba]
  (Hex/encodeHexString ba))

(defn digest
  [^String s]
  (when s
    (let [md5 (MessageDigest/getInstance "MD5")]
      (->> (.getBytes s)
           (.digest md5)
           bytes->hex))))

(defn retain-read-data
  [reader-special-token form]
  (letfn [(mk-reader-symbol [token] (->> token
                                         name
                                         (str "clojure-reader-")
                                         symbol))]
    (-> reader-special-token
        mk-reader-symbol
        (list form))))

(def edamame-options
  {:fn           (partial retain-read-data :function-literal)
   :syntax-quote (partial retain-read-data :syntax-quote)
   :deref        (partial retain-read-data :deref)
   :quote        (partial retain-read-data :quote)
   :regex        (partial retain-read-data :regex)
   :var          (partial retain-read-data :var)
   :read-eval    (partial retain-read-data :eval)
   :uneval       (partial retain-read-data :comment)
   :next         (partial retain-read-data :next)
   :end-location false
   :location?    seq?
   :auto-resolve '{:current clojure.core}})

(defn message->forms
  "Produce a list of forms read from string `s`. Any read failure throws"
  [s & options]
  (let [opts (or (first options) edamame-options)]
    (parse-string-all s opts)))

(defn- ex->data
  [ex phase]
  (assoc (Throwable->map ex) :phase phase))

(defn eval-forms*
  [{{:keys [reader writer]} :client}
   {:keys [forms user name-ns] :or {name-ns 'user}}]
  (let [form-count (count forms)
        written-forms (mapv #(write-form! writer %) forms)
        EOF (Object.)
        read-fn #(with-read-known (read reader false EOF))]
    (loop [output []
           output-map (read-fn)
           ret-tag-count 0]
      (let [out-map (assoc output-map :user user :input (nth written-forms ret-tag-count))
            output (conj output out-map)
            _ (prn :output output)
            ret-tag-count (if (= :ret (:tag output-map))
                            (inc ret-tag-count)
                            ret-tag-count)]
        (if (= form-count ret-tag-count)
          output
          (recur output (read-fn) ret-tag-count))))))

(defn eval-all-forms*
  "Evaluate the forms provided using the given `writer`"
  [client-opts {:keys [forms user name-ns]
                :or   {name-ns 'user}
                :as   message-data}]
  (println :message-data message-data)
  (try
    (let [default-data {:ns name-ns, :ms 0 :tag :ret :val "nil" :user user :input forms}]
      (if-not (seq forms)
        [(merge message-data default-data)]
        (eval-forms* client-opts message-data)))
    (catch Throwable ex
      (let [msg+ex (merge {:exception true
                           :ns        name-ns
                           :user      user
                           :ms        0
                           :tag       :ret
                           :val       (ex->data ex :eval-forms)}
                          message-data)]
        [msg+ex]))))

(defn sync-eval-forms [prepl-opts message-data]
  (eval-all-forms* prepl-opts message-data))

(defn async-forms-eval*
  [{{:keys [reader writer]} :client {:keys [out-ch]} :opts} user forms]
  (let [form-count (count forms)
        written-forms (mapv #(write-form! writer %) forms)
        EOF (Object.)
        read-fn #(with-read-known (read reader false EOF))]
    (loop [output-map (read-fn)
           ret-tag-count 0]
      (let [out-map (assoc output-map :user user :input (nth written-forms ret-tag-count))
            ret-tag-count (if (= :ret (:tag output-map))
                            (inc ret-tag-count)
                            ret-tag-count)]
        (a/put! out-ch out-map)
        (when-not (= form-count ret-tag-count)
          (recur (read-fn) ret-tag-count))))))

(defn- async-shared-eval*
  "Evaluate the form(s) provided in the string `form` using the given `writer`"
  [{{:keys [out-ch]} :opts :as client-opts} {:keys [form user name-ns]
                                             :or   {name-ns 'user}
                                             :as   message-data}]
  (prn :message-data message-data)
  (try
    (let [forms (message->forms form)
          default-data {:ns name-ns, :ms 0 :tag :ret :val "nil" :user user :input form}]
      (if-not (seq forms)
        (a/put! out-ch (merge message-data default-data))
        (async-forms-eval* client-opts user forms)))
    (catch Throwable trouble
      (a/put! out-ch (merge {:exception true
                             :ns        name-ns
                             :user      user
                             :ms        0
                             :tag       :ret
                             :val       (ex->data trouble :eval-forms)}
                            message-data)))))

(defn shared-eval [prepl-opts message-data]
  (future (async-shared-eval* prepl-opts message-data)))

(defn shared-prepl-server
  "Create a PREPL socket-server and return the port on which it is available"
  [opts]
  (let [socket-opts (merge {:port   0
                            :name   "shared-prepl-server"
                            :accept 'replacement.server.prepl-fork/io-prepl}
                           opts)
        current-thread (Thread/currentThread)
        cl (.getContextClassLoader current-thread)
        _ (.setContextClassLoader current-thread (DynamicClassLoader. cl))
        socket (start-server socket-opts)]
    {:socket socket
     :port   (.getLocalPort ^ServerSocket socket)}))

(defn prepl-rw
  "Get the prepl client reader & writer for a socket on the given `port`"
  [port]
  (let [client (Socket. "localhost" ^long port)
        reader (-> client .getInputStream InputStreamReader.
                   BufferedReader. LineNumberingPushbackReader.)
        writer (-> client .getOutputStream OutputStreamWriter.)]
    {:reader reader
     :writer writer}))

(defn shared-prepl
  ([]
   (shared-prepl {}))
  ([opts]
   (let [server (shared-prepl-server opts)]
     {:opts   opts
      :server server
      :client (prepl-rw (:port server))})))

;; This works for the UI but sucks...the background process grinds on for a long time before being killed.
;; Need to use the Executor Service and then stop the thread on cancel
(defn cancel
  [{:keys [server opts]}]
  (let [new-server (shared-prepl-server opts)]
    (.close ^ServerSocket (:socket server))
    {:opts   opts
     :server new-server
     :client (prepl-rw (:port new-server))}))

(def ^:private repl-setup
  ["(require '[clojure.repl :refer [source apropos dir pst doc find-doc]])"
   "(require '[clojure.java.javadoc :refer [javadoc]])"
   "(require '[clojure.pprint :refer [pp pprint]])"
   "(require '[clj-deps.core :refer [add-lib]])"
   "(set! *warn-on-reflection* true)"])

(defn init-prepl
  [{:keys [out-ch set-up-commands]
    :or   {set-up-commands repl-setup}
    :as   opts}]
  (let [channel-opts {:out-ch out-ch}
        prepl-opts (assoc (shared-prepl channel-opts) :init-count (count set-up-commands))
        digested (map #(assoc {} :form % :message-id (digest %)) set-up-commands)]
    (mapv #(shared-eval prepl-opts %) digested)
    (merge opts prepl-opts)))

(comment
  (defn run-cmds
    [writer]
    ;; TODO: use this as a .close option
    ;; TODO: prevent this from being sent by users!!
    (.write writer ":repl/quit\n"))


  ;; Use edamame to parse-strings - works for macros :)
  (let [options {:all          true
                 :row-key      :line
                 :syntax-quote (fn [form] (list 'clojure.core/quote form))
                 :col-key      :column
                 :end-location false
                 :location?    seq?
                 :auto-resolve '{:current clojure.core}}]
    (parse-string-all clj-core options)))
