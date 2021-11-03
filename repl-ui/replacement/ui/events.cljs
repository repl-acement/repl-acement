(ns replacement.ui.events
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [clojure.string :as string]
    [nextjournal.clojure-mode.extensions.formatting :as format]
    [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.ui.ws :as ws]
    [replacement.specs.messages :as message-specs]
    [replacement.specs.user :as user-specs]
    [taoensso.sente :as sente]
    [zprint.core :refer [zprint-file-str]]))

(def system-user (user-specs/->user "system" "0"))

(def default-server-timeout 3000)

(defn- key-bindings
  [os]
  (let [ckey (if (= os :macosx) "cmd" "ctrl")
        keys ["enter" "up" "down" "left" "right"]]
    (into
      {}
      (map
        (fn [c-key the-key]
          [(keyword the-key)
           (keyword (str (string/capitalize c-key) "-" (string/capitalize the-key)))])
        (repeat ckey) keys))))

(defonce ^:private os-data
         (let [app-version (.-appVersion js/navigator)
               os          (cond
                             (re-find #"Win" app-version) :windows
                             (re-find #"X11" app-version) :unix
                             (re-find #"Linux" app-version) :linux
                             (re-find #"Mac" app-version) :macosx
                             :else :unknown-os)]
           {:os os :key-bindings (key-bindings os)}))

; --- Events ---
(reg-event-db
  ::initialize-db
  (fn [_ _]
    (merge {::name             "repl-acement"
            ::other-visibility true}
           os-data)))

(reg-event-db
  ::network-status
  (fn [db [_ status]]
    (assoc db :network-status status)))

(reg-event-db
  ::client-uid
  (fn [db [_ uid]]
    (if-let [{::user-specs/keys [name]} (::user-specs/user db)]
      (assoc db ::user-specs/user (user-specs/->user name uid) ::user-specs/uid uid)
      (assoc db ::user-specs/user (user-specs/->user "Editor" uid) ::user-specs/uid uid))))

(defn pred-fails
  [problems]
  (some->> problems
           (map #(str "🤔  " (:val %) " is not " (:pred %)))
           (interpose "\n")
           (apply str)))

(defn default-reptile-tag-reader
  [tag val]
  {:nk-tag tag :nk-val (rdr/read-string (str val))})

;; TODO deal with ex-data / Throwable->map
(defn safe-read
  [val]
  (try
    (let [reader-opts {:default default-reptile-tag-reader :eof nil}]
      (rdr/read-string reader-opts val))
    (catch :default _ignored-ex
      (cond
        (= "#" (first val)) (symbol val)
        :else val))))

(def bugs "...\n")

;; TODO integrate a nice spec formatting library
;; TODO enable expansion or some other UI affordance for full exception data
(defn check-exception
  [val]
  (let [{:keys [cause via trace data phase]} (safe-read val)
        problems (:clojure.spec.alpha/problems data)
        spec     (:clojure.spec.alpha/spec data)
        value    (:clojure.spec.alpha/value data)
        args     (:clojure.spec.alpha/args data)]
    (or problems cause)))

(defn format-response
  [show-times? result]
  (let [{:keys [val tag ms user input]} result
        username       (::user-specs/name user)
        exception-data (check-exception val)]
    (cond
      exception-data
      (str input "\n"
           "=> " exception-data "\n\n")

      (= tag :err)
      (str input "\n" val "\n\n")

      (= tag :out)
      (str val)

      (= tag :ret)
      (str "[" username "] " input "\n" (when show-times? (str ms " ms "))
           "=> " (or val "nil") "\n\n"))))

(defn format-results
  [show-times? results]
  (doall (map (partial format-response show-times?) results)))

(reg-event-db
  ::input-history
  (fn [db [_ clojure-form]]
    (let [history (or (:input-history db) [])]
      (assoc db
        :input-history (conj history clojure-form)
        :history-index (count history)))))

(reg-fx
  ::set-cm-value
  (fn [{:keys [value code-mirror]}]
    (prn ::set-cm-value :value value :code-mirror code-mirror)))

(reg-event-db
  ::eval-result
  (fn [db [_ {:keys [form val] :as eval-result}]]
    (prn :eval-result eval-result)
    (if (= form "*clojure-version*")
      (assoc db :clojure-version val)
      (assoc db :latest-result eval-result
                :eval-results (conj (:eval-results db) eval-result)))))

(reg-event-fx
  ::show-times
  (fn [{:keys [db]} [_ show-times]]
    (let [code-mirror  (:eval-code-mirror db)
          show-times?  (true? show-times)
          eval-results (:eval-results db)
          str-results  (apply str (reverse (format-results show-times? eval-results)))]
      {:db            (assoc db :show-times show-times)
       ::set-cm-value {:value       str-results
                       :code-mirror code-mirror}})))

(reg-event-db
  ::eval-code-mirror
  (fn [db [_ code-mirror]]
    (assoc db :eval-code-mirror code-mirror)))

(reg-event-db
  ::team-bootstrap
  (fn [db [_ boot]]
    (assoc db :bootstrap boot)))

(reg-fx
  ::>repl-eval
  (fn [[source user form]]
    (when-not (string/blank? form)
      (ws/chsk-send!
        [:replacement/eval {:form      form
                            :team-name "team-name"
                            :source    source
                            :user      user
                            :forms     form}]
        (or (:timeout form) default-server-timeout))
      (when-not (= system-user user)
        (re-frame/dispatch [::input-history form])))))

(reg-event-fx
  ::eval
  (fn [{:keys [db]} _]
    (let [form (:current-form db)
          user (::user-specs/user db)]
      {:db          (assoc db :form-to-eval form)
       ::>repl-eval [:user user form]})))

(reg-event-db
  ::result-fn
  (fn [db [_ result-fn]]
    (assoc db :result-fn result-fn)))

(reg-event-fx
  ::clojure-version
  (fn [_ _]
    {::>repl-eval [:user system-user "*clojure-version*"]}))

(reg-fx
  ::>login
  (fn [{:keys [options timeout]}]
    (let [user (user-specs/->user (::user-specs/name options)
                                  (::user-specs/uid options))]
      (ws/chsk-send!
        [:replacement/login user]
        (or timeout default-server-timeout)
        (fn [reply]
          (if (and (sente/cb-success? reply)
                   (= reply :login-ok))
            (do (re-frame/dispatch [::logged-in-user user])
                (re-frame/dispatch [::clojure-version]))
            (js/alert "Login failed")))))))

(reg-event-fx
  ::login
  (fn [{:keys [db]} [_ login-options]]
    (when-let [uid (::user-specs/uid db)]
      {:db      (assoc db :proposed-user (::user-specs/name login-options)
                          ::user-specs/name nil)
       ::>login {:options (assoc login-options ::user-specs/uid uid)}})))

(reg-fx
  ::>logout
  (fn [{:keys [options timeout]}]
    (ws/chsk-send! [:replacement/logout options]
                   (or timeout default-server-timeout))))

(reg-event-fx
  ::logout
  (fn [{:keys [db]} _]
    (when-let [user (::user-specs/user db)]
      {:db       (dissoc db ::user-specs/user)
       ::>logout {:options user}})))

(reg-event-db
  ::show-add-lib-panel
  (fn [db [_ show?]]
    (assoc db :show-add-lib-panel show?)))

(reg-event-db
  ::analysis
  (fn [db [_ analysis]]
    (prn :analysis analysis)
    (assoc db :analysis analysis)))

(reg-event-fx
  ::add-lib
  (fn [cofx [_ {:keys [name version url sha maven] :as lib}]]
    (let [lib-spec (str "(add-lib '" (string/trim name) " {"
                        (if maven
                          (str ":mvn/version \""
                               (string/trim version) "\"")
                          (str ":git/url \""
                               (string/trim url) "\" :sha \""
                               (string/trim sha) "\""))
                        "})")]
      (re-frame/dispatch [::show-add-lib-panel false])
      {:db          (assoc (:db cofx) :proposed-lib lib)
       ::>repl-eval [:user system-user lib-spec]})))

;; ---------------------- Network sync

;; Share editing updates
(reg-fx
  ::>current-form
  (fn [[form user]]
    (ws/chsk-send!
      [:replacement/keystrokes (message-specs/->keystrokes form (or user system-user))])))

(reg-event-fx
  ::current-form
  (fn [{:keys [db]} [_ current-form]]
    (when-not (string/blank? (string/trim current-form))
      (let [user (::user-specs/user db)]
        {:db             (assoc db :current-form current-form)
         ::>current-form [current-form user]}))))

;; ------------------------------------------------------------------

(defn- next-prev [db f]
  (let [index   (f (:history-index db))
        history (:input-history db)
        item    (nth history index :not-found)]
    (if (= item :not-found)
      db
      (assoc db :history-index index
                :restore-item item
                :current-form item))))

(reg-event-fx
  ::history-prev
  (fn [{:keys [db]} _]
    (let [db (next-prev db dec)]
      {:db            db
       ::set-cm-value {:code-mirror (:code-mirror db)
                       :value       (:current-form db)}})))

(reg-event-fx
  ::history-next
  (fn [{:keys [db]} _]
    (let [db (next-prev db inc)]
      {:db            db
       ::set-cm-value {:code-mirror (:code-mirror db)
                       :value       (:current-form db)}})))

(reg-event-db
  ::logged-in-user
  (fn [db [_ user]]
    (assoc db ::user-specs/user user)))

(reg-event-db
  ::toggle-others
  (fn [db _]
    (assoc db ::other-visibility (not (::other-visibility db)))))

(reg-event-db
  ::users
  (fn [db [_ users]]
    (assoc db ::user-specs/users users)))

(reg-event-fx
  ::other-user-keystrokes
  (fn [{:keys [db]} [_ {:keys [::user-specs/user
                               ::message-specs/form]}]]
    (when-not (= user (::user-specs/user db))
      (let [editor-key   (keyword (::user-specs/name user))
            code-mirrors (:other-user-code-mirrors db)
            code-mirror  (get code-mirrors editor-key)]
        {:db            db
         ::set-cm-value {:code-mirror code-mirror
                         :value       form}}))))


