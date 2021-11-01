(ns replacement.ui.events
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [nextjournal.clojure-mode.extensions.formatting :as format]
    [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.ui.fn-specs :as fn-specs]
    [replacement.ui.core-specs :as core-specs]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.ui.ws :as ws]
    [replacement.specs.messages :as message-specs]
    [replacement.specs.user :as user-specs]
    [replacement.ui.wiring :as wiring]
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

(defn extract-tx-text
  [^js tx]
  (->> (.toJSON (.-newDoc tx)) js->cljs (apply str)))

(reg-fx
  ::code-mirror-update-view
  (fn [[{:keys [cm]} tx]]
    (.update cm #js [tx])))

(defn fn-tx-event->cm-name
  ([event-name]
   (fn-tx-event->cm-name event-name 0))
  ([event-name index]
   (-> (name event-name) (string/replace #"-tx$" (str "-cm-" index)) keyword)))

(defn fn-set-event->cm-name
  ([event-name]
   (fn-set-event->cm-name event-name 0))
  ([event-name index]
   (-> (name event-name) (string/replace #"^set-" "") (string/replace #"$" (str "-" index)) keyword)))

(defn fn-part->cm-name
  ([part-name]
   (fn-part->cm-name part-name 0))
  ([part-name index]
   (-> (name part-name) (string/replace #"$" (str "-cm-" index)) keyword)))

(defn fn-tx-event->part-name
  ([event-name]
   (fn-tx-event->part-name event-name 0))
  ([event-name index]
   (-> (name event-name) (string/replace #"-tx$" (str "-" index)) keyword)))


(defn fn-part->cm-name-simpler
  [part-name]
  (-> (name part-name)
      (string/replace #"$" "-cm")
      keyword))

(defn n-arity-parts-map
  [n-arity-data]
  (->> n-arity-data
       (map-indexed
         (fn [index arity-data]
           (let [arity (str "arity-" index)]
             (->> (reduce
                    (fn [parts part]
                      (let [part-name (keyword (str arity "." (name part)))]
                        (assoc parts part (fn-part->cm-name part-name))))
                    {} (keys arity-data))))))
       (into [])
       (hash-map :arity-n-data)))

(reg-event-fx
  ::code-mirror-tx
  (fn [{:keys [db]} [_ tx]]
    (let [{:keys [code-mirror-view]} db]
      (prn :tx tx)
      {:db                       (assoc db :tx tx)
       ::code-mirror-update-view [code-mirror-view tx]})))

(def fn-common-parts [:defn.name :defn.docstring :defn.meta])
(def fn-arity-parts [:defn.params :defn.prepost :defn.body])
(def fn-single-arity-parts (concat fn-common-parts fn-arity-parts))
(defn multi-arity-parts
  [arity]
  (map #(keyword (str "arity-" arity "." (name %))) fn-arity-parts))

(reg-fx
  ::fn-part-update
  (fn [[cm tx]]
    (.update cm #js [tx])
    (let [format-tx (format/format-all (.-state cm))]
      (.update cm #js [format-tx]))
    (re-frame/dispatch [::set-form-part])))

(defn fn-part-tx
  [{:keys [db]} [event-name tx index]]
  (prn ::fn-part-tx :event event-name)
  (let [part-name (fn-tx-event->part-name event-name index)
        cm-name   (fn-tx-event->cm-name event-name index)
        cm        (get-in db [cm-name :cm])]
    (prn :event event-name :index index :part-name part-name :text (extract-tx-text tx))
    {:db              (assoc db :tx tx part-name (extract-tx-text tx))
     ::fn-part-update [cm tx]}))

(reg-event-fx ::fn-name-tx fn-part-tx)
(reg-event-fx ::fn-doc-tx fn-part-tx)
(reg-event-fx ::fn-attrs-tx fn-part-tx)
(reg-event-fx ::fn-args-tx fn-part-tx)
(reg-event-fx ::fn-pp-tx fn-part-tx)
(reg-event-fx ::fn-body-tx fn-part-tx)

(reg-event-fx
  ::part-edit
  (fn [{:keys [db]} [part-cm-name tx]]
    (let [part-name (wiring/cm-name->comp-name part-cm-name)
          cm        (get-in db [part-cm-name :cm])]
      {:db              (assoc db :tx tx
                                  part-cm-name cm
                                  part-name (extract-tx-text tx))
       ::fn-part-update [cm tx]})))


(reg-fx
  ::fn-whole-edit
  (fn [[part-cm tx]]
    (let [{:keys [cm]} part-cm]
      (.update cm #js [tx])
      (let [format-tx (format/format-all (.-state cm))]
        (.update cm #js [format-tx])))
    (re-frame/dispatch [::set-whole-form (extract-tx-text tx)])))

(reg-event-fx
  ::fn-whole-form-tx
  (fn [{:keys [db]} [event-name tx]]
    (let [cm-name (fn-tx-event->cm-name event-name)
          cm-map  (get db cm-name)]
      {:db             (assoc db :tx tx)
       ::fn-whole-edit [cm-map tx]})))


(reg-event-db
  ::set-code-mirror-view
  (fn [db [_ view]]
    (assoc db :code-mirror-view view)))

(reg-event-db
  ::set-cm+name
  (fn [db [_ cm comp-name]]
    (assoc db comp-name {:cm cm :name comp-name})))

(reg-event-db
  ::set-result-code-mirror-view
  (fn [db [_ view]]
    (assoc db :result-code-mirror-view view)))

(reg-event-db
  ::other-user-code-mirror
  (fn [db [_ code-mirror user]]
    (let [user-key   (first user)
          cm-entries (:other-user-code-mirrors db)
          cm-entry   (assoc {} user-key code-mirror)
          cm-update  (merge cm-entries cm-entry)]
      (assoc db :other-user-code-mirrors cm-update))))

(defn- arity-data
  [params+body]
  (let [params+body-value (s/unform ::fn-specs/params+body params+body)
        params-value      (first params+body-value)
        pp?               (map? (second params+body-value))
        pp                (when pp? (second params+body-value))
        body-value        (last params+body-value)]
    {:params-value params-value
     :body         body-value
     :pre-post-map pp}))

(defn split-defn-args
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        single-arity? (= :arity-1 (first fn-tail))
        arity-data    (if single-arity?
                        (arity-data (-> fn-tail last))
                        (map arity-data (-> fn-tail last :bodies)))]
    (merge conformed-defn-args
           {:single-arity? single-arity?
            :arity-data    arity-data})))

(reg-fx
  ::update-cms
  (fn [changes]
    (doall (reduce (fn [_ {:keys [cm tx]}]
                     (.update cm #js [tx])
                     (let [format-tx (format/format-all (.-state cm))]
                       (.update cm #js [format-tx])))
                   [] changes))))

(reg-event-fx
  ::transact-over-cms
  (fn [{:keys [db]} [_ fn-properties]]
    (let [fn-parts         (keys (select-keys fn-properties fn-single-arity-parts))
          cms-with-changes (reduce (fn [cms fn-part]
                                     (let [cm-name    (fn-part->cm-name fn-part)
                                           cm         (get-in db [cm-name :cm])
                                           cm-state   (-> cm .-state)
                                           doc-length (-> cm-state .-doc .-length)
                                           new-text   (str (get fn-properties fn-part))
                                           tx         ^js (.update cm-state (clj->js {:changes {:from 0 :to doc-length :insert new-text}}))]
                                       (conj cms {:cm cm :tx tx})))
                                   [] fn-parts)]
      ;; NOTE: could be optimized to update only the changed ones
      {:db          db
       ::update-cms cms-with-changes})))

(defn- arity-data->properties
  [arity-data]
  {:defn.prepost (:pre-post-map arity-data)
   :defn.params  (:params-value arity-data)
   :defn.body    (:body arity-data)})

(defn- defn-data->properties
  [defn-data]
  {:defn.name      (:fn-name defn-data)
   :defn.docstring (:docstring defn-data)
   :defn.meta      (:meta defn-data)})

(defn update-cm-states
  [db defn-data cms cm-key]
  (if-let [cm (get-in db [cm-key :cm])]
    (let [defn-property-name (wiring/cm-name->comp-name cm-key)
          cm-state           (-> cm .-state)
          doc-length         (-> cm-state .-doc .-length)
          new-text           (pr-str (get defn-data defn-property-name))
          tx                 ^js (.update cm-state (clj->js {:changes {:from 0 :to doc-length :insert new-text}}))]
      (conj cms {:cm cm :tx tx}))
    cms))

(reg-event-fx
  ::fn-arity-1-update-cms
  (fn [{:keys [db]} [_]]
    (let [cm-keys          (map wiring/comp-name->cm-name fn-single-arity-parts)
          defn-data        (merge (defn-data->properties db)
                                  (arity-data->properties (:arity-data db)))
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))

(reg-event-fx
  ::fn-arity-n-update-cms
  (fn [{:keys [db]} [_ fn-properties index]]
    (let [cm-keys          (map (partial wiring/indexed-comp-name->cm-name index) fn-arity-parts)
          defn-data (arity-data->properties fn-properties)
          cms-with-changes (reduce (partial update-cm-states db defn-data) [] cm-keys)]
      {:db          db
       ::update-cms cms-with-changes})))


(reg-fx
  ::fn-whole-update
  (fn [[whole-form-cm whole-text]]
    (let [cm-state   (-> whole-form-cm .-state)
          doc-length (-> cm-state .-doc .-length)
          tx         ^js (.update cm-state (clj->js {:changes {:from 0 :to doc-length :insert whole-text}}))]
      (.update whole-form-cm #js [tx])
      (let [format-tx (format/format-all (.-state whole-form-cm))]
        (.update whole-form-cm #js [format-tx])))))

(defn- parts-text
  [db parts]
  (apply str (reduce
               (fn [text k]
                 (prn :k k :db-k (get db k) :db-attrs-0 (get db :fn-attrs-0) :text text)
                 (conj text (pr-str (get db k))))
               [] parts)))

(defn- multi-arity-text
  [parts]
  (str "(" (parts-text parts (keys parts)) ")"))

;; Add definition type (defn- or defn)
(reg-event-fx
  ::set-form-part
  (fn [{:keys [db]} [_]]
    (let [single-arity? (:single-arity? db)
          text          (if single-arity?
                          (parts-text db fn-single-arity-parts)
                          (let [common-text (parts-text db fn-common-parts)
                                multi-text  (map (fn [data]
                                                   (multi-arity-text data)) (:arity-n-data db))]
                            (prn :common-text common-text)
                            (prn :multi-text multi-text)
                            (apply str common-text multi-text)))
          whole-text    (str "(defn " text ")")
          formatted     (zprint-file-str whole-text "::set-form-part")
          whole-cm      (get-in db [:fn-whole-form-cm-0 :cm])]
      {:db               db
       ::fn-whole-update [whole-cm formatted]})))

(reg-fx
  ::fn-parts-update
  (fn [{:keys [single-arity? arity-data]}]
    (re-frame/dispatch [::fn-arity-1-update-cms])
    (if-not single-arity?
      (doall (map-indexed (fn [index data]
                            (re-frame/dispatch [::fn-arity-n-update-cms data index]))
                          arity-data)))))

(defn- text->conformed
  [text]
  (->> text (rdr/read-string)
       (s/conform ::core-specs/defn)))

(reg-event-db
  ::set-fn-id
  (fn [db [_ whole-form-text]]
    (let [{:keys [defn-args]} (text->conformed whole-form-text)
          {:keys [fn-name]} (split-defn-args defn-args)]
      (assoc db :fn-id (gensym fn-name)))))

(reg-event-fx
  ::transact-whole-defn-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (let [conformed     (text->conformed whole-form-text)
          unformed      (s/unform ::core-specs/defn conformed)
          fn-properties {:defn.text      whole-form-text
                         :defn.conformed conformed
                         :defn.unformed  unformed}
          fn-data       (split-defn-args (:defn-args conformed))
          updates       (merge fn-properties fn-data)]
      {:db               (merge db updates)
       ::fn-parts-update updates})))

(reg-event-fx
  ::set-whole-form
  (fn [{:keys [db]} [_ whole-form-text]]
    (prn ::set-whole-form-0)
    (let [fn-form            (rdr/read-string whole-form-text)
          conformed          (s/conform ::core-specs/defn fn-form)
          fn-data            (split-defn-args (:defn-args conformed))
          _                  (prn ::set-whole-form :fn-data fn-data)
          arity-data         (:arity-data fn-data)
          unformed           (s/unform ::core-specs/defn conformed)
          fn-properties      {:fn-text      whole-form-text
                              :fn-form      fn-form
                              :fn-conformed conformed
                              :fn-data      fn-data
                              :fn-unformed  unformed
                              :fn-name      (:fn-name fn-data)
                              :fn-doc       (:docstring fn-data)
                              :fn-attrs     (:meta fn-data)}
          arity-1-properties (arity-data->properties arity-data)
          arity-n-properties {:arity-n-data (map arity-data->properties arity-data)}
          updated-parts      (merge fn-properties arity-1-properties arity-n-properties fn-data)]
      {:db               (merge db fn-properties arity-1-properties arity-n-properties fn-data)
       ::fn-parts-update updated-parts})))

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


