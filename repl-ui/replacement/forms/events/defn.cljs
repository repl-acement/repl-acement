(ns replacement.forms.events.defn
  "A `defn` form describing a function is a sequence of parts.
  The first parts (name, docstring and meta) are fixed, only `name` is mandatory.
  The next and last part (the tail) is a sequence of a list of parts that can be repeated.
  Iff there is only 1 element in the tail sequence, the parts are stripped out of the list
  and merged with the function sequence. In all other cases, each of the tail items are
  retained in their own list. An extra metadata map can be provided for multi-arity forms.

  This namespace is concerned with breaking the defn forms down using `spec/conform` and
  putting them back together with `unform`. Editing by a human or a function can happen
  in between.

  ;; It is organized to match the life-cycle:
  ✓ event to set the whole view of a specific form-id
  ✓ --> fn to write the parts CMs to set the parts view of the whole
  ✓ event to transact changes (keystrokes) on the whole form
  ✓ --> fn to ripple out change to appropriate parts
  ✓ events to transact changes (keystrokes) on any of the form parts
  ✓ --> fn to reflect back the part change to whole"
  (:require
    [cljs.tools.reader.edn :as rdr]
    [clojure.core.async]
    [cljs.spec.alpha :as s]
    [nextjournal.clojure-mode.extensions.formatting :as format]
    [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
    [replacement.forms.parser.parse :as forms-parse]
    [replacement.protocol.cljs-fn-specs :as core-fn-specs]
    [replacement.protocol.data :as data-specs]
    [replacement.protocol.patched-core-specs]
    [replacement.ui.helpers :refer [js->cljs]]
    [replacement.structure.wiring :as wiring]
    [zprint.core :refer [zprint-file-str]]
    [clojure.edn :as edn]
    [clojure.walk :as walk]
    [replacement.forms.events.common :as common]))

(defn- unformed-arity-data
  [params+body]
  (let [params+body-value (s/unform ::core-fn-specs/params+body params+body)
        params-value      (first params+body-value)
        pre-post?         (map? (second params+body-value))
        pre-post          (when pre-post? (second params+body-value))
        body-value        (if pre-post? (drop 2 params+body-value)
                                        (drop 1 params+body-value))]
    {:params-value params-value
     :body         body-value
     :pre-post-map pre-post}))

(defn conformed-arity-data
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        single-arity? (= :arity-1 (first fn-tail))
        extra-meta    (when-not single-arity? (get-in fn-tail [:arity-n :arity-map]))]
    {:single-arity? single-arity?
     :extra-meta    extra-meta}))

(defn split-defn-args
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        {:keys [single-arity?] :as props} (conformed-arity-data conformed-defn-args)
        arity-data (if single-arity?
                     (-> fn-tail last unformed-arity-data vector)
                     (map unformed-arity-data (-> fn-tail last :bodies)))]
    (merge props {:defn-args  conformed-defn-args
                  :arity-data arity-data})))

(defn set-params
  "Insert `new-value` for :params into the params at `index` (or 0) of the `conformed-data`"
  [conformed-data new-value index]
  (let [index       (or index 0)
        index-count (volatile! 0)]
    (walk/postwalk
      (fn [node]
        (cond
          (and (:params node)
               (map? node))
          (let [node' (cond-> node
                              (= index @index-count) (assoc-in [:params] new-value))]
            (vswap! index-count inc)
            node')
          :default node))
      conformed-data)))

(defn set-pre-post
  "Insert `new-value` for :prepost into the params at `index` (or 0) of the `conformed-data`"
  [conformed-data new-value index]
  (let [index       (or index 0)
        index-count (volatile! 0)]
    (walk/postwalk
      (fn [node]
        (cond
          (and (= index @index-count)
               (vector? node)) (cond
                                 ;; There is an existing prepost property at the index
                                 (and (= (first node) :prepost+body))
                                 [:prepost+body (assoc (last node) :prepost new-value)]

                                 ;; There is not an existing prepost property at the index
                                 (and (= (first node) :body)
                                      (= (first (last node)) :body))
                                 [:body [:prepost+body {:prepost new-value
                                                        :body    (last (last node))}]]

                                 :default node)

          (and (:params node)
               (map? node)) (do (vswap! index-count inc)
                                node)
          :default node))
      conformed-data)))

(defn set-body
  "Insert `new-value` for :body into the params at `index` (or 0) of the `conformed-data`"
  [conformed-data new-value index]
  (let [index       (or index 0)
        index-count (volatile! 0)
        new-body    (if (seq? (first new-value)) new-value [new-value])]
    (walk/postwalk
      (fn [node]
        (cond
          (and (= index @index-count)
               (vector? node)) (cond
                                 ;; There is an existing prepost property at the index
                                 (and (= (first node) :prepost+body))
                                 [:prepost+body (assoc (last node) :body new-body)]

                                 ;; There is not an existing prepost property at the index
                                 (and (= (first node) :body)
                                      (= (first (last node)) :body))
                                 [:body [:body new-body]]

                                 :default node)

          (and (:params node)
               (map? node)) (do (vswap! index-count inc)
                                node)
          :default node))
      conformed-data)))

(defn set-extra
  "Insert `new-value` for :attr-map after the multi-arities in the `conformed-data`"
  [conformed-data new-value]
  (let [tail (get-in conformed-data [:defn-args :fn-tail])]
    (if (some-> tail first (= :arity-n))
      (update-in conformed-data [:defn-args :fn-tail]
                 (fn [[arity bodies]]
                   [arity (assoc bodies :attr-map new-value)]))
      conformed-data)))

;; Specs that are in clojure.core.specs.alpha but are not named
(s/def ::optional-string (s/nilable string?))
(s/def ::optional-map (s/nilable map?))
(s/def ::body any?)

(def parts {:defn.name      {:name :fn-name
                             :spec simple-symbol?
                             :path #(assoc-in %1 [:defn-args :fn-name] %2)}
            :defn.docstring {:name :docstring
                             :spec ::optional-string
                             :path #(assoc-in %1 [:defn-args :docstring] %2)}
            :defn.meta      {:name :meta
                             :spec ::optional-map
                             :path #(assoc-in %1 [:defn-args :meta] %2)}
            :defn.params    {:name   :params-value
                             :spec   ::core-fn-specs/param-list
                             :arity? true
                             :path   #(set-params %1 %2 %3)}
            :defn.pre-post  {:name   :pre-post-map
                             :spec   ::optional-map
                             :arity? true
                             :path   #(set-pre-post %1 %2 %3)}
            :defn.body      {:name   :body
                             :spec   ::body
                             :arity? true
                             :path   #(set-body %1 %2 %3)}
            :extra-meta     {:name :attr-map
                             :spec ::optional-map
                             :path #(set-extra %1 %2)}})

(defn- conformed-data->properties
  "Function to automate conformed form data to form specific properties"
  [data props-map]
  (let [name<->component (reduce-kv (fn [m k v]
                                      (merge m (hash-map (:name v) k)))
                                    {} props-map)]
    (-> (reduce-kv (fn [data k v]
                     (-> data
                         (dissoc k)
                         (assoc v (data k))))
                   data name<->component)
        (select-keys (vals name<->component)))))

;; Data to automate conformed form data to form specific properties
(def fixed-parts [:defn.name :defn.docstring :defn.meta])
(def arity-parts [:defn.params :defn.pre-post :defn.body])
(def multi-arity-attrs [:extra-meta])


(defn- fixed-items-update-cms!
  "Update the parts of this form that are fixed when it is conformed, including any that are nillable."
  [{:keys [defn-args] :as db} triggering-cm-key]
  (let [properties       (conformed-data->properties defn-args (select-keys parts fixed-parts))
        cm-keys          (->> fixed-parts
                              (map wiring/comp-name->cm-name)
                              (filter #(not= triggering-cm-key %)))
        cms-with-changes (reduce (partial common/update-cm-states db properties) [] cm-keys)]
    (common/update-cms! cms-with-changes)))

(defn- arity-update-cms!
  "Update the parts of this form that are variable when it is conformed, including any that are nillable."
  [db triggering-cm-key an-arity-data]
  (let [properties       (conformed-data->properties an-arity-data (select-keys parts arity-parts))
        cm-keys          (->> arity-parts
                              (map wiring/comp-name->cm-name)
                              (filter #(not= triggering-cm-key %)))
        cms-with-changes (reduce (partial common/update-cm-states db properties) [] cm-keys)]
    (common/update-cms! cms-with-changes)))

(defn- attrs-update-cms!
  "Update the extra attributes for multi-arity forms, if present"
  [{:keys [defn-args] :as db} triggering-cm-key]
  (let [arity-data       (-> defn-args :fn-tail last)
        properties       {:extra-meta (:attr-map arity-data)}
        cm-keys          (->> multi-arity-attrs
                              (map wiring/comp-name->cm-name)
                              (filter #(not= triggering-cm-key %)))
        cms-with-changes (reduce (partial common/update-cm-states db properties) [] cm-keys)]
    (common/update-cms! cms-with-changes)))

(reg-event-db
  ::arity-update-cms
  (fn [{:keys [current-form-data] :as db} [_ index]]
    (let [arity-data (get-in current-form-data [:form :arity-data])]
      (arity-update-cms! db :none (nth arity-data index))
      (assoc db :arity-index index))))

(reg-event-db
  ::set-arity-index
  (fn [db [_ index]]
    (assoc db :arity-index index)))

(defn- text->spec-data
  [text]
  (let [data          (rdr/read-string text)
        conformed     (s/conform ::data-specs/defn-form data)
        explain-data  (and (= s/invalid? conformed) (s/explain-data ::data-specs/defn-form data))
        unformed      (or (= s/invalid? conformed) (s/unform ::data-specs/defn-form conformed))
        fn-properties {:defn.text         (-> unformed pr-str common/fix-width-format)
                       :defn.conformed    conformed
                       :defn.explain-data explain-data
                       :defn.unformed     unformed}
        fn-data       (split-defn-args (:defn-args conformed))]
    (merge fn-properties fn-data)))

(defn- conformed->spec-data
  [conformed]
  (let [unformed      (when-not (= s/invalid? conformed)
                        (s/unform ::data-specs/defn-form conformed))
        fn-properties {:defn.text         (-> unformed pr-str common/fix-width-format)
                       :defn.conformed    conformed
                       :defn.explain-data nil
                       :defn.unformed     unformed}
        fn-data       (split-defn-args (:defn-args conformed))]
    (merge fn-properties fn-data)))

(defn update-from-parts
  "Updates the value of `property-name` to `new-value` in the `conformed-data` map. If `new-value` is
  not conforming, the update is nil and the spec explain-data is provided"
  [conformed-data property-name new-value arity-index]
  (let [{:keys [spec path arity?]} (get parts property-name)
        input          (forms-parse/read-whole-string new-value)
        conformed-part (s/conform spec input)
        update         (when-not (s/invalid? conformed-part)
                         (if arity?
                           (path conformed-data conformed-part arity-index)
                           (path conformed-data conformed-part)))
        explain        (when (s/invalid? conformed-part)
                         (s/explain-data spec input))]
    {:value   new-value
     :input   input
     :update  update
     :explain explain}))

(reg-event-db
  ::part-edit
  (fn [db [_ part-cm-name tx]]
    ;; First update the transacting code-mirror
    (-> (get-in db [part-cm-name :cm]) (common/update-cm! tx))
    (if-not (js->cljs (.-docChanged tx))
      db
      (let [{:keys [arity-index visible-form-id]} db
            {:keys [ref-conformed]} (get db visible-form-id)
            part-name (wiring/cm-name->comp-name part-cm-name)
            text      (common/extract-tx-text tx)
            updates   (update-from-parts ref-conformed part-name text arity-index)]
        (if-not (:update updates)
          db
          (let [db' (merge db (conformed->spec-data (:update updates)))
                cm  (-> :db :defn.form.cm :cm)]
            (->> db' :defn.text (common/format-tx cm) (common/update-cm! cm))
            db'))))))

(defn parts-update!
  [{:keys [arity-data] :as db} source-cm-key]
  (fixed-items-update-cms! db source-cm-key)
  (arity-update-cms! db source-cm-key (first arity-data))
  (attrs-update-cms! db source-cm-key))

(reg-event-db
  ::set-form
  (fn [db [_ var-id]]
    (when-let [cm (get-in db [:defn.form.cm :cm])]
      (let [{:keys [ref-conformed ref-name]} (db var-id)
            visibility {:visible-form-id var-id :the-defn-form ref-name}
            db'        (merge db visibility (conformed->spec-data ref-conformed))]
        (->> db' :defn.text (common/format-tx cm) (common/update-cm! cm))
        (when (:defn.conformed db') (parts-update! db' :defn.form.cm))
        db'))))

;; TODO: Associate the change with the current form and persist it
(reg-event-db
  ::form-tx
  (fn [db [_ cm-name tx]]
    ;; First update the transacting code-mirror
    (-> (get-in db [cm-name :cm]) (common/update-cm! tx))
    (let [text (common/extract-tx-text tx)
          db'  (merge db (text->spec-data text))]
      (when (:defn.conformed db') (parts-update! db' cm-name))
      db')))

;; TODO - persist editing changes
; x event to persist changes at user behest
; x event to persist changes when the form under inspection is changed

;; TODO - set some warnings if not conformed



