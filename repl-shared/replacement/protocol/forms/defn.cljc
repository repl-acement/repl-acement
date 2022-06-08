(ns replacement.protocol.forms.defn
  "A `defn` form describing a function is a sequence of parts.
  The first parts (name, docstring and meta) are fixed, only `name` is mandatory.
  The next and last part (the tail) is a sequence of a list of parts that can be repeated.
  Iff there is only 1 element in the tail sequence, the parts are stripped out of the list
  and merged with the function sequence. In all other cases, each of the tail items are
  retained in their own list. An extra metadata map can be provided for multi-arity forms.

  This namespace is concerned with breaking the defn forms down using `spec/conform` and
  putting them back together with `unform`. Editing by a human or a function may happen
  in between providing that it remains `unform`-able. This is relaxed for humans but not
  for functions."
  (:require
    [clojure.data :as data]
    [clojure.spec.alpha :as s]
    [clojure.walk :as walk]
    #?(:clj  [clojure.core.specs.alpha :as specs]
       :cljs [replacement.protocol.cljs-fn-specs :as specs])
    [replacement.protocol.patched-core-specs :as core-specs]
    [replacement.protocol.data :as data-specs]
    [replacement.protocol.state :as state]
    [replacement.protocol.text-parsing :as text-parsing]))

(defn- unformed-arity-data
  [params+body]
  (let [params+body-value (s/unform ::specs/params+body params+body)
        params-value (first params+body-value)
        pre-post? (map? (second params+body-value))
        pre-post (when pre-post? (second params+body-value))
        body-value (if pre-post? (drop 2 params+body-value)
                                 (drop 1 params+body-value))]
    {:params-value params-value
     :params-count (count params-value)
     :body         body-value
     :pre-post-map pre-post}))

(defn conformed-arity-data
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        single-arity? (= :arity-1 (first fn-tail))
        extra-meta (when-not single-arity? (get-in fn-tail [:arity-n :arity-map]))]
    {:single-arity? single-arity?
     :extra-meta    extra-meta}))

(defn split-defn-args
  [conformed-defn-args]
  (let [{:keys [fn-tail]} conformed-defn-args
        {:keys [single-arity?] :as props} (conformed-arity-data conformed-defn-args)
        arity-data (if single-arity?
                     (-> fn-tail last unformed-arity-data vector)
                     (map unformed-arity-data (-> fn-tail last :bodies)))]
    (merge props {:arity-data arity-data})))


(defn set-params
  "Insert `new-value` for :params into the params at `index` (or 0) of the `conformed-data`"
  [conformed-data new-value index]
  (let [index (or index 0)
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
  (let [index (or index 0)
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
  (let [index (or index 0)
        index-count (volatile! 0)
        new-body (if (seq? (first new-value)) new-value [new-value])]
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

#_"The map for each of the parts of a conforming defn"

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
                             :spec   ::core-specs/param-list
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

#_"Data to automate conformed defn form data to form specific properties"
(def fixed-parts [:defn.name :defn.docstring :defn.meta])
(def arity-parts [:defn.params :defn.pre-post :defn.body])
(def multi-arity-attrs [:extra-meta])

(defn- text->spec-data
  [text]
  (let [data (first (text-parsing/text->edn-forms text))
        conformed (s/conform ::data-specs/defn-form data)
        explain-data (when (s/invalid? conformed) (s/explain-data ::data-specs/defn-form data))
        unformed (when-not (s/invalid? conformed)
                   (s/unform ::data-specs/defn-form conformed))]
    {:defn-text         (pr-str unformed)
     :defn-conformed    conformed
     :defn-explain-data explain-data
     :defn-unformed     unformed}))

(defn conformed->spec-data
  [conformed]
  (let [unformed (when-not (s/invalid? conformed)
                   (s/unform ::data-specs/defn-form conformed))]
    {:defn.text         (pr-str unformed)
     :defn.conformed    conformed
     :defn.explain-data (when (s/invalid? conformed)
                          (s/explain-data ::data-specs/defn-form conformed))
     :defn.unformed     unformed}))

(defn conformed-form->spec-data
  [{:keys [conformed]}]
  (let [unformed (when-not (s/invalid? conformed)
                   (s/unform ::data-specs/form conformed))]
    {:text         (pr-str unformed)
     :conformed    conformed
     :explain-data (when (s/invalid? conformed) (s/explain-data ::data-specs/form conformed))
     :unformed     unformed}))

(defn update-from-parts
  "Updates the value of `property-name` to `new-value` in the `conformed-data` map.
  If `new-value` is not conforming, the update is nil and the spec explain-data is provided"
  [conformed-data property-name new-value arity-index]
  (let [{:keys [spec path arity?]} (get parts property-name)
        input (text-parsing/text->edn-forms new-value)
        conformed-part (s/conform spec input)
        update (when-not (s/invalid? conformed-part)
                 (if arity?
                   (path conformed-data conformed-part arity-index)
                   (path conformed-data conformed-part)))
        explain (when (s/invalid? conformed-part)
                  (s/explain-data spec input))]
    {:value   new-value
     :input   input
     :update  update
     :explain explain}))

(defn update-state
  [state text form-id+digest]
  (let [{:keys [defn-conformed] :as text-updates} (text->spec-data text)
        form-id (namespace form-id+digest)]
    (if-not defn-conformed
      state
      (state/update-state state text-updates :defn-conformed form-id))))

(defn state-diff
  [state current-id previous-id]
  (let [current-state (split-defn-args (get-in state [current-id :defn-conformed :defn-args]))
        original-state (split-defn-args (get-in state [previous-id :defn-conformed :defn-args]))]
    (data/diff current-state original-state)))

(defn property-change?
  [diff1 diff2 path change-tag]
  (let [pc1 (get-in diff1 path)
        pc2 (get-in diff2 path)
        change? (not= pc1 pc2)]
    (cond-> {change-tag {:change? change?}}
            change? (assoc-in [change-tag :diff] (data/diff pc1 pc2)))))

(defn param-count-changes
  [[diff1 diff2 _]]
  (let [changes (property-change? diff1 diff2 [:arity-data :params-count] :params-count)
        structural? (true? (get-in changes [:params-count :change?]))]
    (merge changes {:structural-change? structural?})))

(defn param-value-changes
  [[diff1 diff2 _]]
  (property-change? diff1 diff2 [:arity-data :params-value] :params-value))

(defn param-changes
  [diff]
  (let [count-changes (param-count-changes diff)
        value-changes (param-value-changes diff)]
    (merge count-changes value-changes)))

(defn fn-name-changes
  [[diff1 diff2 _]]
  (property-change? diff1 diff2 [:fn-name] :fn-name))

(defn fn-changes
  [diff]
  (merge (fn-name-changes diff)
         {:structural-change? false}))

(defn prepost-changes
  [[diff1 diff2 _]]
  (merge (property-change? diff1 diff2 [:arity-data :pre-post-map] :pre-post-map)
         {:structural-change? false}))

(defn docstring-changes
  [[diff1 diff2 _]]
  (merge (property-change? diff1 diff2 [:docstring] :docstring)
         {:structural-change? false}))

(defn is-single-arity-fn-change
  [[diff1 diff2 _]]
  (let [changes (property-change? diff1 diff2 [:single-arity?] :single-arity)
        structural? (true? (get-in changes [:single-arity :change?]))]
    (merge changes {:structural-change? structural?})))

(defn arity-change-data
  [[index arity-diff]]
  {:index            index
   :param-changes    (param-changes arity-diff)
   :pre-post-changes (prepost-changes arity-diff)})

(defn state-change-data
  [{:keys [outer-diff arity-diff multi-diffs] :as diffs}]
  (merge diffs
         {:arity-changes (map arity-change-data (:arity-diffs multi-diffs))}
         {:fn-changes             (arity-change-data [0 (:diff arity-diff)])
          :single-arity-fn-change (is-single-arity-fn-change (:diff arity-diff))
          :docstring-changes      (docstring-changes (:diff outer-diff))
          :fn-name-changes        (fn-changes (:diff outer-diff))}))

(defn highlight-changes
  [{:keys [multi-diffs] :as diffs}]
  (let [outer-diffs (keep (fn [[k v]]
                            (when (map? v)
                              (when (:structural-change? v)
                                (hash-map k v))))
                          diffs)
        inner-diffs (keep (fn [[k v]]
                            (when (map? v)
                              (when (:structural-change? v)
                                (hash-map k v))))
                          multi-diffs)]
    (concat outer-diffs inner-diffs)))

;; TODO next ...
;; make the diff interesting
;; and then have tweaks on the UI for accessing the state
;; plus dealing with CLJC bugs

(defn diff-multi-arity-data
  "Compare the arities with the same parameter count"
  [arity-data1 arity-data2]
  (reduce (fn [results arity-data1-item]
            (concat results
                    (reduce-kv (fn [inner-results index arity-data2-item]
                                 (if (= (:params-count arity-data1-item) (:params-count arity-data2-item))
                                   (let [[d1 d2 _same :as arity-diff] (data/diff arity-data1-item arity-data2-item)]
                                     (if (or d1 d2)
                                       (conj inner-results [index arity-diff])
                                       inner-results))
                                   inner-results))
                               results (zipmap (range) arity-data2))))
          [] arity-data1))

(defn diff-multis
  [multi1 multi2]
  (let [arity-data-1 (:arity-data multi1)
        arity-data-2 (:arity-data multi2)
        arity-diffs (diff-multi-arity-data arity-data-1 arity-data-2)
        arity-counts (let [count1 (count arity-data-1)
                           count2 (count arity-data-2)
                           [c1 c2 _same :as count-diff] (data/diff count1 count2)]
                       (cond-> {:count1 count1
                                :count2 count2
                                :diff   count-diff}
                               (or c1 c2) (merge {:structural-change? true})))
        param-count-sets (let [set1 (set (map :params-count arity-data-1))
                               set2 (set (map :params-count arity-data-2))
                               [s1 s2 _same :as set-diff] (data/diff set1 set2)]
                           (cond-> {:set1 set1
                                    :set2 set2
                                    :diff set-diff}
                                   (or s1 s2) (merge {:structural-change? true})))
        change? (boolean (or (seq arity-diffs)
                             (:structural-change? arity-counts)
                             (:structural-change? param-count-sets)))]
    {:change?          change?
     :arity-diffs      arity-diffs
     :arity-counts     arity-counts
     :param-count-sets param-count-sets
     ;; TODO
     :meta-diff        []}))

(defn outer-part-changes
  [current-defn-outer original-defn-outer]
  (let [[old new _same :as diff] (data/diff current-defn-outer original-defn-outer)]
    {:change? (boolean (or old new))
     :diff    diff}))

(defn single-arity-changes
  [arity-data1 arity-data2]
  (let [[old new _same :as diff] (data/diff arity-data1 arity-data2)]
    {:change? (boolean (or old new))
     :diff    diff}))

(defn semantic-diff-data
  [db state-id1 state-id2]
  (let [current (get-in db [state-id1])                     ;; TODO track type evolution
        original (get-in db [state-id2])

        current-conformed (:defn-conformed current)
        original-conformed (:defn-conformed original)

        current-defn (:defn-args current-conformed)
        original-defn (:defn-args original-conformed)
        defn-diff (data/diff current-defn original-defn)    ;; For debug / eye-ball checks

        current-defn-outer (dissoc current-defn :fn-tail)
        original-defn-outer (dissoc original-defn :fn-tail)
        outer-diff (outer-part-changes current-defn-outer original-defn-outer)

        current-defn-args (split-defn-args current-defn)
        original-defn-args (split-defn-args original-defn)
        single-arity-diff? (and (:single-arity? current-defn-args)
                                (:single-arity? original-defn-args))
        ;; TODO only dealing with two singles or two multis right now, need to handle a mix
        arity-diff (when single-arity-diff?
                     (single-arity-changes (assoc current-defn-args :arity-data (first (:arity-data current-defn-args)))
                                           (assoc original-defn-args :arity-data (first (:arity-data original-defn-args)))))
        multi-diffs (diff-multis current-defn-args original-defn-args)]
    {:defn-diff          defn-diff
     :outer-diff         outer-diff
     :single-arity-diff? single-arity-diff?
     :arity-diff         arity-diff
     :multi-diffs        multi-diffs}))


(comment

  (require '[replacement.protocol.events :as events])
  (require '[replacement.protocol.text-parsing :as text-parsing])

  (def hello-db (events/add+index-ns {} (->> text-parsing/hello-sample
                                             (text-parsing/text->edn-forms)
                                             (text-parsing/whole-ns->spec-form-data)
                                             (events/add-reference-data))))

  (def clojured-DB
    (let [form-id (-> (:form-name->id hello-db) :replacement.greet/hello-world)
          updates ["(defn hello-world \"Welcome to repl-acement\" [] \"Hello world\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello worl\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello wor\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello wo\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello w\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello C\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Cl\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Clo\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Cloj\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Cloju\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Clojur\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Clojure\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello ClojureD\")"]]
      (reduce (fn [state change]
                (update-state state change form-id))
              hello-db updates)))

  (require '[clojure.data :as data])

  (let [current-state-id (first (:changelog clojured-DB))
        original-state-id (last (:changelog clojured-DB))
        current-state (split-defn-args (get-in clojured-DB [current-state-id :defn-conformed :defn-args]))
        original-state (split-defn-args (get-in clojured-DB [original-state-id :defn-conformed :defn-args]))]
    (events/pprint [:current-state current-state])
    (events/pprint [:original-state original-state])
    (data/diff current-state original-state))

  )