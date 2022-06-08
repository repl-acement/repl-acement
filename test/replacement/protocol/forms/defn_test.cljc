(ns replacement.protocol.forms.defn-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data :as data]
            [replacement.protocol.forms.defn :as defn]
            [replacement.protocol.events :as events]
            [replacement.protocol.text-parsing :as text-parsing]))

(def body-updates
  ["(defn hello-world \"Welcome to repl-acement\" [] \"Hello world\")"
   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello ClojureD\")"])

(def name-updates
  ["(defn hello-world \"Welcome to repl-acement\" [] \"Hello ClojureD\")"
   "(defn hello-clojured \"Welcome to repl-acement\" [] \"Hello ClojureD\")"])

(def param-count-updates
  ["(defn hello-clojured [] \"Hello ClojureD\")"
   "(defn hello-clojured [place] (str \"Hello \" place))"])

(def param-name+body-updates
  ["(defn hello-clojured [_place-holder] \"Hello ClojureD\")"
   "(defn hello-clojured [place] (str \"Hello \" place))"])

(def body-text-updates
  ["(defn hello [place] (str \"Hello \" place))"
   "(defn hello [place] (str \"Hi \" place))"])

(def single-meta-updates
  ["(defn hello-clojured [place] (str \"Hello \" place))"
   "(defn hello-clojured {:api-version \"0.0.1\"} [place] (str \"Hello \" place))"])

(def single-prepost-updates
  ["(defn hello-world [place] (str \"Hello \" place))"
   "(defn hello-world [place] {:pre [(string? place)]} (str \"Hello \" place))"])

(def doc-string-updates
  ["(defn hello-world [] \"Hello ClojureD\")"
   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello ClojureD\")"])

(def single-to-multi-arity-updates
  ["(defn greeting [name] (str \"Hello \" name))"
   "(defn greeting ([] (greeting \"you\"))
                   ([name] (str \"Hello \" name)))"])

(def multi-arity-body-updates
  ["(defn greeting ([] (greeting \"you\"))
                   ([name] (str \"Hello \" name)))"
   "(defn greeting ([] (greeting \"you\"))
                   ([name] (str \"Good day \" name)))"])

;; TODO type evolution
#_(def var-type-updates
    ["(def hello-world \"Hello ClojureD\")"
     "(defn hello-world [] \"Hello ClojureD\")"])

(def hello-db (events/add+index-ns {} (->> text-parsing/hello-sample
                                           (text-parsing/text->edn-forms)
                                           (text-parsing/whole-ns->spec-form-data)
                                           (events/add-reference-data))))

(defn ->updated-db
  [updates]
  (let [form-id (-> (:form-name->id hello-db) :replacement.greet/hello-world)]
    (reduce (fn [state change]
              (defn/update-state state change form-id))
            hello-db updates)))

(deftest update-state-tests
  (let [db (->updated-db body-updates)]
    (testing "that updates produce new state per update and a changelog"
      (is (= (count body-updates) (- (dec (count (keys db))) ;; dec accounts for :changelog
                                     (count (keys hello-db)))))
      (is (= (count body-updates) (count (:changelog db)))))))

(defn get-diffs
  [db diff-path]
  (let [current-state-id (first (:changelog db))
        original-state-id (last (:changelog db))
        diff-data (defn/semantic-diff-data db current-state-id original-state-id)
        state-changes (defn/state-change-data diff-data)
        highlights (defn/highlight-changes state-changes)
        specific-diff (get-in state-changes diff-path)]
    {:diff-data     diff-data
     :state-changes state-changes
     :highlights    highlights
     :specific-diff specific-diff}))

(deftest diff-tests
  (testing "single-arity body updates"
    (let [db (->updated-db body-updates)
          {:keys [highlights specific-diff]} (get-diffs db [:multi-diffs :arity-diffs])
          [new-body old-body same] (-> specific-diff first last)]
      (is (= ["Hello ClojureD"] (get-in new-body [:body])))
      (is (= ["Hello world"] (get-in old-body [:body])))
      (is (zero? (get-in same [:params-count])))
      (is (= [] (get-in same [:params-value])))
      (is (empty? highlights))))

  (testing "function name updates"
    (let [db (->updated-db name-updates)
          {:keys [highlights specific-diff]} (get-diffs db [:fn-name-changes :fn-name :diff])]
      (is (= '[hello-clojured hello-world nil] specific-diff))
      (is (empty? highlights))))

  (testing "param count updates"
    (let [db (->updated-db param-count-updates)
          {:keys [highlights specific-diff]} (get-diffs db [:fn-changes :param-changes :params-count :diff])
          [new old same] specific-diff]
      (is (= 1 new))
      (is (= 0 old))
      (is (nil? same))
      (is (true? (-> highlights first :param-count-sets :structural-change?)))))

  (testing "param name and body updates"
    (let [db (->updated-db param-name+body-updates)
          {:keys [state-changes highlights specific-diff]} (get-diffs db [:fn-changes :param-changes :params-value :diff])
          [new-stuff old-stuff same-stuff] (get-in state-changes [:arity-diff :diff])]
      (is (true? (:single-arity-diff? state-changes)))
      (is (list? (get-in new-stuff [:arity-data :body 0])))
      (is (string? (get-in old-stuff [:arity-data :body 0])))
      (is (true? (:single-arity? same-stuff)))
      (is (= '[[place] [_place-holder] nil] specific-diff))
      (is (empty? highlights))))

  (testing "text in functions updates"
    (let [db (->updated-db body-text-updates)
          {:keys [state-changes highlights]} (get-diffs db [])
          [new-stuff old-stuff same-stuff] (get-in state-changes [:arity-diff :diff])]
      (is (true? (:single-arity-diff? state-changes)))
      (is (vector? (get-in new-stuff [:arity-data :body 0])))
      (is (= 2 (count (get-in new-stuff [:arity-data :body 0]))))
      (is (nil? (first (get-in new-stuff [:arity-data :body 0]))))
      (is (= "Hi " (last (get-in new-stuff [:arity-data :body 0]))))
      (is (vector? (get-in old-stuff [:arity-data :body 0])))
      (is (= 2 (count (get-in old-stuff [:arity-data :body 0]))))
      (is (nil? (first (get-in old-stuff [:arity-data :body 0]))))
      (is (= "Hello " (last (get-in old-stuff [:arity-data :body 0]))))
      (is (true? (:single-arity? same-stuff)))
      (is (empty? highlights))))

  (testing "single arity metadata updates"
    (let [db (->updated-db single-meta-updates)
          {:keys [state-changes highlights]} (get-diffs db [])
          [new-stuff old-stuff same-stuff] (get-in state-changes [:outer-diff :diff])]
      (is (true? (:single-arity-diff? state-changes)))
      (is (map? (-> new-stuff :meta)))
      (is (= "0.0.1" (-> new-stuff :meta :api-version)))
      (is (nil? old-stuff))
      (is (map? (-> same-stuff)))
      (is (= 'hello-clojured (-> same-stuff :fn-name)))
      (is (empty? highlights))))

  (testing "single arity pre-post updates"
    (let [db (->updated-db single-prepost-updates)
          {:keys [state-changes highlights]} (get-diffs db [])
          [new-stuff old-stuff same-stuff] (get-in state-changes [:arity-diff :diff])]
      (is (true? (:single-arity-diff? state-changes)))
      (is (map? (-> new-stuff :arity-data :pre-post-map)))
      (is (seq (-> new-stuff :arity-data :pre-post-map :pre)))
      (is (nil? (-> old-stuff :arity-data :pre-post-map)))
      (is (map? (-> same-stuff)))
      (is (= 1 (-> same-stuff :arity-data :params-count)))
      (is (empty? highlights))))

  (testing "docstring updates"
    (let [db (->updated-db doc-string-updates)
          {:keys [state-changes highlights specific-diff]} (get-diffs db [:docstring-changes :docstring :diff])
          [new-stuff old-stuff same-stuff] specific-diff]
      (is (true? (:single-arity-diff? state-changes)))
      (is (= "Welcome to repl-acement" new-stuff))
      (is (nil? old-stuff))
      (is (nil? same-stuff))
      (is (empty? highlights))))

  (testing "change from single to multi-arity"
    (let [db (->updated-db single-to-multi-arity-updates)
          {:keys [state-changes highlights]} (get-diffs db [])
          arity-change (first (filter :arity-counts highlights))
          param-count-changes (first (filter :param-count-sets highlights))]
      (is (false? (:single-arity-diff? state-changes)))
      (is (= [2 1 nil] (-> arity-change :arity-counts :diff)))
      (is (= [#{0} nil #{1}] (-> param-count-changes :param-count-sets :diff)))))

  (testing "multi-arity body updates"
    (let [db (->updated-db multi-arity-body-updates)
          {:keys [state-changes highlights specific-diff]} (get-diffs db [:multi-diffs :arity-diffs])
          [index arity-diff-data] (first specific-diff)
          [new old same] arity-diff-data]
      (is (false? (:single-arity-diff? state-changes)))
      (is (= 1 (count specific-diff)))
      (is (= 1 index))
      (is (vector? (:body new)))
      (is (= 1 (count (:body new))))
      (is (nil? (ffirst (:body new))))
      (is (= "Good day " (-> new :body first last)))
      (is (vector? (:body old)))
      (is (= 1 (count (:body old))))
      (is (nil? (ffirst (:body old))))
      (is (= "Hello " (-> old :body first last)))
      (is (= 'name (-> same :body first last)))
      (is (empty? highlights)))))

;; TODO find a way to check all properties that have changed
;; eg make sure they all have the key :change? true


