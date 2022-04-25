(ns replacement.server.core-test
  (:require
    [clojure.core.async :as async]
    [clojure.spec.alpha :as spec]
    [clojure.string :as str]
    [clojure.test :refer :all]
    [replacement.server.async-prepl :as prepl]
    [replacement.server.await :refer [async-test-prepl gather-sync]]
    [replacement.specs.user :as user-specs])
  (:import (clojure.lang DynamicClassLoader)))

(defn- ->prepl-client
  []
  (let [current-thread (Thread/currentThread)
        classloader    (.getContextClassLoader current-thread)]
    ; Need DynamicClassLoader to support add-lib
    (.setContextClassLoader current-thread (DynamicClassLoader. classloader))
    (async-test-prepl (async/chan))))

(def test-user (user-specs/->user "test-user" "uid0"))

(defn- run-eval
  [prepl-opts eval-string]
  (prepl/shared-eval prepl-opts {:form eval-string :user test-user}))

(defn- sync-results
  [{:keys [opts] :as prepl-opts} eval-string
   & {:keys [expected-ret-count first-only?]
      :or   {expected-ret-count 1
             first-only?        true}}]
  @(run-eval prepl-opts eval-string)
  (let [results (gather-sync (:out-ch opts) expected-ret-count)]
    (if first-only? (first results) results)))

(deftest ^:version-tests version-tests
  (testing "That we are using 1.10 or later so that we have prepl"
    (let [prepl-opts  (->prepl-client)
          eval-string "*clojure-version*"
          {:keys [val]} (sync-results prepl-opts eval-string :first-only? true)
          {:keys [major minor]} (read-string val)]
      (is (= major 1))
      (is (>= minor 10)))))

(deftest ^:prepl-repl-tests prepl-repl-tests
  (testing "REPL options"
    (let [prepl-opts (->prepl-client)
          results    (sync-results prepl-opts "(doc map)" :first-only? false)
          outs       (butlast results)
          ret        (last results)]
      (is (= 4 (count outs)))
      (is (= (inc (count outs)) (count results)))
      (is (clojure.string/starts-with? (-> outs first :val) "----"))
      (map (fn [output] (let [{:keys [val tag]} output]
                          (is (= :out tag))
                          (is (string? val)))) outs)
      (let [{:keys [tag val]} ret]
        (is (= :ret tag))
        (is (nil? (read-string val))))

      (let [results (sync-results prepl-opts "(dir clojure.set)" :first-only? false)
            outs    (butlast results)
            ret     (last results)]
        (is (= 12 (count outs)))
        (is (= (inc (count outs)) (count results)))
        (is (clojure.string/starts-with? (-> outs first :val) "difference"))
        (map (fn [output] (let [{:keys [val tag]} output]
                            (is (= :out tag))
                            (is (string? val)))) outs)
        (let [{:keys [tag val]} ret]
          (is (= :ret tag))
          (is (nil? (read-string val)))))

      (let [results (sync-results prepl-opts "(find-doc #\"root.*cause\")" :first-only? false)
            outs    (butlast results)
            ret     (last results)]
        (is (>= (count outs) 20))
        (is (= (inc (count outs)) (count results)))
        (is (clojure.string/starts-with? (-> outs first :val) "----"))
        (map (fn [output] (let [{:keys [val tag]} output]
                            (is (= :out tag))
                            (is (string? val)))) outs)
        (let [{:keys [tag val]} ret]
          (is (= :ret tag))
          (is (nil? (read-string val)))))

      (let [results (sync-results prepl-opts "(source max)" :first-only? false)
            outs    (butlast results)
            ret     (last results)]
        (is (= 1 (count outs)))
        (is (= (inc (count outs)) (count results)))
        (is (clojure.string/starts-with? (-> outs first :val) "(defn max"))
        (map (fn [output] (let [{:keys [val tag]} output]
                            (is (= :out tag))
                            (is (string? val)))) outs)
        (let [{:keys [tag val]} ret]
          (is (= :ret tag))
          (is (nil? (read-string val)))))

      (let [results (sync-results prepl-opts "(apropos \"map\")" :first-only? false)
            ret     (first results)]
        (let [{:keys [tag val]} ret
              data (read-string val)]
          (is (= :ret tag))
          (is (seq? data))
          (is (>= (count data) 20)))))))

(deftest ^:basic-prepl-tests basic-prepl-tests
  (testing "Basic Clojure forms"
    (let [prepl-opts (->prepl-client)]

      ; self evaluating forms
      (let [xs ["42" "4.2" ":x" (pr-str "foo")]]
        (doall
          (map
            (fn [x]
              (let [{:keys [val]} (sync-results prepl-opts x)]
                (is (= x val))))
            xs)))

      ; vars
      (let [{:keys [tag val]} (sync-results prepl-opts "(def x 1)")]
        (is (= :ret tag))
        (is (= "#'user/x" val)))

      (let [{:keys [val]} (sync-results prepl-opts "x")]
        (is (= "1" val)))

      ; Literals
      (let [{:keys [val]} (sync-results prepl-opts "[123 \\newline ##Inf nil true :foo]")]
        (is (= "[123 \\newline ##Inf nil true :foo]" val)))

      (let [{:keys [val tag]} (sync-results prepl-opts "#'x")]
        (is (= :ret tag))
        (is (= "#'user/x" val))))))

(deftest ^:common-invocation-tests common-invocation-tests
  (testing "Standard, side effects and recursion"
    (let [prepl-opts (->prepl-client)]

      ; Standard invocations
      (let [{:keys [val]} (sync-results prepl-opts "(+ 3 4)")]
        (is (= "7" val)))

      (let [{:keys [val]} (sync-results prepl-opts "(inc x)")]
        (is (= "2" val)))

      (let [{:keys [val]} (sync-results prepl-opts "(range 2)")]
        (is (= "(0 1)" val)))

      ; Side effects
      (let [resp (sync-results prepl-opts "(println \"foo\")" :first-only? false)
            {:keys [val tag]} (first resp)]
        (is (= 2 (count resp)))
        (is (= "foo\n" val))
        (is (= :out tag))
        (is (= nil (get-in (last resp) [:eval-result :val]))))

      ; Loop / recur
      (let [input "(loop [results [1]]
                     (if (= [1 2 3] results)
                       results
                       (recur (conj results (inc (last results))))))"
            {:keys [val]} (sync-results prepl-opts input)]
        (is (= [1 2 3] (read-string val)))))))

(deftest ^:read-lambda-tests read-lambda-tests
  (testing "Lambdas"
    (let [prepl-opts (->prepl-client)
          {:keys [val]} (sync-results prepl-opts "(map #(inc %) (range 3))")]
      (is (= '(1 2 3) (read-string val)))

      (let [{:keys [val]} (sync-results prepl-opts "(map (fn [x] (inc x)) (range 3))")]
        (is (= '(1 2 3) (read-string val)))))))

(deftest ^:reflection-tests reflection-tests
  (testing "Reflection warnings"
    (let [client (->prepl-client)
          results (sync-results client "(.toString (identity \"foo\"))" :first-only? false)]
      (is (= 2 (count results))))))


(deftest ^:reader-char-tests reader-char-tests
  (testing "Reader special characters"
    (let [prepl-opts (->prepl-client)
          {:keys [val]} (sync-results prepl-opts "'l33t")]
      (is (= 'l33t (read-string val)))

      (let [{:keys [val]} (sync-results prepl-opts "(quote l33t)")]
        (is (= "l33t" val)))

      (let [_ (sync-results prepl-opts "(def atomic (atom 123))")
            {:keys [val]} (sync-results prepl-opts "@atomic")]
        (is (= "123" val)))

      (let [_ (sync-results prepl-opts "(defn type-hints [^String s] (clojure.string/trim s))")
            {:keys [val]} (sync-results prepl-opts "(type-hints \"  Hello-World    \")")]
        (is (= (pr-str "Hello-World") val)))

      (let [_ (sync-results prepl-opts "(def x 1)")
            _ (sync-results prepl-opts "(def lst '(a b c))")
            {:keys [val]} (sync-results prepl-opts "`(fred x ~x lst ~@lst 7 8 :nine)")]
        (is (= "(user/fred user/x 1 user/lst a b c 7 8 :nine)" val)))

      (let [{:keys [val]} (sync-results prepl-opts "#{1}")]
        (is (= "#{1}" val)))

      (let [{:keys [val]} (sync-results prepl-opts "(re-find #\"\\s*\\d+\" \"Hello-World42\")")]
        (is (= (pr-str "42") val))))))

(deftest ^:comment-tests comment-tests
  (testing "Various comment styles"
    (let [prepl-opts (->prepl-client)
          input      ";; 42"
          {:keys [tag val form] :as x} (sync-results prepl-opts input)]
      (is (= :ret tag))
      (is (= form input))
      (is (nil? (read-string val)))

      (let [input "(comment 42)"
            {:keys [tag val form]} (sync-results prepl-opts input)]
        (is (= :ret tag))
        (is (= form input))
        (is (nil? (read-string val))))

      (let [input "#_ xx"
            {:keys [tag val form]} (sync-results prepl-opts input)]
        (is (= :ret tag))
        (is (= form input))
        #_(is (nil? (read-string val))))

      (let [input "#_ xx 1"
            {:keys [tag val form]} (sync-results prepl-opts input)]
        (is (= :ret tag))
        (is (= form "1"))
        (is (= 1 (read-string val)))))))

(deftest ^:multi-form-tests multi-form-tests
  (testing "Multiple forms in a buffer"
    (let [prepl-opts    (->prepl-client)
          resp          (sync-results prepl-opts "(def x 1) x"
                                      :first-only? false
                                      :expected-ret-count 2)
          first-result  (first resp)
          second-result (last resp)]
      (is (= 2 (count resp)))

      (is (= :ret (:tag first-result)))
      (is (= "#'user/x" (:val first-result)))

      (is (= :ret (:tag second-result)))
      (is (= "1" (:val second-result))))))

(deftest ^:add-lib-tests add-lib-tests
  (testing "Test spec / add-lib"
    (let [prepl-opts (->prepl-client)
          add-ok     (sync-results prepl-opts "(add-lib 'vvvvalvalval/supdate {:mvn/version \"0.2.3\"})"
                                   :first-only? false)]
      (is (boolean? (read-string (:val (last add-ok)))))

      (let [req-ok (sync-results prepl-opts "(require '[vvvvalvalval.supdate.api :refer [supdate]])")]
        (is (nil? (read-string (:val req-ok)))))

      (let [def-ok (sync-results prepl-opts "(def my-input {:a 1 :b [1 2 3] :c {\"d\" [{:e 1 :f 1} {:e 2 :f 2}]} :g 0 :h 0 :i 0})")]
        (is (= 'var (first (read-string (:val def-ok))))))

      (let [sup-ok (sync-results prepl-opts "(supdate my-input {:a inc :b [inc] :c {\"d\" [{:e inc}]} :g [inc inc inc] :my-missing-key inc :i false})")
            result (read-string (:val sup-ok))]
        (is (map? result))
        (is (= 3 (:g result)))))))

#_(deftest ^:graceful-fail-tests graceful-fail-tests
  (testing "Test graceful failures for syntax and spec errors"
    (let [prepl-opts (->prepl-client)
          {:keys [tag val exception] :as x} (sync-results prepl-opts "(prn \"000")]
      (is (= :ret tag))
      (is (and (map? val)
               (true? exception)))

      (let [{:keys [tag val exception] :as x} (sync-results prepl-opts "(")]
        (is (= :ret tag))
        (is (and (map? val)
                 (true? exception))))

      (let [{:keys [tag val]} (sync-results prepl-opts "(defn x (+ 1 2))")
            {:keys [cause via trace data phase]}
            (binding [*default-data-reader-fn* prepl/nk-tag-reader]
              (read-string val))
            problems (::spec/problems data)
            spec     (::spec/spec data)
            value    (::spec/value data)
            args     (::spec/args data)]

        (is (= :ret tag))
        (is (= :macro-syntax-check phase))
        (is (= cause "Call to clojure.core/defn did not conform to spec."))
        (is (= 2 (count (filter :message via))))
        (is (and (vector? trace) (> (count trace) 10)))
        (is (= 2 (count problems)))
        (is (= 2 (count (keys spec))))
        (is (= '(x (+ 1 2)) value))
        (is (= '(x (+ 1 2)) args))))))

(deftest ^:in-namespaces in-namespaces
  (testing "Testing the support and use of namespaces"
    (let [prepl-opts (->prepl-client)]

      ; NS properties
      (let [{:keys [val ns tag]} (sync-results prepl-opts "*ns*")]
        (is (= ns "user"))
        (is (= :ret tag))
        (is (str/ends-with? val "\"user\"]"))
        (is (str/starts-with? val "#object")))

      ; creating / switching ns
      (let [_ (sync-results prepl-opts "(ns test123)")
            {:keys [val ns tag]} (sync-results prepl-opts "*ns*")]
        (is (= ns "test123"))
        (is (= :ret tag))
        (is (str/ends-with? val "\"test123\"]"))
        (is (str/starts-with? val "#object"))
        (let [{:keys [val ns tag]} (sync-results prepl-opts "*ns*")]
          (is (= ns "test123"))
          (is (= :ret tag))
          (is (str/ends-with? val "\"test123\"]"))
          (is (str/starts-with? val "#object"))))

      ; Functions
      (let [_ (sync-results prepl-opts "(in-ns 'user)")
            {:keys [val tag]} (sync-results prepl-opts "(defn ns-x2 [x] (+ x x))")]
        (is (= :ret tag))
        (is (= val "#'user/ns-x2")))

      (let [{:keys [val]} (sync-results prepl-opts "(ns-x2 17)")]
        (is (= "34" val)))

      (let [{:keys [val tag ns]} (sync-results prepl-opts "(in-ns 'repl-test)")]
        (is (= ns "repl-test"))
        (is (= :ret tag))
        (is (str/ends-with? val "\"repl-test\"]"))
        (is (str/starts-with? val "#object"))

        (let [{:keys [val tag ns]} (sync-results prepl-opts "(in-ns 'user)")]
          (is (= ns "user"))
          (is (= :ret tag))
          (is (str/ends-with? val "\"user\"]"))
          (is (str/starts-with? val "#object")))))))

(deftest ^:cancellation cancel-tests
  (testing "Cancel will create a valid new prepl and retain state"
    (let [prepl-opts  (->prepl-client)
          _resp       (sync-results prepl-opts "(def x 1)")
          _prepl-opts (prepl/cancel prepl-opts)
          prepl-opts  (->prepl-client)
          resp        (sync-results prepl-opts "x")
          resp*1      (sync-results prepl-opts "*1")]
      (is (= :ret (:tag resp)))
      (is (= "1" (:val resp)))
      (is (= :ret (:tag resp*1)))
      (is (= "1" (:val resp*1)))))
  (testing "Cancel will kill long running form evaluations"
    (let [prepl-opts  (->prepl-client)
          _resp       (sync-results prepl-opts "(def x 1)")
          _resp       (run-eval prepl-opts "(range)")
          _prepl-opts (prepl/cancel prepl-opts)
          prepl-opts  (->prepl-client)
          resp        (sync-results prepl-opts "x")
          resp*1      (sync-results prepl-opts "*1")]
      (is (= :ret (:tag resp)))
      (is (= "1" (:val resp)))
      (is (= :ret (:tag resp*1)))
      (is (= "1" (:val resp*1))))))

