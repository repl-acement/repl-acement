(ns replacement.xform.defn
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as core-specs]
            [replacement.server.parser :as parser]))

(defn single-arity?
  [conformed-data]
  (-> conformed-data
      (get-in [:defn-args :fn-tail])
      (first)
      (= :arity-1)))

(defn unform-defn-params+body
  [conformed-data]
  (let [[arity arity-map] (get-in conformed-data [:defn-args :fn-tail])]
    (if (= :arity-1 arity)
      (s/unform ::core-specs/params+body arity-map)
      (map #(s/unform ::core-specs/params+body %) (:bodies arity-map)))))

(defn add-code-body-start
  [params+body code]
  (let [pre-post? (map? (second params+body))
        pre-post  (and pre-post? (second params+body))
        body      (if pre-post? (drop 2 params+body)
                                (drop 1 params+body))
        init      (cond-> []
                          pre-post? (conj pre-post)
                          :always (conj code))]
    (vec (reduce (fn [result form]
                   (conj result form))
                 init body))))

(defn update-body
  [[kw _old-body] new-body]
  [kw new-body])

(defn update-prepost+body
  [[kw pp-map] new-body]
  [kw (assoc pp-map :body new-body)])


(defn xform-conformed-defn-data
  ([conformed-defn-data xform-fn]
   (xform-conformed-defn-data (str *ns*) conformed-defn-data xform-fn))
  ([the-ns-name conformed-defn-data xform-fn]
   ;; single arity first then re-structure for multi
   (let [now-body      (unform-defn-params+body conformed-defn-data)
         arity-1?      (single-arity? conformed-defn-data)
         fn-name       (get-in conformed-defn-data [:defn-args :fn-name])
         new-body      (xform-fn now-body (keyword the-ns-name (name fn-name)))
         body          (get-in conformed-defn-data [:defn-args :fn-tail 1 :body])
         pre-post?     (= :prepost+body (first body))
         updated-body  (if pre-post? (update-prepost+body body new-body)
                                     (update-body body new-body))
         new-defn-data (assoc-in conformed-defn-data [:defn-args :fn-tail 1 :body] updated-body)]
     (if-not (s/invalid? (s/unform ::parser/defn new-defn-data))
       new-defn-data
       conformed-defn-data))))

(comment

  (def conformed
    (s/conform ::parser/defn '(defn pre-xy [x y]
                                {:pre [(and (int? x)
                                            (int? y))]}
                                (+ x y)
                                (+ y x))))

  (def conformed-1
    (s/conform ::parser/defn '(defn multi-form-xy [x y]
                                (+ x y)
                                (+ y x))))

  (def conformed-2
    (s/conform ::parser/defn '(defn simplest-xy [x y]
                                (+ y x))))

  (defn xform-sample-f-prn
    "Print simple parameters - deal with de-structuring next"
    [params+body fn-name]
    (->> (first params+body)
         (reduce (fn [form param]
                   (conj form (keyword param) param))
                 ['prn (keyword fn-name)])
         (seq)
         (add-code-body-start params+body)))

  (defn xform-sample-f-tap
    "Tap simple parameters - deal with de-structuring next"
    [params+body fn-name]
    (->> (first params+body)
         (conj [fn-name])
         (conj '[tap>])
         (seq)
         (add-code-body-start params+body)))

  (xform-conformed-defn-data conformed xform-sample-f-prn)
  (xform-conformed-defn-data conformed-1 xform-sample-f-prn)
  (xform-conformed-defn-data conformed-2 xform-sample-f-prn)

  (xform-conformed-defn-data conformed xform-sample-f-tap)
  (xform-conformed-defn-data conformed-1 xform-sample-f-tap)
  (xform-conformed-defn-data conformed-2 xform-sample-f-tap)

  (def conformed-multi-arity
    (s/conform ::parser/defn '(defn pre-xy
                                ([x]
                                 (pre-xy x x))
                                ([x y]
                                 (+ x y)))))

  )

