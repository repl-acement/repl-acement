(ns replacement.xform.defn
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as core-specs]
            [replacement.server.parser :as parser]))

(defn- arity-1-params
  [conformed-data]
  (let [[arity arity-map] (get-in conformed-data [:defn-args :fn-tail])]
    (if (= :arity-1 arity)
      (:params arity-map))))

(defn unform-defn-params+body
  [conformed-data]
  (let [[arity arity-map] (get-in conformed-data [:defn-args :fn-tail])]
    (if (= :arity-1 arity)
      (s/unform ::core-specs/params+body arity-map))))

(defn xform-conformed-defn-data
  [conformed-defn-data xform-fn]
  ;; single arity first
  (let [now-body (unform-defn-params+body conformed-defn-data)
        new-body (xform-fn now-body)
        body (get-in conformed-defn-data [:defn-args :fn-tail 1 :body])
        pre-post? (= :prepost+body (first body))
        ]
    ;; pop the new body back in...
    (prn :conformed-defn-data conformed-defn-data)
    (prn :now (vec (rest now-body)))
    (prn :new (vec new-body)))
  )

(comment

  (def conformed
    (s/conform ::parser/defn '(defn xy [x y]
                                {:pre [(and (int? x)
                                            (int? y))]}
                                (+ x y)
                                (+ y x))))

  (def conformed-1
    (s/conform ::parser/defn '(defn xy [x y]
                                (+ x y)
                                (+ y x))))

  (def conformed-2
    (s/conform ::parser/defn '(defn xy [x y]
                                (+ y x))))

  (defn xform-sample-f
    [params+body]
    (let [params    (first params+body)
          pre-post? (map? (second params+body))
          pre-post  (and pre-post? (second params+body))
          body      (if pre-post? (drop 2 params+body)
                                  (drop 1 params+body))
          code      (seq (reduce (fn [form param]
                                   (conj form (keyword param) param))
                                 ['prn] params))
          init      (cond-> []
                            pre-post? (conj pre-post)
                            :always (conj code))]
      (seq (reduce (fn [result form]
                     (conj result form))
                   init body))))

  (xform-conformed-defn-data conformed xform-sample-f)

  (xform-conformed-defn-data conformed-1 xform-sample-f)

  (xform-conformed-defn-data conformed-2 xform-sample-f)

  )

