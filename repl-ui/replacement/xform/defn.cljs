(ns replacement.xform.defn
  (:require [cljs.spec.alpha :as s]
            [replacement.structure.core-fn-specs :as core-specs]
            [replacement.structure.form-specs :as form-specs]))

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
        body      (if pre-post? (drop 2 params+body)
                                (drop 1 params+body))]
    (vec (reduce (fn [result form]
                   (conj result form))
                 [code] body))))

(defn update-body
  [[kw _old-body] new-body]
  [kw new-body])

(defn update-prepost+body
  [[kw pp-map] new-body]
  [kw (assoc pp-map :body new-body)])


(defn xform-conformed-defn-data
  [conformed-defn-data xform-fn metadata]
  ;; single arity first then re-structure for multi
  (let [now-body      (unform-defn-params+body conformed-defn-data)
        arity-1?      (single-arity? conformed-defn-data)
        fn-name       (get-in conformed-defn-data [:defn-args :fn-name])
        new-body      (xform-fn now-body fn-name metadata)
        body          (get-in conformed-defn-data [:defn-args :fn-tail 1 :body])
        pre-post?     (= :prepost+body (first body))
        updated-body  (if pre-post? (update-prepost+body body new-body)
                                    (update-body body new-body))
        new-defn-data (assoc-in conformed-defn-data [:defn-args :fn-tail 1 :body] updated-body)]
    (if-not (s/invalid? (s/unform ::form-specs/defn new-defn-data))
      new-defn-data
      conformed-defn-data)))

