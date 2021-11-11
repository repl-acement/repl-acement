(ns replacement.xform.defn
  (:require [cljs.spec.alpha :as s]
            [replacement.structure.form-specs :as form-specs]))

(defn- )

(defn- update-defn-body
  [conformed-data]
  (let [[arity arity-map] (-> (s/conform ::form-specs/defn conformed-data)
                              (get-in [:defn-args :fn-tail]))]
    (if (= :arity-1 arity)
      (let [local-symbols (map (fn [param]
                                 (if (= :local-symbol (first param))
                                   (last param)))
                               (:args (:params (update-defn-body arity-map))))]
        (prn :trace local-symbols)))))

(defn apply-to-body-entry
  [conformed-data f]
  ;; single arity first

  )

