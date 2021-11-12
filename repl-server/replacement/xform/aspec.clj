(ns replacement.xform.aspec
  (:require [replacement.xform.defn :refer [add-code-body-start]]))

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
       (conj ['tap> (keyword fn-name)])
       (seq)
       (add-code-body-start params+body)))

(def sample-xform-declaration
  {:ui      {:label   "Print parameters"
             :control :radio-button}
   :joins   [{:ns-regex   #"repl.*"
              :form-types #{:defn :defn-}}]
   :actions [xform-sample-f]})



