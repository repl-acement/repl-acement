(ns replacement.xform.aspec
  (:require [replacement.xform.defn :refer [add-code-body-start]]))

(defn xform-prn
  "Print simple parameters - deal with de-structuring next"
  [params+body fn-name xform-meta]
  (let [code (->> (first params+body)
                  (reduce (fn [form param]
                            (conj form (keyword param) param))
                          ['prn (keyword fn-name)])
                  (seq))]
    (add-code-body-start params+body (with-meta code xform-meta))))

(defn xform-prn-undo
  "Remove prn transform"
  [params+body xform-meta]
  ;; look up meta and remove the form
  (prn :xform-sample-f-prn-rm :params+body params+body :xform-meta xform-meta))

(defn xform-tap
  "Tap simple parameters - deal with de-structuring next"
  [params+body fn-name xform-meta]
  (let [code (->> (first params+body)
                  (conj [(keyword fn-name)])
                  (conj '[tap>])
                  (seq))]
    (add-code-body-start params+body (with-meta code xform-meta))))

(defn xform-tap-undo
  "Remove tap xform"
  [params+body fn-name]
  ;; look up meta and remove the form
  (->> (first params+body)
       (conj ['tap> (keyword fn-name)])
       (seq)
       (add-code-body-start params+body)))

(def sample-xform-declaration
  {:defn.params.prn {:enabled? false
                     :meta     {:prn-xform :v0-0-1}
                     :ui       {:label   "Print parameters"
                                :control :checkbox}
                     :joins    [{:ns-regex   #"repl.*"
                                 :form-types #{:defn :defn-}}]
                     :actions  {:apply [xform-prn]
                                :undo  [xform-prn-undo]}}

   :defn.params.tap {:enabled? false
                     :meta     {:prn-tap :v0-0-1}
                     :ui       {:label   "Tap parameters"
                                :control :checkbox}
                     :joins    [{:ns-regex   #"repl.*"
                                 :form-types #{:defn :defn-}}]
                     :actions  {:apply [xform-tap]
                                :undo  [xform-tap-undo]}}})



