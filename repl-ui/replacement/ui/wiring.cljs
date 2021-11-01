(ns replacement.ui.wiring
  "Provide functions to wire up events and components"
  (:require [clojure.string :as string]))

(defn comp-name->cm-name
  "Return a derived CodeMirror name from a given component name keyword.
  Assuming a component name of `:fn-name`, will return `:fn-name.cm`"
  [comp-name]
  {:pre  [(keyword? comp-name)]
   :post [(keyword? %)]}
  (-> (name comp-name) (string/replace #"$" ".cm") keyword))

(defn indexed-comp-name->cm-name
  "Return a derived CodeMirror name from a given component name keyword and a zero-based index.
  Assuming a component name of `:fn-name` and an index of 0, will return `:fn-name.0.cm`"
  [index comp-name]
  {:pre  [(keyword? comp-name) (nat-int? index)]
   :post [(keyword? %)]}
  (-> (name comp-name) (string/replace #"$" (str "." index ".cm")) keyword))

(defn- indexed-cm-name->comp-name
  "Return the component name from an indexed, derived CodeMirror keyword.
  Assuming a CodeMirror name of `:fn-name.42.cm`, will return `:fn-name`"
  [cm-name]
  {:pre  [(and (keyword? cm-name)
               (-> (name cm-name) (string/ends-with? ".cm")))]
   :post [(keyword? %)]}
  (-> (name cm-name) (string/replace #".\d+\.cm$" "") keyword))

(defn cm-name->comp-name
  "Return the component name from a derived CodeMirror keyword.
  Assuming a CodeMirror name of `:fn-name.cm`, will return `:fn-name`"
  [cm-name]
  {:pre  [(and (keyword? cm-name)
               (-> (name cm-name) (string/ends-with? ".cm")))]
   :post [(keyword? %)]}
  (or (indexed-cm-name->comp-name cm-name)
      (-> (name cm-name) (string/replace #"\.cm$" "") keyword)))
