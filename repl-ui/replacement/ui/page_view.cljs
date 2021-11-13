(ns replacement.ui.page-view
  (:require ["@codemirror/closebrackets" :refer [closeBrackets]]
            ["@codemirror/fold" :as fold]
            ["@codemirror/gutter" :refer [lineNumbers]]
            ["@codemirror/highlight" :as highlight]
            ["@codemirror/history" :refer [history historyKeymap]]
            ["@codemirror/state" :refer [EditorState]]
            ["@codemirror/view" :as view :refer [EditorView]]
            ["lezer" :as lezer]
            ["lezer-generator" :as lg]
            ["lezer-tree" :as lz-tree]
            [applied-science.js-interop :as j]
            [cljs.tools.reader.edn :as edn]
            [clojure.string :as str]
            [nextjournal.clojure-mode :as cm-clj]
            [nextjournal.clojure-mode.extensions.close-brackets :as close-brackets]
            [nextjournal.clojure-mode.extensions.formatting :as format]
            [nextjournal.clojure-mode.extensions.selection-history :as sel-history]
            [nextjournal.clojure-mode.keymap :as keymap]
            [nextjournal.clojure-mode.live-grammar :as live-grammar]
            [nextjournal.clojure-mode.node :as n]
            [nextjournal.clojure-mode.selections :as sel]
            [nextjournal.clojure-mode.test-utils :as test-utils]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-com.core :refer [h-box v-box checkbox box button gap line scroller border label input-text md-circle-icon-button
                                 md-icon-button input-textarea h-split v-split popover-anchor-wrapper
                                 popover-content-wrapper title flex-child-style p slider]]
            [re-com.splits :refer [hv-split-args-desc]]
            [re-com.tabs :refer [vertical-pill-tabs]]
            [re-frame.core :as re-frame]
            [replacement.forms.events.defn :as defn-events]
            [replacement.forms.events.whole-ns :as whole-ns]
            [replacement.forms.parser.parse :as form-parser]
            [replacement.structure.wiring :as wiring]
            [replacement.ui.remote-prepl :as prepl]
            [replacement.ui.subs :as subs]
            [zprint.core :refer [zprint-file-str]]))

(def theme
  (.theme EditorView
          (j/lit {".cm-content"             {:white-space "pre-wrap"
                                             :padding     "10px 0"}
                  "&.cm-focused"            {:outline "none"}
                  ".cm-line"                {:padding     "0 9px"
                                             :line-height "1.6"
                                             :font-size   "14px"
                                             :font-family "var(--code-font)"}
                  ".cm-matchingBracket"     {:border-bottom "1px solid #ff0000"
                                             :color         "inherit"}
                  ".cm-gutters"             {:background "transparent"
                                             :border     "none"}
                  ".cm-gutterElement"       {:margin-left "5px"}
                  ;; only show cursor when focused
                  ".cm-cursor"              {:visibility "hidden"}
                  "&.cm-focused .cm-cursor" {:visibility "visible"}})))

(def ^:private extensions
  #js[theme
      (history)
      highlight/defaultHighlightStyle
      (view/drawSelection)
      (fold/foldGutter)
      (.. EditorState -allowMultipleSelections (of true))
      cm-clj/default-extensions
      (.of view/keymap cm-clj/complete-keymap)
      (.of view/keymap historyKeymap)
      (.of view/keymap
           (j/lit
             [{:key "Alt-Enter"
               :run (fn [x] (prepl/eval-cell x))}]))])

(def extensions-read-only
  #js[theme
      highlight/defaultHighlightStyle
      (view/drawSelection)
      (.. EditorState -allowMultipleSelections (of true))
      cm-clj/default-extensions
      (.. EditorView -editable (of false))])

(defn editor-view
  [component initial-document event-name index]
  (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                        :extensions extensions})
                    :parent   (rdom/dom-node component)
                    :dispatch (fn [tx]
                                (re-frame/dispatch [event-name tx index]))}))

(defn part-edit
  [part-cm-name tx]
  ;(prn :part-edit :part-cm-name part-cm-name :tx tx)
  (re-frame/dispatch [::defn-events/part-edit part-cm-name tx]))

(defn comp-editor-view
  [dom-element initial-document part-cm-name]
  (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                        :extensions extensions})
                    :parent   (rdom/dom-node dom-element)
                    :dispatch (partial part-edit part-cm-name)}))

(defn comp-editor
  "Produces a function to act on a code mirror view with the given cm-name
  using an optional initial document. An empty string is used if no
  document is provided."
  ([cm-name]
   (comp-editor cm-name ""))
  ([cm-name initial-document]
   (fn [dom-element]
     (let [!view (comp-editor-view dom-element initial-document cm-name)]
       (re-frame/dispatch-sync [::defn-events/set-cm+name !view cm-name])))))

(defn part-editor
  [cm-name]
  ;(prn :part-editor :cm-name cm-name)
  [:div {:ref (comp-editor cm-name)}])

(def doc-options
  ['(defn ranker
      "improve ranking on the celestial index"
      {:since "0.0.1"}
      [x]
      {:pre [(pos-int? x)]}
      (inc x))

   '(defn ranker-arity-n
      "improve ranking on the celestial index"
      {:since "0.0.1"}
      ([x]
       {:pre [(pos-int? x)]}
       (ranker-arity-n x (inc x)))
      ([x y]
       {:pre [(pos-int? x)]}
       [(inc x) (inc y)]))])

(defn editable-fn-form []
  (let [!mount (fn [comp]
                 (let [cm-name (wiring/comp-name->cm-name :defn.form)
                       !view   (EditorView. #js {:state    (.create EditorState #js {:doc        ""
                                                                                     :extensions extensions})
                                                 :parent   (rdom/dom-node comp)
                                                 :dispatch (fn [tx]
                                                             (re-frame/dispatch [::defn-events/fn-whole-form-tx cm-name tx]))})]
                   (re-frame/dispatch-sync [::defn-events/set-cm+name !view cm-name])))]
    [:div {:ref !mount}]))

(defn result-view [{:keys [val]}]
  (let [!mount (fn [comp]
                 (EditorView. #js {:state  (.create EditorState #js {:doc        (str "=> " val)
                                                                     :extensions extensions-read-only})
                                   :parent (rdom/dom-node comp)}))]
    [:div {:ref !mount}]))

(defn result-box []
  [v-box :children
   [[title :level :level2 :label "REPL Output"]
    (let [results (re-frame/subscribe [::subs/latest-result])]
      (fn []
        (prn :result-box (:val @results))
        (when @results [result-view @results])))]])

(defn form-box []
  (let [form (re-frame/subscribe [::subs/the-defn-form])]
    [v-box :children
     [[title :level :level2 :label @form]
      [editable-fn-form]]]))

(defn prepend-index
  [arity-index n-arities label]
  (if (>= 1 n-arities)
    label
    (str arity-index "-" label)))

(defn component-part
  ([part-name label-text]
   (component-part part-name label-text 0 0))
  ([part-name label-text arity-index n-arities]
   [h-box :gap "5px" :align :center
    :children
    [[label :width "110px" :label (prepend-index arity-index n-arities label-text)]
     (if (zero? n-arities)
       [part-editor (wiring/comp-name->cm-name part-name)]
       [part-editor (wiring/indexed-comp-name->cm-name arity-index part-name)])]]))

(defn defn-arity-parts
  [arity-index n-arities]
  [v-box :gap "5px"
   :children
   [[line :color "#D8D8D8"]
    [component-part :defn.params "Parameters" arity-index n-arities]
    [line :color "#D8D8D8"]
    [component-part :defn.prepost "Pre/Post" arity-index n-arities]
    [line :color "#D8D8D8"]
    [component-part :defn.body "Body" arity-index n-arities]]])

(defn defn-parts
  []
  (let [arity-data (re-frame/subscribe [::subs/fn-arity-data])]
    [v-box
     :gap "5px"
     :children
     (into [[title :level :level2 :label "Function Parts"]
            [component-part :defn.name "Name"]
            [line :color "#D8D8D8"]
            [component-part :defn.docstring "Docstring"]
            [line :color "#D8D8D8"]
            [component-part :defn.meta "Attributes"]]
           (let [n-arities (count @arity-data)]
             (map (fn [arity-index]
                    (defn-arity-parts arity-index n-arities))
                  (range n-arities))))]))

(defn linux? []
  (some? (re-find #"(Linux)|(X11)" js/navigator.userAgent)))

(defn mac? []
  (and (not (linux?))
       (some? (re-find #"(Mac)|(iPhone)|(iPad)|(iPod)" js/navigator.platform))))

(defn key-mapping []
  (cond-> {"ArrowUp"    "↑"
           "ArrowDown"  "↓"
           "ArrowRight" "→"
           "ArrowLeft"  "←"
           "Mod"        "Ctrl"}
          (mac?)
          (merge {"Alt"   "⌥"
                  "Shift" "⇧"
                  "Enter" "⏎"
                  "Ctrl"  "⌃"
                  "Mod"   "⌘"})))

(defn defn-form []
  [v-box :gap "5px" :width "500px"
   :children
   [[form-box]]])

(defn type-label
  [ref-type]
  (condp = ref-type
    :def "\uD83C\uDC6B"
    :defn "λ"
    "⸮"))

(defn var-list
  [var-data default-selection]
  (let [ns-vars      (mapv (fn [[id data]]
                             {:id    id
                              :type  (:type data)
                              :label [h-box :align :center :children
                                      [[label :width "15px" :label (type-label (:type data))]
                                       [label :label (:name data)]]]})
                           var-data)
        type-mapping (apply merge (map #(hash-map (:id %) (:type %)) ns-vars))
        selected-var (r/atom default-selection)]
    [v-box :gap "5px"
     :children
     [[vertical-pill-tabs
       :model selected-var
       :tabs ns-vars
       :on-change (fn [var-id]
                    (let [var-type (type-mapping var-id)]
                      (if (= :defn var-type)
                        (re-frame/dispatch [::defn-events/set-fn-view var-id])))
                    (reset! selected-var var-id))]]]))

(defn format-ns
  [the-ns-name]
  (let [[_ first-part last-part] (re-find #"(.*)\.(.*)" (str the-ns-name))]
    [h-box :align :center :children
     [[label :style {:color "grey" :font-size :smaller} :label (str first-part ".")]
      [label :style {:color "blue" :font-weight :bolder} :label last-part]]]))

(defn ns-view [the-ns-name]
  (let [ns-data (re-frame/subscribe [::subs/id-index the-ns-name])
        default-selection        (first (last @ns-data))]
    (re-frame/dispatch [::defn-events/set-fn-view default-selection])
    (when (seq @ns-data)
      [v-box :gap "5px"
       :children
       [[title :level :level2 :label (format-ns the-ns-name)]
        (var-list @ns-data default-selection)]])))

(defn transformers []
  (let [prn-ticked? (r/atom false)
        tap-ticked? (r/atom false)]
    ;; read this from config
    [h-box :align :center :gap "15px"
     :children
     [[checkbox
       :label "Print params"
       :model prn-ticked?
       :on-change (fn [setting]
                    (reset! prn-ticked? setting)
                    (re-frame/dispatch [::whole-ns/defn-transforms-toggle :defn.params.prn setting]))]
      [checkbox
       :label "Tap params"
       :model tap-ticked?
       :on-change (fn [setting]
                    (reset! tap-ticked? setting)
                    (re-frame/dispatch [::whole-ns/defn-transforms-toggle :defn.params.tap setting]))]]]))

(defn transform-options []
  [h-box :align :center :gap "20px"
   :children
   [[title :level :level2 :label "Transformations"]
    [gap :size "10px"]
    [transformers]]])

(defn set-ns-data
  []
  (let [forms-read   (form-parser/read-whole-ns form-parser/sample)
        un+conformed (form-parser/whole-ns->forms forms-read)
        enriched     (form-parser/add-reference-data un+conformed)]
    (re-frame/dispatch [::whole-ns/ns-forms enriched])))

(defn defn-view []
  (let [the-ns-name (re-frame/subscribe [::subs/the-ns-name])]
    [v-box :gap "20px" :justify :center :padding "15px"
     :children
     [[transform-options]
      [line :color "#D8D8D8"]
      [h-box :align :start :gap "75px" :padding "5px"
       :children
       [[ns-view @the-ns-name]
        [defn-form]
        [line :color "#D8D8D8"]
        [defn-parts]]]]]))

(defn default-ns-data
  []
  (->> form-parser/sample
       (form-parser/read-whole-ns)
       (form-parser/whole-ns->forms)
       (form-parser/add-reference-data)
       (conj [::whole-ns/ns-forms])
       (re-frame/dispatch)))

(defn render []
  (default-ns-data)                                         ;; TODO obtain from project classpath viewer

  (rdom/render [defn-view] (js/document.getElementById "form-editor"))

  (let [mapping (key-mapping)]
    (.. (js/document.querySelectorAll ".mod,.alt,.ctrl")
        (forEach #(when-let [k (get mapping (.-innerHTML %))]
                    (set! (.-innerHTML %) k)))))

  (when (linux?)
    (js/twemoji.parse (.-body js/document))))
