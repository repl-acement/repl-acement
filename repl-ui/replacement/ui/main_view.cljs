(ns replacement.ui.main-view
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
            [re-frame.core :as rf]
            [replacement.ui.events :as events]
            [replacement.ui.remote-prepl :as prepl]
            [replacement.ui.subs :as subs]
            [replacement.ui.wiring :as wiring]
            [cljs.tools.reader.edn :as edn]
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

(def ^:private extensions #js[theme
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

(def extensions-read-only #js[theme
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
                                (rf/dispatch [event-name tx index]))}))

(defn fn-editor
  ([part]
   (fn-editor part 0))
  ([part index]
   (fn-editor part index ""))
  ([part index doc]
   (let [event-ns "replacement.ui.events"
         tx-event (keyword event-ns (str "fn-" (name part) "-tx"))
         cm-event (keyword event-ns (str "set-fn-" (name part) "-cm"))]
     (fn [comp]
       (let [!view (editor-view comp doc tx-event index)]
         (rf/dispatch-sync [cm-event !view index]))))))

(defn part-edit
  [part-cm-name tx]
  (rf/dispatch [::events/part-edit part-cm-name tx]))

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
       (rf/dispatch-sync [::events/set-cm+name !view cm-name])))))

(defn part-editor
  [cm-name]
  [:div {:ref (comp-editor cm-name)}])

(comment
  '(defn ranker-arity-1
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
      (ranker x (inc x)))
     ([x y]
      {:pre [(pos-int? x)]}
      [(inc x) (inc y)]))
  )

(defn editable-fn-form []
  (let [!mount (fn [comp]
                 (let [doc       (str '(defn ranker-arity-n
                                         "improve ranking on the celestial index"
                                         {:since "0.0.1"}
                                         ([x]
                                          {:pre [(pos-int? x)]}
                                          (ranker x (inc x)))
                                         ([x y]
                                          {:pre [(pos-int? x)]}
                                          [(inc x) (inc y)])))
                       formatted (zprint-file-str doc "::fn-whole-update")
                       cm-name   (wiring/comp-name->cm-name :defn.form)
                       !view     (comp-editor-view comp formatted cm-name)]
                   (rf/dispatch [::events/set-cm+name !view cm-name])
                   (rf/dispatch [::events/transact-whole-defn-form formatted])))]
    [:div {:ref !mount}]))

(defn result-view [{:keys [val]}]
  (let [!mount (fn [comp]
                 (EditorView. #js {:state  (.create EditorState #js {:doc        (str "=> " val)
                                                                     :extensions extensions-read-only})
                                   :parent (rdom/dom-node comp)}))]
    [:div {:ref !mount}]))

(defn result-box []
  (let [results (rf/subscribe [::subs/latest-result])]
    (fn []
      (when @results [result-view @results]))))

(defn defn-form []
  [:div {:class "wrap"}
   [:main
    [:input-view {:class "bg-grey"}
     [:h3 "Function"]
     [:div {:class "code-wrapper"}
      [:div {:class "code-box"}
       [editable-fn-form]]]]
    [:result-view
     [:h3 "REPL Output"]
     [:div {:class "code-wrapper"}
      [:div {:class "code-box"}
       [result-box]]]]]])

(defn prepend-index
  [arity-index n-arities label]
  (if (zero? n-arities)
    label
    (str "[" arity-index "] " label)))

(defn component-part
  ([part-name label]
   (component-part part-name label 0 0))
  ([part-name label arity-index n-arities]
   [:tr.align-center
    [:td.py-1 (prepend-index arity-index n-arities label)]
    [:td.py-1.pr-12
     [:div {:class "code-wrapper"}
      [:div {:class "code-box"}
       (if (zero? n-arities)
         [part-editor (replacement.ui.wiring/comp-name->cm-name part-name)]
         [part-editor (replacement.ui.wiring/indexed-comp-name->cm-name arity-index part-name)])]]]]))

(defn defn-arity-parts
  [arity-index n-arities]
  [:table.w-full.md:max-w-sm.text-sm
   [:tbody
    [:tr.border-t [:td]]
    [component-part :defn.params "Parameters" arity-index n-arities]
    [:tr.border-t [:td]]
    [component-part :defn.prepost "Pre/Post" arity-index n-arities]
    [:tr.border-t [:td]]
    [component-part :defn.body "Body" arity-index n-arities]]])

(defn defn-parts
  []
  (let [arity-n-data (rf/subscribe [::subs/fn-arity-n-data])]
    [:div {:class "wrap"}
     (into [:main
            [:table.w-full.md:max-w-sm.text-sm
             [:tbody
              [component-part :defn.name "Name"]
              [:tr.border-t [:td]]
              [component-part :defn.docstring "Docstring"]
              [:tr.border-t [:td]]
              [component-part :defn.meta "Attributes"]]]]
           (let [defn-arities (or @arity-n-data [:arity-1])
                 n-arities    (count defn-arities)]
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

(defn render []
  (rdom/render [defn-form] (js/document.getElementById "form-editor"))

  (rdom/render [defn-parts] (js/document.getElementById "parts-editor"))

  (let [mapping (key-mapping)]
    (.. (js/document.querySelectorAll ".mod,.alt,.ctrl")
        (forEach #(when-let [k (get mapping (.-innerHTML %))]
                    (set! (.-innerHTML %) k)))))

  (when (linux?)
    (js/twemoji.parse (.-body js/document))))
