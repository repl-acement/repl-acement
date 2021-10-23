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
            [cljs.tools.reader.edn :as edn]))

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
  [component initial-document event-name]
  (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                        :extensions extensions})
                    :parent   (rdom/dom-node component)
                    :dispatch (fn [tx]
                                (rf/dispatch [event-name tx]))}))

(defn fn-editor
  ([part]
   (fn-editor part ""))
  ([part doc]
   (let [event-ns "replacement.ui.events"
         tx-event (keyword event-ns (str "fn-" (name part) "-tx"))
         cm-event (keyword event-ns (str "set-fn-" (name part) "-cm"))]
     (fn [comp]
       (let [!view (editor-view comp doc tx-event)]
         (rf/dispatch [cm-event !view]))))))

(defn form-fn-name []
  (let [!mount (fn-editor :name)]
    [:div {:ref !mount}]))

(defn form-fn-doc []
  (let [!mount (fn-editor :doc)]
    [:div {:ref !mount}]))

(defn form-fn-attrs []
  (let [!mount (fn-editor :attrs)]
    [:div {:ref !mount}]))

(defn form-fn-args []
  (let [!mount (fn-editor :args)]
    [:div {:ref !mount}]))

(defn form-fn-pp []
  (let [!mount (fn-editor :pp)]
    [:div {:ref !mount}]))

(defn form-fn-body []
  (let [!mount (fn-editor :body)]
    [:div {:ref !mount}]))

(defn editable-fn-form []
  (let [!mount (fn [comp]
                 (let [doc   (str "(defn " (name (gensym "my-fn-"))
                                  " \"improve ranking on the celestial index\" "
                                  {:since "0.0.1"}
                                  "[x]"
                                  {:pre ['(nat-int? x)]}
                                  '(inc x) ")")
                       !view (editor-view comp doc ::events/fn-whole-form-tx)]
                   (rf/dispatch [::events/set-fn-whole-form-cm !view])
                   (rf/dispatch [::events/set-whole-form doc])))]
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

(defn defn-parts []
  [:div {:class "wrap"}
   [:main
    [:table.w-full.md:max-w-sm.text-sm
     [:tbody
      [:tr.align-center
       [:td.py-1 "Name"]
       [:td.py-1.pr-12 [:div {:class "code-wrapper"}
                        [:div {:class "code-box"}
                         [form-fn-name]]]]]
      [:tr.border-t [:td]]
      [:tr.align-center
       [:td.py-1 "Docs"]
       [:td.py-1.pr-12 [:div {:class "code-wrapper"}
                        [:div {:class "code-box"}
                         [form-fn-doc]]]]]
      [:tr.border-t [:td]]
      [:tr.align-center
       [:td.py-1 "Attributes"]
       [:td.py-1.pr-12 [:div {:class "code-wrapper"}
                        [:div {:class "code-box"}
                         [form-fn-attrs]]]]]
      [:tr.border-t [:td]]
      ;; FIX ... multi-arity
      [:tr.align-center
       [:td.py-1 "Parameters"]
       [:td.py-1.pr-12 [:div {:class "code-wrapper"}
                        [:div {:class "code-box"}
                         [form-fn-args]]]]]
      [:tr.border-t [:td]]
      [:tr.align-center
       [:td.py-1 "Pre/Post"]
       [:td.py-1.pr-12 [:div {:class "code-wrapper"}
                        [:div {:class "code-box"}
                         [form-fn-pp]]]]]
      [:tr.border-t [:td]]
      [:tr.align-center
       [:td.py-1 "Body"]
       [:td.py-1.pr-12 [:div {:class "code-wrapper"}
                        [:div {:class "code-box"}
                         [form-fn-body]]]]]]]]])

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
