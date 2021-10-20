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
                                             :font-size   "16px"
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
                                (rf/dispatch-sync [event-name tx]))}))

(defn editor []
  (let [!mount (fn [comp]
                 (let [doc   (str '(+ 1 2))
                       !view (editor-view comp doc ::events/code-mirror-tx)]
                   (rf/dispatch [::events/set-code-mirror-view !view])))]
    [:div
     [:div {:ref !mount}]]))

(defn form-fn-name []
  (let [!mount (fn [comp]
                 (let [doc   (name (gensym "my-fn-"))
                       !view (editor-view comp doc ::events/fn-name-tx)]
                   (rf/dispatch [::events/set-fn-name-cm !view])))]
    [:div
     [:div {:ref !mount}]]))

(defn form-fn-doc []
  (let [!mount (fn [comp]
                 (let [doc   "flips the sprocket on the gear flange"
                       !view (editor-view comp doc ::events/fn-doc-tx)]
                   (rf/dispatch [::events/set-fn-doc-cm !view])))]
    [:div
     [:div {:ref !mount}]]))

(defn form-fn-attrs []
  (let [!mount (fn [comp]
                 (let [doc   "{:since \"0.0.1\"}"
                       !view (editor-view comp doc ::events/fn-attrs-tx)]
                   (rf/dispatch [::events/set-fn-attrs-cm !view])))]
    [:div
     [:div {:ref !mount}]]))

(defn form-fn-args []
  (let [!mount (fn [comp]
                 (let [doc   "[x]"
                       !view (editor-view comp doc ::events/fn-args-tx)]
                   (rf/dispatch [::events/set-fn-args-cm !view])))]
    [:div
     [:div {:ref !mount}]]))

(defn form-fn-pp []
  (let [!mount (fn [comp]
                 (let [doc   "{:pre [(int? x)]}"
                       !view (editor-view comp doc ::events/fn-pp-tx)]
                   (rf/dispatch [::events/set-fn-pp-cm !view])))]
    [:div
     [:div {:ref !mount}]]))

(defn form-fn-body []
  (let [!mount (fn [comp]
                 (let [doc   "(inc x)"
                       !view (editor-view comp doc ::events/fn-body-tx)]
                   (rf/dispatch [::events/set-fn-body-cm !view])))]
    [:div
     [:div {:ref !mount}]]))

(defn result-view [{:keys [val input]}]
  (let [!mount (fn [comp]
                 (EditorView. #js {:state  (.create EditorState #js {:doc        (str input " => " val)
                                                                     :extensions extensions-read-only})
                                   :parent (rdom/dom-node comp)}))]
    [:div
     [:div {:ref !mount}]]))

(defn result-box []
  (let [results (rf/subscribe [::subs/latest-result])]
    (fn []
      (when @results [result-view @results]))))

(defn editable-form-view [some-text]
  (let [!mount (fn [comp]
                 (EditorView. #js {:state  (.create EditorState #js {:doc        some-text
                                                                     :extensions extensions-read-only})
                                   :parent (rdom/dom-node comp)}))]
    [:div {:ref !mount}]))

(defn editable-form-box []
  (let [body (rf/subscribe [::subs/fn-body])]
    (fn []
      (when @body [editable-form-view @body]))))

(defn editable
  []
  [:div {:class "wrap"}
   [:main                                                   ;; FIX ... multi-arity
    [:input-view {:class "bg-grey"}
     [:h3 "Editable Form"]
     [:div {:class "code-wrapper"}
      [:div {:class "code-box"}
       [editable-form-box]]]]]])

(defn page []
  [:div {:class "wrap"}
   [:main
    [:table.w-full.md:max-w-sm.text-sm
     [:tbody
      [:tr.align-center
       [:td.py-1 "Function"]
       [:td.py-1.pr-12 [:div {:class "code-wrapper"}
                        [:div {:class "code-box"}
                         [form-fn-name]]]]]
      [:tr.border-t [:td]]
      [:tr.align-center
       [:td.py-1 "Doc string"]
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
      [:tr.align-center
       [:td.py-1 "Arguments"]
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
                         [form-fn-body]]]]]]]

    ;; FIX ... multi-arity
    [:input-view {:class "bg-grey"}
     [:h3 "Editor"]
     [:div {:class "code-wrapper"}
      [:div {:class "code-box"}
       [editor]]]]
    [:result-view
     [:h3 "REPL Output"]
     [:div {:class "code-wrapper"}
      [:div {:class "code-box"}
       [result-box]]]]]])

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

(defn render-key [key]
  (let [keys (into [] (map #(get ((memoize key-mapping)) % %) (str/split key #"-")))]
    (into [:span]
          (map-indexed (fn [i k]
                         [:<>
                          (when-not (zero? i) [:span " + "])
                          [:kbd.kbd k]]) keys))))

(defn key-bindings-table []
  [:table.w-full.text-sm
   [:thead
    [:tr.border-t
     [:th.px-3.py-1.align-top.text-left.text-xs.uppercase.font-normal.black-50 "Command"]
     [:th.px-3.py-1.align-top.text-left.text-xs.uppercase.font-normal.black-50 "Keybinding"]
     [:th.px-3.py-1.align-top.text-left.text-xs.uppercase.font-normal.black-50 "Alternate Binding"]
     [:th.px-3.py-1.align-top.text-left.text-xs.uppercase.font-normal.black-50 {:style {:min-width 290}} "Description"]]]
   (into [:tbody]
         (->> keymap/paredit-keymap*
              (merge (prepl/keymap* "Alt"))
              (sort-by first)
              (map (fn [[command [{:keys [key shift doc]} & [{alternate-key :key}]]]]
                     [:<>
                      [:tr.border-t.hover:bg-gray-100
                       [:td.px-3.py-1.align-top.monospace.whitespace-nowrap [:b (name command)]]
                       [:td.px-3.py-1.align-top.text-right.text-sm.whitespace-nowrap (render-key key)]
                       [:td.px-3.py-1.align-top.text-right.text-sm.whitespace-nowrap (some-> alternate-key render-key)]
                       [:td.px-3.py-1.align-top doc]]
                      (when shift
                        [:tr.border-t.hover:bg-gray-100
                         [:td.px-3.py-1.align-top [:b (name shift)]]
                         [:td.px-3.py-1.align-top.text-sm.whitespace-nowrap.text-right
                          (render-key (str "Shift-" key))]
                         [:td.px-3.py-1.align-top.text-sm]
                         [:td.px-3.py-1.align-top]])]))))])

(defn ^:dev/after-load render []
  (rdom/render [page] (js/document.getElementById "editor"))

  (rdom/render [editable] (js/document.getElementById "editable"))

  (.. (js/document.querySelectorAll "[clojure-mode]")
      (forEach #(when-not (.-firstElementChild %)
                  (rdom/render [editor (str/trim (.-innerHTML %))] %))))

  (let [mapping (key-mapping)]
    (.. (js/document.querySelectorAll ".mod,.alt,.ctrl")
        (forEach #(when-let [k (get mapping (.-innerHTML %))]
                    (set! (.-innerHTML %) k)))))

  (when (linux?)
    (js/twemoji.parse (.-body js/document))))
