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
            [replacement.forms.events.def :as def-events]
            [replacement.forms.events.defn :as defn-events]
            [replacement.forms.events.ns :as ns-events]
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
  (re-frame/dispatch [::defn-events/part-edit part-cm-name tx])
  (re-frame/dispatch [::def-events/part-edit part-cm-name tx]))

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

(defn comp-def-editor
  "Produces a function to act on a code mirror view with the given cm-name
  using an optional initial document. An empty string is used if no
  document is provided."
  ([cm-name]
   (comp-editor cm-name ""))
  ([cm-name initial-document]
   (fn [dom-element]
     (let [!view (comp-editor-view dom-element initial-document cm-name)]
       (re-frame/dispatch-sync [::def-events/set-cm+name !view cm-name])))))

(defn part-editor
  ([cm-name]
   (part-editor cm-name :defn))
  ([cm-name part-type]
   (part-editor cm-name part-type ""))
  ([cm-name part-type document]
   [:div {:ref (if (= :def part-type)
                 (comp-def-editor cm-name document)
                 (comp-editor cm-name document))}]))

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

(defn editable-var-form
  [form-data]
  (let [initial-document (-> (get-in form-data [:form :def.text])
                             (zprint-file-str ::editable-var-form))
        !mount           (fn [comp]
                           (let [cm-name (wiring/comp-name->cm-name :def.form)
                                 !view   (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                                                               :extensions extensions})
                                                           :parent   (rdom/dom-node comp)
                                                           :dispatch (fn [tx]
                                                                       (re-frame/dispatch [::def-events/def-whole-form-tx cm-name tx]))})]
                             (re-frame/dispatch-sync [::def-events/set-cm+name !view cm-name])))]
    [:div {:ref !mount}]))

(defn editable-fn-form
  [form-data]
  (let [initial-document (-> (get-in form-data [:form :defn.text])
                             (zprint-file-str ::editable-fn-form))
        !mount           (fn [comp]
                           (let [cm-name (wiring/comp-name->cm-name :defn.form)
                                 !view   (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                                                               :extensions extensions})
                                                           :parent   (rdom/dom-node comp)
                                                           :dispatch (fn [tx]
                                                                       (re-frame/dispatch [::defn-events/fn-whole-form-tx cm-name tx]))})]
                             (re-frame/dispatch-sync [::defn-events/set-cm+name !view cm-name])))]
    [:div {:ref !mount}]))

(defn editable-ns-form
  [form-data]
  (let [initial-document
               (-> (get-in form-data [:form :ns.text])
                   (zprint-file-str ::editable-fn-form))
        !mount (fn [comp]
                 (let [cm-name (wiring/comp-name->cm-name :ns.form)
                       !view   (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                                                     :extensions extensions})
                                                 :parent   (rdom/dom-node comp)
                                                 :dispatch (fn [tx]
                                                             (re-frame/dispatch [::ns-events/whole-form-tx cm-name tx]))})]
                   (re-frame/dispatch-sync [::ns-events/set-cm+name !view cm-name])))]
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

(def editable-forms (atom {}))

(defn form-view
  []
  (let [form-data (re-frame/subscribe [::subs/current-form-data])]
    (when @form-data
      [v-box :gap "5px" :width "500px"
       :children
       [[title :level :level2 :label (:name @form-data)]
        (condp = (:type @form-data)
          :def (or (:var-form @editable-forms)
                   (:var-form (swap! editable-forms assoc :var-form [editable-var-form @form-data])))
          :defn (or (:defn-form @editable-forms)
                    (:defn-form (swap! editable-forms assoc :defn-form [editable-fn-form @form-data])))
          :ns (or (:ns-form @editable-forms)
                  (:ns-form (swap! editable-forms assoc :ns-form [editable-ns-form @form-data])))
          ;; TODO - better default logic
          (or (:defn-form @editable-forms)
              (:defn-form (swap! editable-forms assoc :defn-form [editable-fn-form @form-data]))))]])))

(defn prepend-index
  [arity-index n-arities label]
  (if (>= 1 n-arities)
    label
    (str arity-index "-" label)))

(defn pretty-label
  [label-keyword]
  (-> (name label-keyword)
      (str/replace #".*\." "")
      (str/capitalize)))

(defn component-part
  ([form-type part-name label-text]
   (component-part form-type part-name label-text ""))
  ([form-type part-name label-text document]
   (component-part form-type part-name label-text document 0 0))
  ([form-type part-name label-text document arity-index n-arities]
   [h-box :gap "5px" :align :center
    :children
    [[label :width "110px" :label (prepend-index arity-index n-arities label-text)]
     (if (zero? n-arities)
       [part-editor (wiring/comp-name->cm-name part-name) form-type]
       [part-editor (wiring/indexed-comp-name->cm-name arity-index part-name) form-type])]]))

(defn defn-arity-parts
  [arity-index n-arities]
  [v-box :gap "5px" :width "500px"
   :children
   (mapcat (fn [part-name]
             [[line :color "#D8D8D8"]
              [component-part :defn part-name (pretty-label part-name) "" arity-index n-arities]])
           defn-events/arity-parts)])

(defn defn-parts
  [form-data arity-data]
  [v-box :gap "5px" :width "500px"
   :children
   (vec (concat (into [[title :level :level2 :label "Function Parts"]]
                      (mapcat (fn [part-name]
                                [[line :color "#D8D8D8"]
                                 [component-part :defn part-name (pretty-label part-name)]])
                              defn-events/common-parts))
                (let [n-arities (count arity-data)]
                  (map (fn [arity-index]
                         (defn-arity-parts arity-index n-arities))
                       (range n-arities)))))])

(defn def-parts
  []
  [v-box :gap "5px"
   :children
   (into [[title :level :level2 :label "Var Parts"]]
         (mapcat (fn [part-name]
                   [[line :color "#D8D8D8"]
                    [component-part :defn part-name (pretty-label part-name)]])
                 def-events/parts))])

(defn ns-parts
  []
  [v-box :gap "5px"
   :children
   (into [[title :level :level2 :label "Namespace Parts"]]
         (mapcat (fn [part-name]
                   [[line :color "#D8D8D8"]
                    [component-part :defn part-name (pretty-label part-name)]])
                 def-events/parts))])

(defn form-parts
  []
  (let [form-data  (re-frame/subscribe [::subs/current-form-data])
        arity-data (re-frame/subscribe [::subs/the-defn-arity-data])]
    (cljs.pprint/pprint [:form-parts (:defn-args (:form @form-data))])
    (when @form-data
      (condp = (:type @form-data)
        :defn [defn-parts @form-data @arity-data]
        :def [def-parts]
        :ns [ns-parts]
        [defn-parts @form-data @arity-data]))))

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

(defn type-label
  [ref-type]
  (condp = ref-type
    :def "zmdi-settings"
    :defn "zmdi-functions"
    :ns "zmdi-format-list-bulleted"
    "zmdi-help"))

(defn var-view
  [var-data default-selection]
  (let [ns-vars      (mapv (fn [[id data]]
                             (let [{:keys [type name]} data]
                               (merge (select-keys data [:name :type])
                                      {:id    id
                                       :label [h-box :align :center :gap "7px" :children
                                               [[md-icon-button :md-icon-name (type-label type) :size :smaller]
                                                [label :label name]]]})))
                           var-data)
        type-mapping (apply merge (map #(hash-map (:id %) (select-keys % [:name :type])) ns-vars))
        selected-var (r/atom default-selection)]
    [v-box :gap "5px"
     :children
     [[vertical-pill-tabs
       :model selected-var
       :tabs ns-vars
       :on-change (fn [var-id]
                    (let [{:keys [type name]} (type-mapping var-id)]
                      (re-frame/dispatch [::whole-ns/current-form-data {:id var-id :type type :name name}]))
                    (reset! selected-var var-id))]]]))

(defn format-ns
  [the-ns-name]
  (let [[_ first-part last-part] (re-find #"(.*)\.(.*)" (str the-ns-name))]
    [h-box :align :center :children
     [[label :style {:color "grey" :font-size :smaller} :label (str first-part ".")]
      [label :style {:color "blue" :font-weight :bolder} :label last-part]]]))

(def first-time (atom true))

(defn ns-view
  [the-ns-name]
  (let [ns-data (re-frame/subscribe [::subs/id-index the-ns-name])
        var-id  (and @ns-data (first (last @ns-data)))]
    (when var-id
      (let [{:keys [type name]} (last (last @ns-data))]
        (when @first-time
          (re-frame/dispatch [::whole-ns/current-form-data {:id var-id :type type :name name}])
          (reset! first-time false)))
      [v-box :gap "5px"
       :children
       [[title :level :level2 :label (format-ns the-ns-name)]
        (var-view @ns-data var-id)]])))

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

(defn whole-ns-view []
  (let [the-ns-name (re-frame/subscribe [::subs/the-ns-name])]
    (when @the-ns-name
      [v-box :gap "20px" :justify :center :padding "15px"
       :children
       [[transform-options]
        [line :color "#D8D8D8"]
        [h-box :align :start :gap "75px" :padding "5px"
         :children
         [[ns-view @the-ns-name]
          [form-view]
          [line :color "#D8D8D8"]
          [form-parts]]]]])))

(defn render []
  (rdom/render [whole-ns-view] (js/document.getElementById "form-editor"))

  (let [mapping (key-mapping)]
    (.. (js/document.querySelectorAll ".mod,.alt,.ctrl")
        (forEach #(when-let [k (get mapping (.-innerHTML %))]
                    (set! (.-innerHTML %) k)))))

  (when (linux?)
    (js/twemoji.parse (.-body js/document))))
