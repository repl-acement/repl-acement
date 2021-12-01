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
            [re-com.tabs :refer [vertical-pill-tabs horizontal-tabs]]
            [re-frame.core :as re-frame]
            [replacement.forms.events.def :as def-events]
            [replacement.forms.events.defn :as defn-events]
            [replacement.forms.events.ns :as ns-events]
            [replacement.forms.events.req-libs :as req-lib-events]
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
  [part-cm-name event-name tx]
  (re-frame/dispatch [event-name part-cm-name tx]))

(defn comp-editor-view
  [dom-element initial-document part-cm-name edit-event]
  (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                        :extensions extensions})
                    :parent   (rdom/dom-node dom-element)
                    :dispatch (partial part-edit part-cm-name edit-event)}))

(defn comp-editor
  "Produces a function to act on a code mirror view with the given cm-name
  using an optional initial document. An empty string is used if no
  document is provided."
  ([cm-name cm-event-name edit-event-name]
   (comp-editor cm-name "" cm-event-name edit-event-name))
  ([cm-name initial-document cm-event-name edit-event-name]
   (fn [dom-element]
     (let [!view (comp-editor-view dom-element initial-document cm-name edit-event-name)]
       (re-frame/dispatch-sync [cm-event-name !view cm-name])))))

(defn part-editor
  ([cm-name part-type]
   (part-editor cm-name part-type ""))
  ([cm-name part-type document]
   (let [editor (partial comp-editor cm-name document)]
     [:div {:ref (condp = part-type
                   :req-libs (editor ::req-lib-events/set-cm+name ::req-lib-events/part-edit)
                   :def (editor ::def-events/set-cm+name ::def-events/part-edit)
                   :defn (editor ::defn-events/set-cm+name ::defn-events/part-edit)
                   :ns (editor ::ns-events/set-cm+name ::ns-events/part-edit))}])))

(defn form-editor
  [{:keys [data text-key cm-key tx-event-name cm-event-name] :as x}]
  (let [initial-document (-> (get-in data [:form text-key])
                             (zprint-file-str ::editable-form))
        !mount           (fn [comp]
                           (let [cm-name (wiring/comp-name->cm-name cm-key)
                                 !view   (EditorView. #js {:state    (.create EditorState #js {:doc        initial-document
                                                                                               :extensions extensions})
                                                           :parent   (rdom/dom-node comp)
                                                           :dispatch (fn [tx]
                                                                       (re-frame/dispatch [tx-event-name cm-name tx]))})]
                             (re-frame/dispatch-sync [cm-event-name !view cm-name])))]
    [:div {:ref !mount}]))

(defn var-form-editor
  [form-data]
  (form-editor {:data          form-data
                :text-key      :def.text
                :cm-key        :def.form
                :tx-event-name ::def-events/def-whole-form-tx
                :cm-event-name ::def-events/set-cm+name}))

(defn defn-form-editor
  [form-data]
  (form-editor {:data          form-data
                :text-key      :defn.text
                :cm-key        :defn.form
                :tx-event-name ::defn-events/fn-whole-form-tx
                :cm-event-name ::defn-events/set-cm+name}))

(defn ns-form-editor
  [form-data]
  (form-editor {:data          form-data
                :text-key      :ns.text
                :cm-key        :ns.form
                :tx-event-name ::ns-events/whole-form-tx
                :cm-event-name ::ns-events/set-cm+name}))

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
                   (:var-form (swap! editable-forms assoc :var-form [var-form-editor @form-data])))
          :defn (or (:defn-form @editable-forms)
                    (:defn-form (swap! editable-forms assoc :defn-form [defn-form-editor @form-data])))
          :ns (or (:ns-form @editable-forms)
                  (:ns-form (swap! editable-forms assoc :ns-form [ns-form-editor @form-data])))
          ;; TODO - improve default behaviour ...
          [label :label "Unknown form"])]])))

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
   [h-box :gap "5px" :align :center
    :children
    [[label :width "110px" :label label-text]
     [part-editor (wiring/comp-name->cm-name part-name) form-type document]]]))

(defn- defn-component-parts
  [part-list]
  (mapcat (fn [part-name]
            [[component-part :defn part-name (pretty-label part-name)]
             [line :color "#D8D8D8"]])
          part-list))

(defn- require-component-parts
  [part-list]
  (mapcat (fn [part-name]
            [[component-part :req-libs part-name (pretty-label part-name)]
             [line :color "#D8D8D8"]])
          part-list))

(def editable-defn-arity-parts (atom nil))

(defn defn-arity-parts
  []
  (let [arity-parts (or @editable-defn-arity-parts
                        (reset! editable-defn-arity-parts (defn-component-parts defn-events/arity-parts)))]
    [v-box :gap "5px" :width "500px" :children arity-parts]))

(def editable-defn-parts (atom nil))

(defn defn-parts
  [arity-data]
  (let [arity-elements (map-indexed (fn [idx _]
                                      {:id    idx
                                       :label (str "Arity " (inc idx))})
                                    arity-data)
        selected-var   (r/atom 0)
        common-parts   (or @editable-defn-parts
                           (reset! editable-defn-parts (defn-component-parts defn-events/common-parts)))]
    [v-box :children
     [[v-box :gap "5x" :width "500px"
       :children (into [[title :level :level2 :label "Function parts"]] common-parts)]
      [gap :size "10px"]
      [horizontal-tabs
       :model selected-var
       :tabs arity-elements
       :on-change (fn [var-id]
                    (re-frame/dispatch [::defn-events/fn-arity-update-cms (nth arity-data var-id)])
                    (reset! selected-var var-id))]
      [defn-arity-parts]]]))

(defn def-parts
  []
  (let [editable-def-parts (atom nil)]
    (fn []
      (let [parts (or @editable-def-parts
                      (reset! editable-def-parts (mapcat (fn [part-name]
                                                           [[line :color "#D8D8D8"]
                                                            [component-part :def part-name (pretty-label part-name)]])
                                                         def-events/parts)))]
        [v-box :gap "5px"
         :children (into [[title :level :level2 :label "Var Parts"]] parts)]))))

(defn ns-parts
  [form-data]
  (let [editable-require-parts (atom nil)]
    (fn []
      (let [requires      (map-indexed (fn [idx {:keys [require]}]
                                         (let [{:keys [lib]} require]
                                           {:id    idx
                                            :label (ns-events/shortened-ns-name (str (last lib)))}))
                                       (get-in form-data [:form :ns.parts :require-libs]))
            ns-part-forms (or @editable-require-parts
                              (reset! editable-require-parts (require-component-parts req-lib-events/parts)))
            first-parts   (mapcat (fn [part-name]
                                    [[line :color "#D8D8D8"]
                                     [component-part :req-libs part-name (pretty-label part-name)]])
                                  req-lib-events/parts)
            selected-var  (r/atom (:id (first requires)))]
        (fn []
          [v-box
           :children
           [[v-box :gap "5px"
             :children (into [[title :level :level2 :label "Namespace Parts"]]
                             first-parts)]
            [line :color "#D8D8D8"]
            [gap :size "20px"]
            [label :label "Requires"]
            [gap :size "20px"]
            [h-box :gap "5px"
             :children
             [[vertical-pill-tabs
               :model selected-var
               :tabs requires
               :on-change #(reset! selected-var %)]
              [v-box :children
               [ns-part-forms]]]]]])))))

(defn form-parts
  []
  (let [form-data (re-frame/subscribe [::subs/current-form-data])]
    (when @form-data
      (condp = (:type @form-data)
        :defn [defn-parts (get-in @form-data [:form :arity-data])]
        :def [def-parts]
        :ns [ns-parts @form-data]
        ;; TODO - improve default behaviour ...
        [label :label "Unknown parts"]))))

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
                      (re-frame/dispatch-sync [::whole-ns/current-form-data {:id var-id :type type :name name}])
                      (reset! selected-var var-id)))]]]))

(defn format-ns
  [the-ns-name]
  (let [[_ first-part last-part] (re-find #"(.*)\.(.*)" (str the-ns-name))]
    [h-box :align :center :children
     [[label :style {:color "grey" :font-size :smaller} :label (str first-part ".")]
      [label :style {:color "blue" :font-weight :bolder} :label last-part]]]))

(defn ns-view
  [the-ns-name]
  (let [first-time-ns-view (atom true)]
    (fn []
      (let [ns-data (re-frame/subscribe [::subs/id-index the-ns-name])
            var-id  (and @ns-data (first (last @ns-data)))]
        (when var-id
          (let [{:keys [type name]} (last (last @ns-data))]
            (when @first-time-ns-view
              (re-frame/dispatch-sync [::whole-ns/current-form-data {:id var-id :type type :name name}])
              (reset! first-time-ns-view false)))
          [v-box :gap "5px"
           :children
           [[title :level :level2 :label (format-ns the-ns-name)]
            (var-view @ns-data var-id)]])))))

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
