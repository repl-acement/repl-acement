(ns repl.repl.code-mirror
  "Code mirror support"
  (:require
    [re-frame.core :refer [reg-fx dispatch]]
    [cljsjs.codemirror]
    [cljsjs.codemirror.mode.clojure]
    [cljsjs.codemirror.addon.edit.matchbrackets]
    [cljsjs.codemirror.addon.hint.show-hint]
    [cljsjs.parinfer-codemirror]
    [cljsjs.parinfer]
    [cljsjs.google-diff-match-patch]))

(defn text-area
  [id]
  (fn [] [:textarea {:id id :default-value ""}]))

(defn parinfer
  [dom-node config]
  (let [editor-options (clj->js (merge {:mode :clojure} (:options config)))
        code-mirror    (js/CodeMirror.fromTextArea dom-node editor-options)
        editor-width   (get-in config [:size :width] "100%")
        editor-height  (get-in config [:size :height] "100%")]
    (.setSize code-mirror editor-width editor-height)
    (js/parinferCodeMirror.init code-mirror)
    code-mirror))

(reg-fx
  ::set-cm-value
  (fn [{:keys [code-mirror value]}]
    (.setValue code-mirror value)))

(reg-fx
  ::patch-cm-value
  (fn [{:keys [code-mirror patch]}]
    (let [value   (.getValue code-mirror)
          differ  (js/diff_match_patch.)
          js-patch (.patch_fromText differ patch)
          [new-val patched?] (.patch_apply differ js-patch value)]
      (when patched?
        (.setValue code-mirror new-val)))))

