(ns replacement.protocol.diffs)


(comment

  (require '[replacement.protocol.events :as events])
  (require '[replacement.protocol.text-parsing :as text-parsing])

  (def hello-db (events/add+index-ns {} (->> text-parsing/hello-sample
                                             (text-parsing/text->edn-forms)
                                             (text-parsing/whole-ns->spec-form-data)
                                             (events/add-reference-data))))

  (def clojured-DB
    (let [form-id (-> (:form-name->id hello-db) :replacement.greet/hello-world)
          updates ["(defn hello-world \"Welcome to repl-acement\" [] \"Hello world\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello worl\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello wor\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello wo\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello w\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello C\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Cl\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Clo\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Cloj\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Cloju\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Clojur\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello Clojure\")"
                   "(defn hello-world \"Welcome to repl-acement\" [] \"Hello ClojureD\")"]]
      (reduce (fn [state change]
                (update-state state change form-id))
              hello-db updates)))

  (require '[clojure.data :as data])

  (let [current-state-id (first (:changelog clojured-DB))
        original-state-id (last (:changelog clojured-DB))
        current-state (split-defn-args (get-in clojured-DB [current-state-id :defn-conformed :defn-args]))
        original-state (split-defn-args (get-in clojured-DB [original-state-id :defn-conformed :defn-args]))]
    (events/pprint [:current-state current-state])
    (events/pprint [:original-state original-state])
    (data/diff current-state original-state))

  )