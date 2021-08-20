(ns repl.repl.prepl
  (:require [clojure.spec.alpha :as s]))

(s/def ::valid-string? ::general/non-empty-string?)

(def phase-indicators
  #{:read-source :macro-syntax-check :macroexpansion
    :compile-syntax-check :compilation :execution
    :read-eval-result :print-eval-result})

(s/def ::phase-error
  (s/with-gen keyword? #(s/gen phase-indicators)))

(comment
  ;; How to process exceptions . . . (only send maps to clients!)
  (let [er (try (slurp "/junk/junk-foo")
                (catch Exception e
                  (clojure.main/ex-triage (Throwable->map e))))
        {:clojure.error/keys [phase source line column symbol class cause spec]} er]
    (println "phase is" phase)))

(s/def ::problem-data
  (s/keys :req-un [::path ::pred ::val ::via ::in]))

(s/def ::explain-data
  (s/keys :req [::s/problems ::s/spec ::s/value]))

(s/def ::val ::valid-string?)
(s/def ::ns ::valid-string?)
(s/def ::ms int?)
(s/def ::form ::valid-string?)

(s/def ::prepl-ret
  (s/with-gen
    (s/keys :req-un [::tag ::val ::ns ::ms]
            :opt-un [::form ::phase-error])
    #(gen/fmap (fn [[val ns ms form? form error? phase-error]]
                 (merge {:tag :ret
                         :val val
                         :ns  ns
                         :ms  ms}
                        (when form?
                          {:form form})
                        (when error?
                          {:phase-error phase-error})))
               (gen/tuple (s/gen ::val)
                          (s/gen ::ns)
                          (s/gen ::ms)
                          (s/gen boolean?)
                          (s/gen ::form)
                          (s/gen boolean?)
                          (s/gen ::phase-error)))))

(s/def ::prepl-out
  (s/with-gen
    (s/keys :req-un [::tag ::val])
    #(gen/fmap (fn [val] {:tag :out :val val}) (s/gen ::val))))

(s/def ::prepl-err
  (s/with-gen
    (s/keys :req-un [::tag ::val])
    #(gen/fmap (fn [val] {:tag :err :val val}) (s/gen ::val))))

(s/def ::prepl-tap
  (s/with-gen
    (s/keys :req-un [::tag ::val])
    #(gen/fmap (fn [val] {:tag :err :tap val}) (s/gen ::val))))
