(ns replacement.server.await
  (:require
    [clojure.core.async :as async]
    [replacement.server.async-prepl :as async-prepl]))

(defn correlate-results
  [tag-count results prepl-output]
  (if (nil? prepl-output)                                   ; edge case
    (reduced results)
    (let [updated       (conj results prepl-output)
          ret-tag-count (count (filter #(= (:tag %) :ret) updated))]
      (if (= ret-tag-count tag-count)
        (reduced updated)
        updated))))

(defn gather-sync
  "Called to synchronously gather results"
  ([channel]
   (gather-sync channel 1))
  ([channel tag-count]
   (reduce (partial correlate-results tag-count)
           [] (repeatedly #(async/<!! channel)))))

(defn async-test-prepl
  "Returns the writer for an initialized PREPL. The results of any forms
  witten on the writer will be available on the given `results-ch`"
  [out-ch]
  (let [{:keys [init-count] :as prepl-opts} (async-prepl/init-prepl {:out-ch out-ch})]
    ;; take init evaluation results off out-ch, not relevant to clients
    (gather-sync out-ch init-count)
    prepl-opts))