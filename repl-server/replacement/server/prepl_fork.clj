(ns replacement.server.prepl-fork
	"Forking the prepl and io-prepl until error flushing is fixed:
	See the details at: https://clojure.atlassian.net/browse/CLJ-2645"
	(:require [clojure.main :as m]))

(defn- ex->data
	[ex phase]
	(assoc (Throwable->map ex) :phase phase))

(defn prepl
	"a REPL with structured output (for programs)
	reads forms to eval from in-reader (a LineNumberingPushbackReader)
	Closing the input or passing the form :repl/quit will cause it to return

	Calls out-fn with data, one of:
	{:tag :ret
	 :val val ;;eval result, or Throwable->map data if exception thrown
	 :ns ns-name-string
	 :ms long ;;eval time in milliseconds
	 :form string ;;iff successfully read
	 :exception true ;;iff exception thrown
	}
	{:tag :out
	 :val string} ;chars from during-eval *out*
	{:tag :err
	 :val string} ;chars from during-eval *err*
	{:tag :tap
	 :val val} ;values from tap>

	You might get more than one :out or :err per eval, but exactly one :ret
	tap output can happen at any time (i.e. between evals)
	If during eval an attempt is made to read *in* it will read from in-reader unless :stdin is supplied

	Alpha, subject to change."
	{:added "1.10"}
	[in-reader out-fn & {:keys [stdin]}]
	(let [EOF   (Object.)
				tapfn #(out-fn {:tag :tap :val %1})]
		(m/with-bindings
			(in-ns 'user)
			(binding [*in*  (or stdin in-reader)
								*out* (PrintWriter-on #(out-fn {:tag :out :val %1}) nil)
								*err* (PrintWriter-on #(out-fn {:tag :err :val %1}) nil)]
				(try
					(add-tap tapfn)
					(loop []
						(when (try
										(let [[form s] (read+string {:eof EOF :read-cond :allow} in-reader)]
											(try
												(when-not (identical? form EOF)
													(let [start (System/nanoTime)
																ret   (eval form)
																ms    (quot (- (System/nanoTime) start) 1000000)]
														(when-not (= :repl/quit ret)
															(binding [*out* *err*] (flush))              ;; <-- The hack
															(set! *3 *2)
															(set! *2 *1)
															(set! *1 ret)
															(out-fn {:tag  :ret
																			 :val  (if (instance? Throwable ret)
																							 (Throwable->map ret)
																							 ret)
																			 :ns   (str (.name *ns*))
																			 :ms   ms
																			 :form s})
															true)))
												(catch Throwable ex
													(set! *e ex)
													(out-fn {:tag       :ret :val (ex->data ex (or (-> ex ex-data :clojure.error/phase) :execution))
																	 :ns        (str (.name *ns*)) :form s
																	 :exception true})
													true)))
										(catch Throwable ex
											(set! *e ex)
											(out-fn {:tag       :ret :val (ex->data ex :read-source)
															 :ns        (str (.name *ns*))
															 :exception true})
											true))
							(recur)))
					(finally
						(remove-tap tapfn)))))))


(defn- resolve-fn [valf]
	(if (symbol? valf)
		(or (resolve valf)
				(requiring-resolve valf)
				(throw (Exception. (str "can't resolve: " valf))))
		valf))

(defn io-prepl
	"prepl bound to *in* and *out*, suitable for use with e.g. server/repl (socket-repl).
	:ret and :tap vals will be processed by valf, a fn of one argument
	or a symbol naming same (default pr-str)

	Alpha, subject to change."
	{:added "1.10"}
	[& {:keys [valf] :or {valf pr-str}}]
	(let [valf (resolve-fn valf)
				out  *out*
				lock (Object.)]
		(prepl *in*
					 (fn [m]
						 (binding [*out* out, *flush-on-newline* true, *print-readably* true]
							 (locking lock
								 (prn (if (#{:ret :tap} (:tag m))
												(try
													(assoc m :val (valf (:val m)))
													(catch Throwable ex
														(assoc m :val (valf (ex->data ex :print-eval-result))
																		 :exception true)))
												m))))))))