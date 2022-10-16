(ns vigilia-logger.tools)

(defn- timeout-exception
  [timeout-ms timeout-msg]
  (ex-info (str "Timeout after " timeout-ms " ms"
                (when (not-empty timeout-msg)
                  ": ")
                timeout-msg)
           {}))

(defn timeout
  "Run the function in a future and return its result.
  If the function takes more time than the provided `timeout-ms`,
  cancel the future and throw an exception."
  ([timeout-ms f] (timeout timeout-ms f nil))
  ([timeout-ms f timeout-msg]
   (let [fut (future (f))
         ret (deref fut timeout-ms ::timed-out)]
     (when (= ret ::timed-out)
       (future-cancel fut)
       (throw (timeout-exception timeout-ms timeout-msg)))
     ret)))

(defmacro with-timeout
  "Eval body and return its result within the provided timeout time,
  otherwise throws an exception"
  [timeout-ms timeout-msg & body]
  `(timeout ~timeout-ms (fn [] ~@body) ~timeout-msg))
