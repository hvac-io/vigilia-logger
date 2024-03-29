(ns vigilia-logger.scan
  (:require [bacure.core :as bac]
            [bacure.local-save :as local]
            [bacure.remote-device :as rd]
            [clj-time.core :as time]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [com.climate.claypoole.lazy :as lazy]
            [vigilia-logger.configs :as configs]
            [vigilia-logger.encoding :as encoding]
            [vigilia-logger.http :as http]))

;;; section for 'advanced' filtering

(defn filter-device
  "Test the criteria maps agains a device ID and return :remove if any
  succeed, otherwise :keep. If the extended information is not yet
  available, simply return nil." [id criteria-coll]
  (try
    (when (rd/extended-information id)
      (let [remote-device-props
            (bac/remote-object-properties id [:device id]
                                          [:vendor-identifier :description :device-type
                                           :vendor-name :object-name :model-name])]
        ;;don't use `:all', it might not return the model-name if it's a
        ;;device that doesn't support read-property-multiple.
        (-> (filter (bac/where (first criteria-coll)) remote-device-props)
            ((fn [x] (let [crits (next criteria-coll)]
                       (cond (and (seq x) (seq (first criteria-coll))) :remove
                             crits (filter-device id crits)
                             :else :keep)))))))
    (catch Exception e nil)))


(def remove-device-table
  "A map of the device ID, associated with its associated scan behavior.
   Each time a new device ID is found, it should be matched against
   the criteria-map to see if it should be scanned. Returns :keep
   or :remove. If the device is still unchecked, it will be tested
   before giving a result."
  (atom {}))

(defn remove-device?
  "Check if the device ID is marked to be removed. If it hasn't been
   tested yet, test it and record the result." [id]
  (if-let [result (get @remove-device-table id)]
    result
    (if-let [criteria-coll (:criteria-coll (configs/fetch))]
      (get (swap! remove-device-table #(->> criteria-coll
                                            (filter-device id)
                                            (assoc % id))) id)
      :keep)));;if no criteria-coll, automatically keep

(defn reset-devices-to-remove-table! []
  (reset! remove-device-table {})
  (pmap remove-device? (rd/remote-devices)))


(defn has-extended-info?
  [id]
  (try
    ;; We catch errors because if the router for a given device is not
    ;; found, it will throw an exception.
    (rd/extended-information id)
    (catch Exception e)))

(defn find-id-to-scan
  "Check all the different filtering options and return a list of
   device-id to scan." []
  (let [{:keys [max-range min-range id-to-remove id-to-keep]} (configs/fetch)
        id-to-keep-fn (fn [x] (if id-to-keep (clojure.set/intersection (into #{} id-to-keep) x) x))
        id-to-remove-fn (fn [x] (clojure.set/difference x (into #{} id-to-remove)))
        remove-device (fn [x] (filter #(= :keep (remove-device? %)) x))
        min-fn (fn [x] (if min-range (filter #(> % min-range) x) x))
        max-fn (fn [x] (if max-range (filter #(< % max-range) x) x))]
    ;; and now just keep the remote devices for which we have extended information
    (->> (rd/remote-devices)
         ;; Avoid fetching extended-info by discarding IDs first
         id-to-keep-fn
         id-to-remove-fn
         min-fn
         max-fn
         ;; Now check extended info
         (filter has-extended-info?)
         remove-device ;; <-- last to avoid querying remote devices we won't keep
         )))




;;;;;;;;;;

(defonce scanning-state
  (atom {:ids-to-scan      #{}
         :ids-scanning     #{}
         :ids-scanned      #{}
         :start-time       (time/now)
         :end-time         (time/now)
         :scanning-time-ms 0
         :completed-scans  0}))

(defn- scanning! [device-id]
  (swap! scanning-state update-in [:ids-scanning] conj device-id))

(defn- scanned! [device-id]
  (swap! scanning-state
         (fn [m]
           (-> (update-in m [:ids-scanning] disj device-id)
               (update-in [:ids-scanned] conj device-id)))))

(defn- mark-start-of-scan! [ids-to-scan]
  (swap! scanning-state
         assoc
         :scanning? true
         :start-time (time/now)
         :ids-to-scan (set ids-to-scan)))

(defn- mark-end-of-scan! []
  (swap! scanning-state
         (fn [m]
           (let [end-time (time/now)
                 start-time (or (:start-time m) (time/now))]
             (-> (update m :completed-scans inc)
                 (assoc :scanning? nil
                        :ids-scanned #{}
                        :end-time end-time
                        :scanning-time-ms (- (.getMillis end-time)
                                             (.getMillis start-time))))))))

(defmacro with-scan-status
  [ids-to-scan & body]
  `(do (mark-start-of-scan! ~ids-to-scan)
       (try ~@body
            (finally (mark-end-of-scan!)))))

;;;;

(defn- interleave-all
  "interleaves including remainder of longer seqs."
  [& seqs]
  (if (not-empty (first seqs))
    (cons (first (first seqs))
          (lazy-seq (apply interleave-all
                           (filter not-empty
                                   (concat (rest seqs) [(rest (first seqs))])))))))

(def ^{:private true} batch-size 20)

(defn- reorder-ids
  "Try to avoid consecutive ids" [ids]
  (let [colls (partition-all (max 4 (int (/ batch-size 4))) ids)]
    (apply interleave-all colls)))

;;;;;;;


(defn scan-network
  "Scan the network and return a `snapshot' for logging purposes."[]
  (let [{:keys [object-delay target-objects]} (configs/fetch)
        ids-to-scan (-> (find-id-to-scan)
                        (sort)
                        (reorder-ids))
        scan-fn (fn [device-id]
                  (scanning! device-id)
                  (try
                    (encoding/scan-device device-id (get target-objects device-id) object-delay)
                    (finally (scanned! device-id))))]
    ;; we begin a new scan
    (with-scan-status ids-to-scan
      (apply merge (lazy/upmap batch-size scan-fn ids-to-scan)))))


(defn find-unsent-logs []
  (let [filename-list (seq (.listFiles (io/file (configs/logs-path))))]
    (->> filename-list
         (map #(.getName %))
         (filter #(re-find #"vigilia.*\.log" %))
         (sort))))


(defn- spit-log!
  [filename data]
  (local/mkdir-spit (str (configs/logs-path) filename) data))

(defn scan-and-send
  "Scan the network and send the result to remote servers. If the server
  can't be reached, save the result in a .log file. Only saves up to
  2016 logs."
  []
  (let [log (scan-network)]
    ;; try to send to server
    (let [error? (http/send-log log)] ;; nil on success
      (when error?
        (log/warn "Remote server can't be reached or refused the log.")
        ;; if it doesn't work, save log locally.
        (when (> 2016 (count (find-unsent-logs))) ;; ~2 weeks
          (let [filename (str/join "-" ["vigilia"
                                        (configs/get-logger-id!)
                                        (str (encoding/timestamp)
                                             ".log")])]
            (spit-log! filename log)))))))

(defn read-log
  "Read the log and return a map or nil."
  [logs-path log-name]
  (let [log-file (io/file logs-path log-name)]
    (let [log (try (edn/read-string (slurp log-file))
                   (catch Exception e ::error))]
      (if (map? log) ; everything other than a map returns nil
        log
        nil))))

(defn send-local-logs
  "Check in the logger path for any unsent logs. If the server can't
   be reached (or simply refuses logs), keep them locally." []
  (let [logs (find-unsent-logs)
        logs-path (configs/logs-path)]
    (when (seq logs)
      (log/info (format "Found %d local logs..." (count logs)))
      (loop [[log-name & rest-logs] logs]
        (if log-name
          (do (log/info (str "sending " log-name "..."))
              (let [error? (http/send-log (read-log logs-path log-name))]
                (if-not error?
                  (do (io/delete-file (str logs-path log-name))
                      (log/info "Sent.")
                      (recur rest-logs))
                  ; An error a this point is probably a network problem.
                  ; Do not recur (stop trying to send logs).
                  (log/warn "Remote server can't be reached or refused the log."))))
          (log/info "No more local logs to send."))))))
