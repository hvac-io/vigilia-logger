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
            [trptcolin.versioneer.core :as version]
            [vigilia-logger.configs :as configs]
            [vigilia-logger.encoding :as encoding]
            [vigilia-logger.http :as http]))




(def logger-version
  "The logger version used to check what data encoding is used."
  (str "vigilia-logger-" (version/get-version "io.hvac.vigilia.logger" "vigilia-logger")))






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

(defn- mark-as-scanning! [device-id]
  (swap! scanning-state update-in [:ids-scanning] conj device-id))

(defn- mark-as-scanned! [device-id]
  (let [ss @scanning-state]
    (reset! scanning-state
            (-> (update-in ss [:ids-scanning] disj device-id)
                (update-in [:ids-scanned] conj device-id)))))

(defn- mark-start-of-scan! [ids-to-scan]
  (swap! scanning-state
         assoc
         :scanning? true
         :start-time (time/now)
         :ids-to-scan (set ids-to-scan)))

(defn- mark-end-of-scan! []
  (let [ss @scanning-state
        end-time (time/now)
        start-time (or (:start-time ss) (time/now))
        completed (inc (:completed-scans ss))]
    (swap! scanning-state
           assoc
           :completed-scans completed
           :scanning? nil
           :ids-scanned #{}
           :end-time end-time
           :scanning-time-ms (- (.getMillis end-time)
                                (.getMillis start-time)))))

(defn- interleave-all
  "interleaves including remainder of longer seqs."
  [& seqs]
  (if (not-empty (first seqs))
    (cons (first (first seqs))
          (lazy-seq (apply interleave-all
                           (filter not-empty
                                   (concat (rest seqs) [(rest (first seqs))])))))))

(def ^{:private true} batch-size 20)

(defn reorder-ids
  "Try to avoid consecutive ids" [ids]
  (let [colls (partition-all (max 4 (int (/ batch-size 4))) ids)]
    (apply interleave-all colls)))

;;;;;;;


(defn scan-network
  "Scan the network and return a `snapshot' for logging purposes."[]
  (let [configs (configs/fetch)
        read-object-delay (:object-delay configs)
        target-objects (:target-objects configs)
        ids-to-scan (-> (find-id-to-scan)
                        (sort)
                        (reorder-ids))
        scan-fn (fn [device-id]
                  (mark-as-scanning! device-id)
                  (let [scan-data (encoding/scan-device device-id
                                                        (get target-objects device-id)
                                                        read-object-delay)]
                    (mark-as-scanned! device-id)
                    scan-data))]
    ;; we begin a new scan
    (mark-start-of-scan! ids-to-scan)
    (let [scan-result (doall (lazy/upmap batch-size scan-fn ids-to-scan))]
      (mark-end-of-scan!)
      (apply merge scan-result))))


(defn find-unsent-logs []
  (let [filename-list (seq (.listFiles (io/file (configs/logs-path))))]
    (->> filename-list
         (map #(.getName %))
         (filter #(re-find #"vigilia.*\.log" %))
         (sort))))



(defn send-logs
  "Send the logs to the remote server. NIL if successful. In case of
  error, return the response."
  [{:keys [api-path project-id logger-id logger-version logger-key]} logs]
  (let [response (http/request {:url    api-path
                                :method :post
                                :as     :text
                                :body   {:logger-key     logger-key
                                         :logger-id      logger-id
                                         :logger-version logger-version
                                         :logs           logs}})]
    (when (http/error? response)
      response)))


(defn send-to-remote-server
  "Send the data to remote servers. Return NIL if successful."
  [data]
  (let [{:keys [project-id logger-key]} (configs/fetch)
        {:keys [logging-allowed? href]} (http/get-project-logger-data project-id logger-key)]
    ;; Check if server intend to accept our logs before sending them
    (if logging-allowed?
      (send-logs {:api-path       href
                  :project-id     project-id
                  :logger-id      (configs/get-logger-id!)
                  :logger-version logger-version
                  :logger-key     logger-key}
                 data)
      :logging-not-allowed)))


(defn spit-log!
  [filename data]
  (local/mkdir-spit (str (configs/logs-path) filename) data))

(defn scan-and-send
  "Scan the network and send the result to remote servers. If the server
  can't be reached, save the result in a .log file. Only saves up to
  2016 logs."
  []
  (let [data (scan-network)]
    ;; try to send to server
    (when (send-to-remote-server data) ;; nil on success
      ;; if it doesn't work, save data locally.
      (when (> 2016 (count (find-unsent-logs))) ;; ~2 weeks
        (let [filename (str/join "-" ["vigilia"
                                      (configs/get-logger-id!)
                                      (str (encoding/timestamp)
                                           ".log")])]
          (spit-log! filename data))))))

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
              (let [error? (send-to-remote-server (read-log logs-path log-name))]
                (if-not error?
                  (do (io/delete-file (str logs-path log-name))
                      (log/info "Sent.")
                      (recur rest-logs))
                  ; An error a this point is probably a network problem.
                  ; De not recur (stop trying to send logs).
                  (log/warn "Remote server can't be reached or refused the log."))))
          (log/info "No more local logs to send."))))))
