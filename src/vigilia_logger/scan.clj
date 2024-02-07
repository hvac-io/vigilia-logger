(ns vigilia-logger.scan
  (:require [bacure.local-save :as local]
            [bacure.remote-device :as rd]
            [clj-time.core :as time]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [com.climate.claypoole.lazy :as lazy]
            [vigilia-logger.configs :as configs]
            [vigilia-logger.encoding :as encoding]
            [vigilia-logger.http :as http]))

;; TODO: min-range and max-range are non-inclusive, which is confusing.
;;       Consider a breaking change to make them inclusive.

(defn find-ids-to-scan
  "Check all the different filtering options and return a list of sorted
   device IDs to scan."
  [potential-ids {:keys [max-range min-range id-to-remove id-to-keep] :as configs}]
  (let [min-fn (fn [x] (if min-range (filter #(> % min-range) x) x))
        max-fn (fn [x] (if max-range (filter #(< % max-range) x) x))]
    (cond-> (set potential-ids)
      id-to-remove (set/difference (into #{} id-to-remove))
      id-to-keep (set/intersection (into #{} id-to-keep))
      true min-fn
      true max-fn
      true sort)))

(comment
  (find-ids-to-scan (range 1 15)
                    {:min-range    5  ; remains: 6 7 8 9 10 11 12 13 14
                     :max-range    13 ; remains: 6 7 8 9 10 11 12
                     :id-to-keep (concat (range 1 8) ;everything but 8
                                         (range 9 15))
                     ; remains: 6 7 9 10 11 12
                     :id-to-remove [11]})
  ;=> (6 7 9 10 12)
  )


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
  "Try to avoid consecutive ids"
  [batch-size ids]
  (let [id-qty (count ids)
        segment-size (max 1 (int (/ id-qty batch-size)))
        colls (partition-all segment-size ids)]
    (apply interleave-all colls)))

(comment
  ;; Simulate the effect of a BACnet network where a device
  ;; communication affects its 'neighbor' communication speed. For
  ;; example, an MS/TP loop with limited bandwidth.

  ;; This is only to give a rough idea. Consider switching to
  ;; 'virtual' time for more serious/rigorous testing.

  (defn simulate!
    [reorder-fn {:keys [batch-size group-size]}]
    (time
     (let [*fetching (atom {})
           test-devices (for [i (range 250)
                              :let [*ret (promise)
                                    group (quot i group-size)]]
                          (fn []
                            (future
                              ;; Increase delay if already fetching a device from same group.
                              (let [penalty 1.4
                                    factor (Math/pow penalty (inc (get @*fetching group 0)))]
                                (Thread/sleep (* factor 100)))
                              (deliver *ret i))
                            (with-meta *ret {:id i, :group group})))]
       (doall (lazy/upmap batch-size (fn [f]
                                       (let [*ret (f)]
                                         (Thread/sleep (+ 20 (rand-int 30)))
                                         (let [{:keys [group id]} (meta *ret)
                                               _ (swap! *fetching update group (fnil inc 0))
                                               ret (deref *ret)
                                               _ (swap! *fetching update group (fnil dec 1))]
                                           ret)))
                          (reorder-fn batch-size test-devices)))
       nil)))

  (simulate! reorder-ids {:batch-size 5, :group-size 25})       ;  7127 ms
  (simulate! (fn [_ ids] ids) {:batch-size 5, :group-size 25})  ; 21406 ms

  (simulate! reorder-ids {:batch-size 10, :group-size 25})      ;  3138 ms
  (simulate! (fn [_ ids] ids) {:batch-size 10, :group-size 25}) ; 19614 ms

  (simulate! reorder-ids {:batch-size 20, :group-size 25})      ;  2497 ms
  (simulate! (fn [_ ids] ids) {:batch-size 20, :group-size 25}) ; 42834 ms

  (simulate! reorder-ids {:batch-size 30, :group-size 25})      ;  2649 ms

  )

;;;;;;;


(defn scan-network
  "Scan the network and return a `snapshot' for logging purposes."[]
  (let [configs (configs/fetch)
        ids-to-scan (->> (find-ids-to-scan (rd/remote-devices) configs)
                         (reorder-ids batch-size))
        scan-fn (fn [device-id]
                  (let [{:keys [object-delay target-objects]} configs]
                    (scanning! device-id)
                    (try
                      (encoding/scan-device device-id (get target-objects device-id) object-delay)
                      (finally (scanned! device-id)))))]
    ;; we begin a new scan
    (with-scan-status ids-to-scan
      (reduce merge (lazy/upmap batch-size scan-fn ids-to-scan)))))


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
