(ns vigilia-logger.timed
  (:require [bacure.core :as b]
            [bacure.remote-device :as rd]
            [bacure.services :as services]
            [clojure.tools.logging :as log]
            [overtone.at-at :as ot]
            [vigilia-logger.configs :as configs]
            [vigilia-logger.scan :as scan]
            [vigilia-logger.tools :as tools]))

(defonce pool (ot/mk-pool))

(declare start-logging)

(defonce logging-state
  (atom "Stopped"))

(def scan-active? (atom nil))

(defn stop-logging []
  (ot/stop-and-reset-pool! pool :strategy :kill)
  (reset! logging-state "Stopped")
  (reset! scan-active? nil)
  (log/info "Vigilia logger stopped"))

(defn restart-logging []
  (stop-logging)
  (start-logging))

(defn is-logging?
  "Return true if we are currently logging." []
  (not= @logging-state "Stopped"))

(defn min-ms
  "Convert minutes into milliseconds."
  [time-in-min]
  (* 60000 time-in-min))

;;; Network discovery

(def default-discovery-interval
  "Minutes between two automatic Who-Is broadcasts."
  60)

(defn discovery-interval-ms
  "Delay between two automatic Who-Is broadcasts, from the
  configs (:discovery-interval, in minutes). Nil when disabled (0)."
  [configs]
  (let [minutes (:discovery-interval configs default-discovery-interval)]
    (when (pos? minutes)
      (min-ms minutes))))

(defn who-is-range
  "The device ID range for our Who-Is, from the configs. Only the
  devices within :min-range and :max-range (when set) will answer,
  instead of the whole network. Nil when no range is configured."
  [{:keys [min-range max-range]}]
  (when (or min-range max-range)
    ;; The logger ranges are exclusive; the BACnet ones are inclusive.
    (cond-> {}
      min-range (assoc :min-range (inc min-range))
      max-range (assoc :max-range (dec max-range)))))

(def *last-execution-time (atom 0))
(defn throttled-find-remote-devices
  "Same as `find-remote-devices`, but not more than once
  per :discovery-interval (60 minutes by default), and restricted to
  the configured device ID range, if any. Does nothing if the
  interval is 0."
  []
  (let [configs (configs/fetch)]
    (when-let [interval-ms (discovery-interval-ms configs)]
      (let [current-time (System/currentTimeMillis)]
        (when (> (- current-time @*last-execution-time) interval-ms)
          (reset! *last-execution-time current-time)
          (rd/find-remote-devices nil (who-is-range configs)))))))

;;; Perimeter

;; The configs are read from the disk; don't do it for every I-Am.
(def ^:dynamic *perimeter-configs-max-age-ms* 60000)

(defonce ^:private perimeter-configs
  (atom {:at 0 :configs nil}))

(defn- current-perimeter-configs
  "The logger configs, re-read at most every
  `*perimeter-configs-max-age-ms*`. If the configs can't be read, the
  last valid ones are kept."
  []
  (let [now (System/currentTimeMillis)
        {:keys [at configs]} @perimeter-configs]
    (if (< (- now at) *perimeter-configs-max-age-ms*)
      configs
      (let [configs (or (configs/fetch) configs)]
        (reset! perimeter-configs {:at now :configs configs})
        configs))))

(defn in-perimeter?
  "True if the device would be scanned with the current configs
  (id-to-keep, id-to-remove, min-range, max-range)."
  [device-id]
  (boolean (seq (scan/find-ids-to-scan [device-id] (current-perimeter-configs)))))

(defn auto-fetch-filter
  "Filter for `bacure.remote-device/set-auto-fetch-filter!`: while
  logging, Bacure only fetches on its own the extended information of
  the devices we scan. When not logging, everything is fetched, as
  usual."
  [device-id]
  (or (not (is-logging?))
      (in-perimeter? device-id)))

(defn install-auto-fetch-filter!
  "Restrict what Bacure fetches on its own to our perimeter (see
  `auto-fetch-filter`)."
  []
  (rd/set-auto-fetch-filter! auto-fetch-filter))


(defn start-logging
  "Add jobs to be executed in the future and/or at regulvar interval.

  :logger ---------> We scan the network at a regulvar time
                     interval (:time-interval in the configs). Also,
                     check for configuration update and send back any
                     local logs found. A Who-Is is sent
                     every :discovery-interval minutes (60 by default,
                     0 to disable).

  ;refresh --------> Restart the local device each week. (This is
                     done in order to discard any `visitor devices'
                     that are no longer on the network, and to clean
                     some 'duplicates' that sometimes occur in the
                     underlying Bacnet4j library.)"
  []
  (when (compare-and-set! logging-state "Stopped" "Mapping network") ;;don't log twice simultaneously
    (log/info "Vigilia logger started")
    (install-auto-fetch-filter!)
    (future ;; in another thread
      (when-not (= @logging-state "Stopped") ;; if we didn't stop the logging meanwhile
        (reset! logging-state "Logging")
        (let [time-interval (min-ms (or (:time-interval (configs/fetch)) 10)) ;; default 10 minutes
              reset-interval (min-ms (* 60 24 7)) ;; 7 days
              after-scan-fn (atom nil)
              logger-job-fn
              (fn [] ;; will start logging and return the pool job
                (ot/every time-interval
                          (fn []
                            (if @scan-active?
                              ;; Skip this scan if the previous one isn't done yet
                              (log/warn "Previous scan incomplete... skipping this round.")

                              ;; we need to catch exception because
                              ;; we can't interrupt sleeping
                              ;; processes. (This means we might not
                              ;; succeed in restarting the local
                              ;; BACnet device).
                              (do
                                (try
                                  ;; Send local logs immediately in a parallel thread
                                  (future
                                    (try
                                      (tools/with-timeout time-interval "Sending local logs"
                                        (scan/send-local-logs))
                                      (catch Exception e
                                        (log/error (.getMessage e)))))

                                  ;; If a scan takes longer than 2 hours, we have a problem...
                                  (tools/with-timeout (* 1000 60 60 2) ""
                                    (log/info "Start scan")
                                    (reset! scan-active? true) ;; mark the scan as active
                                    (services/send-who-is-router-to-network nil)
                                    (throttled-find-remote-devices) ;; In case of new devices
                                    (scan/scan-and-send)
                                    (log/info
                                     (format "Scan completed in %.2fs"
                                             (some-> @scan/scanning-state :scanning-time-ms (/  1000.0)))))

                                  (catch Exception e
                                    (log/error (str "Network scan exception: "(.getMessage e)))))

                                (try
                                  (when-let [f @after-scan-fn]
                                    (f)
                                    (reset! after-scan-fn nil))
                                  (finally
                                    (reset! scan-active? nil))))))
                          pool
                          :desc "Logging the network"))
              logger-job-atom (atom (logger-job-fn))]
          {:logger logger-job-atom
           :refresh  (ot/every reset-interval
                               #(reset! after-scan-fn
                                        (fn []
                                          (try
                                            (log/info "Restarting local BACnet device")
                                            (b/boot-up! nil {:auto-fetch-filter auto-fetch-filter})
                                            (catch Exception e
                                              (log/error (str "Exception: "(.getMessage e)))))))
                               pool
                               :initial-delay reset-interval
                               :desc "Logging reset every week")})))))

(defn maybe-start-logging
  "If a project-id is configured, start the logging and return true.
  Do nothing otherwise and return nil." []
   (when (:project-id (configs/fetch))
     (do (start-logging) true)))
