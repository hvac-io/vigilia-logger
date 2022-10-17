(ns vigilia-logger.timed
  (:require [bacure.local-device :as ld]
            [bacure.remote-device :as rd]
            [bacure.services :as services]
            [clj-time.local :as l]
            [clojure.tools.logging :as log]
            [overtone.at-at :as ot]
            [vigilia-logger.scan :as scan]
            [vigilia-logger.tools :as tools]))

(def pool (ot/mk-pool))

(declare start-logging)

(def logging-state
  (atom "Stopped"))

(defn stop-logging []
  (ot/stop-and-reset-pool! pool :strategy :kill)
  (reset! logging-state "Stopped")
  (log/info "----------------------")
  (log/info "Vigilia logger stopped")
  (log/info "----------------------"))

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

(defn init!
  "Reset the local device, make a list of remote devices and find
   those that should be excluded based on their properties."
  []
  (ld/reset-local-device!)
  (rd/find-remote-devices)
  (Thread/sleep 5000) ;; wait 5s
  (log/info "Fetching extended information for " (count (rd/remote-devices)) " devices")
  (rd/all-extended-information) ;; recheck for extented information
  (scan/reset-devices-to-remove-table!))

(def scan-active? (atom nil))

(defn start-logging
  "Add jobs to be executed in the future and/or at regulvar interval.

  :logger ---------> We scan the network at a regulvar time
                     interval (:time-interval in the configs). Also,
                     check for configuration update and send back any
                     local logs found.

  ;refresh --------> Restart the local device each week. (This is
                     done in order to discard any `visitor devices'
                     that are no longer on the network, and to clean
                     some 'duplicates' that sometimes occur in the
                     underlying Bacnet4j library.)

  At start: we reset the local-device, discover the network, wait a
  while and then start to log the network."[]
  (when-not (is-logging?) ;;don't log twice simultaneously
    (reset! logging-state "Mapping network")
    (log/info "----------------------")
    (log/info "Vigilia logger started")
    (log/info "----------------------")
    (future ;; in another thread
      (init!)
      (when-not (= @logging-state "Stopped") ;; if we didn't stop the logging meanwhile
        (reset! logging-state "Logging")
        (let [time-interval (min-ms (or (:time-interval (scan/get-logger-configs)) 10)) ;; default 10 minutes
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
                                    (log/info (str "Starting network scan at "
                                                   (-> (l/local-now)
                                                       (l/format-local-time :date-hour-minute-second))))
                                    (reset! scan-active? true) ;; mark the scan as active
                                    (services/send-who-is-router-to-network nil)
                                    (rd/find-remote-devices) ;; In case of new devices
                                    (scan/scan-and-send)
                                    (log/info
                                     (format "Scan completed in %.2fs"
                                             (some-> @scan/scanning-state :scanning-time-ms (/  1000.0)))))

                                  (catch Exception e
                                    (log/error (str "Network scan exception: "(.getMessage e)))))

                                (when-let [f @after-scan-fn]
                                  (f)
                                  (reset! after-scan-fn nil))
                                (reset! scan-active? nil))))
                          pool
                          :desc "Logging the network"))
              logger-job-atom (atom (logger-job-fn))]
          {:logger logger-job-atom
           :refresh  (ot/every reset-interval
                               #(reset! after-scan-fn
                                        (fn []
                                          (try
                                            (log/info "Restarting local BACnet device")
                                            (init!)
                                            (catch Exception e
                                              (log/error (str "Exception: "(.getMessage e)))))))
                               pool
                               :initial-delay reset-interval
                               :desc "Logging reset every week")})))))

(defn maybe-start-logging
  "If a project-id is configured, start the logging and return true.
  Do nothing otherwise and return nil." []
   (when (:project-id (scan/get-logger-configs))
     (do (start-logging) true)))
