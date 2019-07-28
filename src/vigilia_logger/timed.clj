(ns vigilia-logger.timed
  (:require [vigilia-logger.scan :as scan]
            [overtone.at-at :as ot]
            [bacure.local-device :as ld]
            [bacure.remote-device :as rd]
            [bacure.services :as services]))

(def pool (ot/mk-pool))

(declare start-logging)

(def logging-state
  (atom "Stopped"))

(defn stop-logging []
  (ot/stop-and-reset-pool! pool :strategy :kill)
  (reset! logging-state "Stopped")
  (println "----------------------")
  (println "Vigilia logger stopped")
  (println "----------------------"))

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
   those that should be excluded based on their properties."[]
   (ld/reset-local-device!)
   (rd/discover-network)
   (Thread/sleep 5000) ;; wait 5s
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
    (println "----------------------")
    (println "Vigilia logger started")
    (println "----------------------")
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
                              (println "Previous scan incomplete... skipping this round.")
                              
                              ;; we need to catch exception because
                              ;; we can't interrupt sleeping
                              ;; processes. (This means we might not
                              ;; succeed in restarting the local
                              ;; BACnet device).
                              (do
                                (try
                                  (println "Scanning network...")
                                  (reset! scan-active? true) ;; mark the scan as active
                                  (services/send-who-is-router-to-network nil)
                                  (rd/discover-network) ;; if new devices (or just slow)
                                  (scan/scan-and-send)
                                  (println 
                                   (format "Scan completed in %.2fs"
                                           (some-> @scan/scanning-state :scanning-time-ms (/  1000.0))))
                                  (scan/send-local-logs)
                                  (catch Exception e
                                    (println (str "Exception: "(.getMessage e)))))
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
                                            (println "Restarting local BACnet device")
                                            (init!)
                                            (catch Exception e
                                              (println (str "Exception: "(.getMessage e)))))))
                               pool
                               :initial-delay reset-interval
                               :desc "Logging reset every week")})))))

(defn maybe-start-logging
  "If a project-id is configured, start the logging and return true.
  Do nothing otherwise and return nil." []
   (when (:project-id (scan/get-logger-configs))
     (do (start-logging) true)))
  
