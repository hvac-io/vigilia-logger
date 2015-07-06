(ns vigilia-logger.scan
  (:require [cognitect.transit :as transit]
            [org.httpkit.client :as http]
            [bacure.core :as bac]
            [bacure.remote-device :as rd]
            [bacure.local-device :as ld]
            [bacure.local-save :as local]
            [vigilia-logger.encoding :as encoding]
            [trptcolin.versioneer.core :as version]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as pp])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))




;;; logger ID generation

(def ^{:private true} constituent-chars
  (->> [[\a \z] [\A \Z] [\0 \9]]
       (mapcat (fn [[x y]] (range (int x) (inc (int y)))))
       (map char)
       vec))

(defn- rand-string
  "Generates a random string of [A-z0-9] of length n."
  [n]
  (apply str (repeatedly n #(rand-nth constituent-chars))))

(defn generate-logger-id
  "Generate a logger-id used to know which logger recorded which
  devices. (In the case of projects with multiple networks.)"
  []
  (str "logger-" (rand-string 6)))
  

;;;;;;;;;;;;;;;;;;;;;;;





(def path (str local/path "logger/"))


(defn- remove-nil-in-maps [m]
  (into {} (remove (comp nil? val) m)))


(defn get-logger-configs
  "Get the logger configs form the disk. Remove all the `nil'
  entries."[]
  (try (->> (str path "/configs.edn")
            slurp
            local/safe-read
            remove-nil-in-maps)
       (catch Exception e)))


(defn save-logger-configs!
  "Save data to configs file. Return data."
  [data]
  (let [pp-data (with-out-str (pp/pprint data))]
    (local/mkdir-spit (str path "/configs.edn") pp-data) data))


(defn delete-logger-configs!
  "Delete the logger configs file, if found." []
  (try (io/delete-file (str path "/configs.edn"))
       (catch Exception e)))


(defn new-logger-id!
  "Generate a new logger-id, save it into the config file and return
  it." []
  (let [new-id (generate-logger-id)]
    (-> (get-logger-configs)
        (assoc :logger-id new-id)
        (save-logger-configs!))
    new-id))






;;; Remote server communication

(defn get-api-root
  "Get the api root path. If nothing is found, default to
  https://vigilia.hvac.io/api/v1."
  []
  (-> (get-logger-configs)
      (:api-root)
      (or "https://vigilia.hvac.io/api/v1")))



(defn transit-decode
  [string reader-type]
  (let [in (ByteArrayInputStream. (.getBytes string))
        reader (transit/reader in reader-type)]
    (transit/read reader)))


(defn transit-encode
  ([data writer-type] (transit-encode data writer-type nil))
  ([data writer-type options]
   (let [out (ByteArrayOutputStream. 4096)
         writer (transit/writer out :json options)]
     ;; Write data to a stream
     (transit/write writer data)
     ;; get the encoded data
     (.toString out))))



(defn http-error? [response]
  (or (:error response)
      (not (some #{(:status response)} [200 201 202 203
                                   204 205 206 207
                                   208 226]))))

(defn can-connect?
  "True if we can reach the specified api-root"
  [api-root]
  (let [response @(http/request 
                   {:url api-root
                    :method :get
                    :as :text})]    
  (when-not 
      (http-error? response)
    true)))



(defn send-transit-request
  "Send a request with the necessary transit headers.
  Convert the data received back into edn."
  [url & [opts]]
  (let [req-config (merge {:url url
                           ;:method :get ;; default method
                           ;:as :text
                           :headers {"Content-Type" "application/transit+json" 
                                     "Accept" "application/transit+json"}}
                          (first opts))
        response @(http/request req-config)]
    (if (and (not (http-error? response))
             (re-find #"application/transit\+json"
                      (get-in response [:headers :content-type])))
      (update-in response [:body] transit-decode :json)
      response)))


(defn get-logger-api-path
  "Given the root API path and a project-id, query the API to find out
  what is the logger path and return it."
  [api-root project-id]
  (some-> (str api-root "/project/" project-id)
          (send-transit-request)
          (get-in [:body :logging :href])))


(defn get-project-logger-data
  "Return various logger data from the project API, or nil in case of
  http error (most probably 403: forbidden) or bad project."
  [api-root-path project-id key]
  (when-let [api-path (get-logger-api-path api-root-path project-id)]
    (let [response
          (-> api-path
              (send-transit-request {:query-params {:key key}}))]
      (when-not (http-error? response)
        (:body response)))))


(defn credentials-valid?
  "Load the current configs and try to connect to the Vigilia
  server."[]
  (let [configs (get-logger-configs)
        {:keys [project-id logger-key api-root]} configs]
    (when (get-project-logger-data 
           api-root project-id logger-key)
      true)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def logger-version
  "The logger version used to check what data encoding is used."
  (str "vigilia-logger-" (version/get-version "io.hvac.vigilia.logger" "vigilia-logger")))



(def last-scan-timestamp (atom nil))


;;; section for 'advanced' filtering

(defn filter-device
  "Test the criteria maps agains a device ID and return :remove if any
  succeed, otherwise :keep. If the extended information is not yet
  available, simply return nil." [id criteria-coll]
  (try
    (do (rd/extended-information id)
        (when (rd/extended-information? id)
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
                                 :else :keep))))))))
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
     (if-let [criteria-coll (:criteria-coll (get-logger-configs))]
       (get (swap! remove-device-table #(->> criteria-coll
                                             (filter-device id)
                                             (assoc % id))) id)
       :keep)));;if no criteria-coll, automatically keep

(defn reset-devices-to-remove-table! []
   (reset! remove-device-table {})
   (pmap remove-device? (rd/remote-devices)))
   



(defn find-id-to-scan
  "Check all the different filtering options and return a list of
   device-id to scan." []
   (let [{:keys [max-range min-range id-to-remove id-to-keep]} (get-logger-configs)
         id-to-keep-fn (fn [x] (if id-to-keep (clojure.set/intersection (into #{} id-to-keep) x) x))
         id-to-remove-fn (fn [x] (clojure.set/difference x (into #{} id-to-remove)))
         remove-device (fn [x] (filter #(= :keep (remove-device? %)) x))
         min-fn (fn [x] (if min-range (filter #(> % min-range) x) x))
         max-fn (fn [x] (if max-range (filter #(< % max-range) x) x))]
     ;; and now just keep the remote devices for which we have extended information
     (-> (into #{} (filter rd/extended-information? (rd/remote-devices)))
         id-to-keep-fn
         id-to-remove-fn
         min-fn
         max-fn
         remove-device ;; <-- last so we can avoid querying remote
                       ;; devices if we already know we don't want to
                       ;; keep them.
         ))) 


(def last-scan-duration (atom nil))

(defn scan-network
  "Scan the network and return a `snapshot' for logging purposes."[]
  (let [read-object-delay (:object-delay (get-logger-configs))]
    (reset! last-scan-timestamp (encoding/iso-8601-timestamp)) ;; for debugging
    (->> (find-id-to-scan)
         (pmap ;;use the power of parallelization!
          (fn [device-id] 
            (encoding/scan-device device-id read-object-delay)))
         (apply merge))))


(defn find-unsent-logs []
  (let [filename-list (seq (.listFiles (clojure.java.io/file
                                        path)))]
    (->> filename-list
         (map #(.getName %))
         (filter #(re-find #"vigilia.*\.log" %))
         (sort))))



(defn send-logs
  "Send the logs to the remote server. NIL if successful. In case of
  error, return the response."
  [{:keys [api-path project-id logger-id logger-version key]} logs]
  (let [response @(http/request 
                   {:url api-path
                    :method :post
                    :as :text
                    :headers {"content-type" "application/transit+json"}
                    :body (transit-encode
                           {:key key
                            :logger-id logger-id
                            :logger-version logger-version
                            :logs logs}
                            :json)})]
    (when (http-error? response)
      response)))


(defn send-to-remote-server
  "Send the data to remote servers. Return NIL if successful."
  [data]
  (let [configs (get-logger-configs)
        {:keys [project-id key api-root
                logger-id]} configs
        project-logger-data (get-project-logger-data 
                             api-root project-id 
                             key)]
  ;; Check if server intend to accept our logs before sending them
    (if (:logging-allowed? project-logger-data)
      (send-logs {:api-path (:href project-logger-data)
                  :project-id project-id
                  :logger-id (or logger-id (new-logger-id!))
                  :logger-version logger-version
                  :key key}
                 data)
      :logging-not-allowed)))


(defn scan-and-send
  "Scan the network and send the result to remote servers. If the
  server can't be reached, save the result in a
  \"vigilia-<timestamp>\".log file. Only saves up to 500 logs." []
   (let [start-time (encoding/timestamp)
         spit-file-fn (partial local/mkdir-spit
                               (str path "vigilia-" start-time ".log"))
         data (scan-network)]
     ;; try to send to server
     (when (send-to-remote-server data) ;; nil on success
       ;; if it doesn't work, save data locally.
       (when (> 500 (count (find-unsent-logs)))
         (spit-file-fn data)))
     (reset! last-scan-duration (- (encoding/timestamp) start-time))))


(defn send-local-logs
  "Check in the logger path for any unsent logs. If the server can't
   be reached (or simply refuses logs), keep them locally." []
   (let [logs (find-unsent-logs)]
     (when (seq logs)
       (println (format "Found %d local logs..." (count logs)))
       (loop [[log-name & rest-logs] logs]
         (if log-name
           (do (println (str "sending " log-name "..."))
               (let [error? (send-to-remote-server 
                             (edn/read-string (slurp (str path log-name))))]          
                 (if-not error?
                   (do (clojure.java.io/delete-file log-name)
                       (println "Sent.")
                       (recur rest-logs))
                   (println "Remote server can't be reached or refused the log."))))
           (println "No more local logs to send."))))))
