(ns vigilia-logger.scan
  (:require [cognitect.transit :as transit]
            [org.httpkit.client :as http]
            [bacure.core :as bac]
            [bacure.remote-device :as rd]
            [bacure.local-save :as local]
            [vigilia-logger.encoding :as encoding]
            [trptcolin.versioneer.core :as version]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clj-time.core :as time]
            [clojure.data.codec.base64 :as b64])
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
  (let [pp-data (pr-str data)]
    (local/mkdir-spit (str path "/configs.edn") pp-data) data))


(defn delete-logger-configs!
  "Delete the logger configs file, if found." []
  (try (io/delete-file (str path "/configs.edn"))
       (catch Exception e)))


(defn- new-logger-id!
  "Generate a new logger-id, save it into the config file and return
  it."
  [logger-configs]
  (let [new-id (generate-logger-id)]
    (-> logger-configs
        (assoc :logger-id new-id)
        (save-logger-configs!))
    new-id))

(defn get-logger-id!
  "Get the existing logger id, or generate one and save it before
  returning it."
  []
  (let [configs (get-logger-configs)]
    (or (:logger-id configs)
        (new-logger-id! configs))))

(defn get-logs-path
  "Return the location where logs are saved."
  []
  (or (:logs-path (get-logger-configs))
      path))

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
  (let [in (ByteArrayInputStream. (.getBytes string "UTF-8"))
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
     (.toString out "UTF-8"))))



(defn http-error? [response]
  (or (:error response)
      (not (some #{(:status response)} [200 201 202 203
                                   204 205 206 207
                                   208 226]))))



(defn auth-header [user pass]
  (str "Basic " (String. ^bytes (b64/encode (.getBytes (str user ":" pass))))))


(defmacro mapify
  "Given some symbols, construct a map with the symbols as keys, and
  the value of the symbols as the map values. For example:
 (Let [aa 12]
     (mapify aa))
 => {:aa 12}"
  [& symbols]
  `(into {}
         (filter second
                 ~(into []
                        (for [item symbols]
                          [(keyword item) item])))))

(defn proxy-configs
  ([] (proxy-configs (get-logger-configs)))
  ([configs]
   (let [{:keys [proxy-host proxy-port proxy-user proxy-password] :as proxy}
         (select-keys configs [:proxy-host :proxy-port :proxy-user :proxy-password])]
     (when (and proxy-host proxy-port)
       (merge (mapify proxy-host proxy-port)
              (when (and proxy-user proxy-password)
                {"Authorization" (auth-header proxy-user proxy-password)}))))))

(defn with-proxy-configs
  "Insert the configured proxy information into the request map."
  ([request-map] (with-proxy-configs request-map (get-logger-configs)))
  ([request-map configs]
   (let [pc (proxy-configs configs)
         host-map (select-keys pc [:proxy-host :proxy-port])
         auth (select-keys pc ["Authorization"])]
     (-> request-map
         (merge host-map)
         (update-in [:headers] merge auth)))))

(defn can-connect?
  "True if we can reach the specified api-root"
  [api-root]
  (let [response @(http/request 
                   (with-proxy-configs 
                     {:url api-root
                      :method :get
                      :as :text}))]
  (when-not 
      (http-error? response)
    true)))



(defn send-transit-request
  "Send a request with the necessary transit headers.
  Convert the data received back into edn."
  [url & [opts]]
  (let [req-config (with-proxy-configs
                     (merge {:url url
                                        ;:method :get ;; default method
                                        ;:as :text
                             :headers {"Content-Type" "application/transit+json" 
                                       "Accept" "application/transit+json"}}
                            (first opts)))
        response @(http/request req-config)]
    (if (and (not (or (http-error? response)
                      (empty? (:body response))))
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
              (send-transit-request {:query-params {:logger-key key}}))]
      (if-not (http-error? response)
        (:body response)
        (print response)))))


(defn credentials-valid?
  "Load the current configs and try to connect to the Vigilia
  server."
  ([] (let [configs (get-logger-configs)]
        (credentials-valid? (:project-id configs) (:logger-key configs))))
  ([project-id logger-key]
   (when (get-project-logger-data 
          (get-api-root) project-id logger-key)
     true)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
     (if-let [criteria-coll (:criteria-coll (get-logger-configs))]
       (get (swap! remove-device-table #(->> criteria-coll
                                             (filter-device id)
                                             (assoc % id))) id)
       :keep)));;if no criteria-coll, automatically keep

(defn reset-devices-to-remove-table! []
   (reset! remove-device-table {})
   (pmap remove-device? (rd/remote-devices)))
   

(defn devices-with-extended-info []
  ;; we catch errors because if the router for a given device is not
  ;; found, it will throw an exception.
  (filter (fn [id] (try (rd/extended-information id)
                        (catch Exception e)))
          (rd/remote-devices)))

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
     (-> (into #{} (devices-with-extended-info))
         id-to-keep-fn
         id-to-remove-fn
         min-fn
         max-fn
         remove-device ;; <-- last so we can avoid querying remote
                       ;; devices if we already know we don't want to
                       ;; keep them.
         )))




;;;;;;;;;;

(defonce scanning-state
  (atom {:ids-to-scan #{}
         :ids-scanning #{}
         :ids-scanned #{}
         :start-time (time/now)
         :end-time (time/now)
         :scanning-time-ms 0
         :completed-scans 0}))

(defn mark-as-scanning! [device-id]
  (swap! scanning-state update-in [:ids-scanning] conj device-id))

(defn mark-as-scanned! [device-id]
  (let [ss @scanning-state]
    (reset! scanning-state 
            (-> (update-in ss [:ids-scanning] disj device-id)
                (update-in [:ids-scanned] conj device-id)))))

(defn mark-start-of-scan! [ids-to-scan]
  (swap! scanning-state 
         assoc
         :scanning? true
         :start-time (time/now)
         :ids-to-scan (set ids-to-scan)))

(defn mark-end-of-scan! []
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

(defn interleave-all
  "interleaves including remainder of longer seqs."
  [& seqs]
  (if (not-empty (first seqs))
    (cons (first (first seqs))
          (lazy-seq (apply interleave-all
                           (filter not-empty
                                   (concat (rest seqs) [(rest (first seqs))])))))))

(defn reorder-ids
  "Try to avoid consecutive ids" [ids]
  (let [proc-qty (.availableProcessors (Runtime/getRuntime))
        ids-qty (count ids)
        pt (-> (/ ids-qty (+ 2 proc-qty))
               (int)
               (max 1)
               (partition-all ids))]
    (apply interleave-all pt)))

;;;;;;;


(defn scan-network
  "Scan the network and return a `snapshot' for logging purposes."[]
  (let [configs (get-logger-configs)
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
    (let [scan-result (doall (pmap scan-fn ids-to-scan))]
      (mark-end-of-scan!)
      (apply merge scan-result))))


(defn find-unsent-logs []
  (let [filename-list (seq (.listFiles (io/file (get-logs-path))))]
    (->> filename-list
         (map #(.getName %))
         (filter #(re-find #"vigilia.*\.log" %))
         (sort))))



(defn send-logs
  "Send the logs to the remote server. NIL if successful. In case of
  error, return the response."
  [{:keys [api-path project-id logger-id logger-version logger-key]} logs]
  (let [response @(http/request 
                   (with-proxy-configs
                     {:url api-path
                      :method :post
                      :as :text
                      :headers {"content-type" "application/transit+json"}
                      :body (transit-encode
                             {:logger-key logger-key
                              :logger-id logger-id
                              :logger-version logger-version
                              :logs logs}
                             :json)}))]
    (when (http-error? response)
      response)))


(defn send-to-remote-server
  "Send the data to remote servers. Return NIL if successful."
  [data]
  (let [configs (get-logger-configs)
        {:keys [project-id logger-key
                logger-id]} configs
        project-logger-data (get-project-logger-data 
                             (get-api-root) project-id 
                             logger-key)]
  ;; Check if server intend to accept our logs before sending them
    (if (:logging-allowed? project-logger-data)
      (send-logs {:api-path (:href project-logger-data)
                  :project-id project-id
                  :logger-id (get-logger-id!)
                  :logger-version logger-version
                  :logger-key logger-key}
                 data)
      :logging-not-allowed)))


(defn scan-and-send
  "Scan the network and send the result to remote servers. If the server
  can't be reached, save the result in a .log file. Only saves up to
  2016 logs."
  []
  (let [start-time   (encoding/timestamp)
        logs-path (get-logs-path)
        spit-file-fn (partial local/mkdir-spit
                              (str logs-path "vigilia-" (get-logger-id!) "-" start-time ".log"))
        data         (scan-network)]
    ;; try to send to server
    (when (send-to-remote-server data) ;; nil on success
      ;; if it doesn't work, save data locally.
      ;(print (send-to-remote-server data))
      (when (> 2016 (count (find-unsent-logs))) ;; ~2 weeks
        (spit-file-fn data)))))


(defn send-local-logs
  "Check in the logger path for any unsent logs. If the server can't
   be reached (or simply refuses logs), keep them locally." []
  (let [logs (find-unsent-logs)
        logs-path (get-logs-path)]
     (when (seq logs)
       (println (format "Found %d local logs..." (count logs)))
       (loop [[log-name & rest-logs] logs]
         (if log-name
           (do (println (str "sending " log-name "..."))
               (let [error? (send-to-remote-server 
                             (edn/read-string (slurp (str path log-name))))]          
                 (if-not error?
                   (do (clojure.java.io/delete-file (str path log-name))
                       (println "Sent.")
                       (recur rest-logs))
                   (do (println "Remote server can't be reached or refused the log.")
                       (println (str "Http status code : " (:status error?)))
                       (println (str "Body : " (:body error?)))))))
           (println "No more local logs to send."))))))
