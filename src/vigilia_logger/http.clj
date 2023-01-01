(ns vigilia-logger.http
  (:require [clj-http.client :as http-client]
            [cognitect.transit :as transit]
            [trptcolin.versioneer.core :as version]
            [vigilia-logger.configs :as configs])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn transit-decode
  ([string] (transit-decode string :json))
  ([string reader-type]
   (let [in (ByteArrayInputStream. (.getBytes string "UTF-8"))
         reader (transit/reader in reader-type)]
     (transit/read reader))))

(defn transit-encode
  ([data] (transit-encode data :json))
  ([data writer-type] (transit-encode data writer-type nil))
  ([data writer-type options]
   (let [out (ByteArrayOutputStream. 4096)
         writer (transit/writer out writer-type options)]
     ;; Write data to a stream
     (transit/write writer data)
     ;; get the encoded data
     (.toString out "UTF-8"))))

(defn error?
  "True if response has :error, or if the http status is not one of the
  successful ones (200 ... 226).
  "[response]
  (let [successful-statuses #{200 201 202 203
                              204 205 206 207
                              208 226}]
    (or (:error response)
        (not (successful-statuses (:status response))))))

(defn- transit-request
  "Wrap `http-client/request` to automatically encode/decode transit when applicable."
  [req]
  (let [req-config (merge {:headers {"Content-Type" "application/transit+json"
                                     "Accept"       "application/transit+json"}}
                          req)
        response (-> req-config
                     (update :body #(when % (transit-encode %)))
                     (http-client/request))]
    (if (and (not (or (error? response)
                      (empty? (:body response))))
             (some->> (get-in response [:headers :content-type])
                      (re-find #"application/transit\+json")))
      (update-in response [:body] transit-decode)
      response)))

(defn with-proxy-configs
  "Insert the configured proxy information into the request map."
  [req]
  (-> (configs/fetch)
      (select-keys [:proxy-host :proxy-port :proxy-ignore-hosts :proxy-user :proxy-pass])
      (merge req)))

(defn request
  "Similar to `http-client/request`.
  - Doesn't throw by default;
  - Automatically includes proxy configs;
  - :get request by default.

  Adding :transit? will automatically handle transit encoding/decoding."
  [req]
  (let [req-f (if (:transit? req) transit-request http-client/request)]
    (req-f (-> {:method           :get
                :throw-exceptions false}
               (with-proxy-configs)
               (merge req)))))

(defn can-connect?
  "True if we can reach the specified url."
  [url]
  (try
    (not (error? (request {:url url})))
    (catch java.net.ConnectException _)))

(defn- fetch-logger-api-path
  "Given the root API path and a project-id, query the API to find out
  what is the logger path and return it."
  [api-root project-id]
  (some-> (request {:url (str api-root "/project/" project-id)
                    :transit? true})
          (get-in [:body :logging :href])))

(defn fetch-project-logger-data
  "Return various logger data from the project API, or nil in case of
  http error (most probably 403: forbidden) or bad project."
  ([project-id logger-key]
   (fetch-project-logger-data (configs/api-root) project-id logger-key))
  ([api-root-path project-id logger-key]
   (when-let [api-path (fetch-logger-api-path api-root-path project-id)]
     (let [response (request {:query-params {:logger-key logger-key}
                              :url          api-path
                              :transit? true})]
       (when-not (error? response)
         (:body response))))))


(defn credentials-valid?
  "True if credentials can connect to Vigilia server.
  Any http error (including 403) will return false."
  ([] (let [configs (configs/fetch)]
        (credentials-valid? (:project-id configs) (:logger-key configs))))
  ([project-id logger-key]
   (if (fetch-project-logger-data (configs/api-root) project-id logger-key)
     true
     false)))

(def logger-version
  "The logger version used to check what data encoding is used."
  (let [artifact "vigilia-logger"]
    (str artifact "-" (version/get-version "io.hvac.vigilia.logger" artifact))))

(defn send-log
  "Send the log to remote server using the saved configurations.
  Return NIL if successful.
  Catch all exceptions"
  [log]
  (try
    (let [{:keys [project-id logger-key]} (configs/fetch)
          {:keys [logging-allowed? href]} (fetch-project-logger-data project-id logger-key)]
      ;; Check if server intends to accept our scan
      (if-not logging-allowed?
        :logging-not-allowed
        (let [response (request {:url      href
                                 :method   :post
                                 :as       :text
                                 :body     {:logger-key     logger-key
                                            :logger-id      (configs/get-logger-id!)
                                            :logger-version logger-version
                                            ; For historical reason the
                                            ; API expects :logs, but it
                                            ; makes more sense to call it
                                            ; :log (singular) in this
                                            ; codebase.
                                            :logs           log}
                                 :transit? true})]
          (when (error? response)
            response))))
    (catch Exception e
      {:exception e})))
