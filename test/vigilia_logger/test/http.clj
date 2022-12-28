(ns vigilia-logger.test.http
  (:require [clojure.test :refer :all]
            [vigilia-logger.http :as http]
            [vigilia-logger.test.util :as u]))

(deftest roundtrip-transit
  (let [data {:a 1, :b "text", :c {:d :nested}}]
    (is (= data
           (-> (http/transit-encode data)
               (http/transit-decode))))))

(deftest transit-request
  (let [transit-echo-handler
        (fn [req]
          (let [body (some-> (:body req) slurp)]
            {:body    (http/transit-encode
                       {:decoded (some-> body http/transit-decode)
                        :raw     body})
             :headers {"Content-Type" "application/transit+json"}}))]
    (u/with-server transit-echo
      (testing "Transit body"
        (let [data {:a 1}
              resp (http/request {:body data
                                  :url  (u/url "/any")})]
          (is (= data (-> resp :body :decoded)))))
      (testing "No body"
        (let [resp (http/request {:url (u/url "/any")})]
          (is (= nil
                 (-> resp :body :decoded)
                 (-> resp :body :raw))))))
    (testing "Not a transit response"
      (u/with-server (fn [req] {:status 200, :body "Hello!"})
        (let [resp (http/request {:url (u/url "/any")})]
          (is (= "Hello!" (:body resp))))))))

(deftest can-connect?
  (u/with-server (fn [req] (condp = (:uri req)
                             "/yes" {:status 200}
                             "/no" {:status 404}))
    (is (http/can-connect? (u/url "/yes")))
    (is (not (http/can-connect? (u/url "/no"))))
    (is (not (http/can-connect? "http://localhost:2122")) ; <-- wrong port
        "Do not throw on 'connection refused'")))


(defn make-handler
  "Will convert response to transit if the metadata :transit? is true.

  {\"some-path\" {:get <response>}}"
  [routes-map]
  (fn [req]
    (if-let [response (get-in routes-map [(:uri req) (:request-method req)])]
      (if (:transit? (meta response))
        {:body    (http/transit-encode response)
         :headers {"content-type" "application/transit+json"}
         :status  200}
        {:body   response
         :status 200}))))

(defn simple-vigilia-handler
  "Simulate the Vigilia API.
  - Project root: return a link to the logging endpoint.
  - Logging endpoint:
     - GET: Returns map of :logging-allowed? and :href
     - POST: accepts logs"
  [req]
  (let [project-path (u/path :project u/project-id)
        logging-path (u/path :project u/project-id :logging)
        handler (make-handler {project-path {:get ^:transit? {:logging {:href (u/url logging-path)}}}
                               logging-path {:get  ^:transit? {:href             (u/url logging-path)
                                                               :logging-allowed? true}
                                             :post "logs accepted"}})]
    (handler req)))

(deftest get-project-logger-data
  (u/with-test-configs
    (u/with-server simple-vigilia-handler
      (is (= {:logging-allowed? true
              :href             (u/url :project u/project-id :logging)}
             (http/get-project-logger-data u/project-id u/logger-key))))
    (u/with-server (fn [req] {:status 404})
      (is (nil? (http/get-project-logger-data u/project-id u/logger-key))
          "Returns nil on error"))))

(deftest credentials-valid?
  (u/with-test-configs
    (u/with-server simple-vigilia-handler
      (is (true? (http/credentials-valid?))))
    (u/with-server (fn [req] {:status 403})
      (is (false? (http/credentials-valid?))))
    (u/with-server (fn [req] {:status 500})
      (is (false? (http/credentials-valid?))))))
