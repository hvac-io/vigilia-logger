(ns vigilia-logger.test.http
  (:require [clojure.test :refer :all]
            [vigilia-logger.http :as http]
            [vigilia-logger.test.util :as u]))

(deftest roundtrip-transit
  (let [data {:a 1, :b "text", :c {:d :nested}}]
    (is (= data
           (-> (http/transit-encode data)
               (http/transit-decode))))))

(defn- transit-echo
  [req]
  (let [body (some-> (:body req) slurp)]
    {:body    (http/transit-encode
               {:decoded (some-> body http/transit-decode)
                :raw     body})
     :headers {"Content-Type" "application/transit+json"}}))

(deftest transit-request
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
        (is (= "Hello!" (:body resp)))))))

(deftest can-connect?
  (u/with-server (fn [req] (condp = (:uri req)
                             "/yes" {:status 200}
                             "/no" {:status 404}))
    (is (http/can-connect? (u/url "/yes")))
    (is (not (http/can-connect? (u/url "/no"))))
    (is (not (http/can-connect? "http://localhost:2122"))
        "Do not throw on 'connection refused'")))
