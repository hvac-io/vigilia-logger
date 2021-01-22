(ns vigilia-logger.test.proxy
  (:require [clj-http.client :as http]
            [clojure.test :refer :all]
            [compojure.core :refer [defroutes GET]]
            [org.httpkit.server :refer [run-server]]
            [ring.adapter.jetty :refer [run-jetty]]
            ring.middleware.basic-authentication
            [ring.middleware.defaults :as defaults]
            [vigilia-logger.scan :as scan]
            [vigilia-logger.test.util :as u]))

;; from the http-kit test directory
(def ^:private not-found-response {:status 404
                                   :body   "{\"message\":\"Not found\"}"})

(defn- build-url [host path query-string]
  (let [url (.toString (java.net.URL. (java.net.URL. host) path))]
    (if (not-empty query-string)
      (str url "?" query-string)
      url)))

(defn- handle-not-found [request]
  not-found-response)

(defn- create-proxy-fn [handler opts]
  (let [identifier-fn  (get opts :identifier-fn identity)
        server-mapping (get opts :host-fn {})
        insecure       (get opts :allow-insecure-ssl false)]
    (fn [request]
      (let [request-key      (identifier-fn request)
            host             (server-mapping request-key)
            stripped-headers (dissoc (:headers request) "content-length")]
        (if host
          (select-keys (http/request {:url              (build-url host (:uri request) (:query-string request))
                                      :method           (:request-method request)
                                      :body             (:body request)
                                      :headers          stripped-headers
                                      :throw-exceptions false
                                      :as               :stream
                                      :insecure?        insecure})
                       [:status :headers :body])
          (handler request))))))

(defn proxy-request
  ([opts]
   (proxy-request handle-not-found opts))
  ([handler opts]
   (create-proxy-fn handler opts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defroutes test-routes
  (GET "/get" [] "hello world")
  (GET "/get/project/test" [] {:logging {:href "/get/project/test/logging"}})
  (GET "/get/project/test/logging" [] "logging"))

(defn authenticated? [name pass]
  (and (= name "me")
       (= pass "pass")))

(defn proxy-authentication-required
  "407 Proxy Authentication Required (ClientError)
  Proxy authentication is required to access the requested resource."
  ([] (proxy-authentication-required nil))
  ([body]
   {:status  407
    :headers {"Proxy-Authenticate" "Basic"}
    :body    body}))

(defn wrap-proxy-auth
  [handler]
  (fn [req]
    (def ppp req)
    (if-some [p-auth (get-in req [:headers "proxy-authorization"])] ;; hack to reuse existing middleware function
      (if (-> (assoc-in req [:headers "authorization"] p-auth)
              (ring.middleware.basic-authentication/basic-authentication-request authenticated?)
              (:basic-authentication))
        (handler req)
        {:status 401
         :body   "access denied"})
      (proxy-authentication-required))))

(def proxy-handler (proxy-request {:identifer-fn       :server-name
                                   :allow-insecure-ssl true
                                   :host-fn            (fn [{:keys [remote-addr server-port scheme] :as opts}]
                                                         (cond
                                                           (= 4347 server-port)
                                                           (str "http://" remote-addr ":" server-port)
                                                           (= 9898 server-port)
                                                           (str "https://" remote-addr ":" server-port)))}))

(defn fixtures
  [f]
  (let [server            (run-server
                           (defaults/wrap-defaults test-routes defaults/site-defaults)
                           {:port 4347})
        ssl-server        (run-jetty
                           (defaults/wrap-defaults test-routes defaults/site-defaults)
                           {:port         14347    :join?    false :ssl-port 9898 :ssl? true :http? false
                            :key-password "123456" :keystore "test/ssl_keystore"})
        ssl-proxy-server  (run-jetty
                           proxy-handler
                           {:port         14348    :join?    false :ssl-port 9899 :ssl? true :http? false
                            :key-password "123456" :keystore "test/ssl_keystore"})
        proxy-server      (run-server
                           proxy-handler
                           {:port 4348 :join? false :ssl? false})
        auth-proxy-server (run-server
                           (-> proxy-handler
                               (wrap-proxy-auth))
                           {:port 4349})]
    (try (f)
         ; Close all servers
         (finally
           (server)
           (proxy-server)
           (auth-proxy-server)
           (.stop ssl-server)
           (.stop ssl-proxy-server)))))

(use-fixtures :once fixtures)

(def proxy-auth-data {:proxy-user "me" :proxy-pass "pass"})



(deftest test-control
  (testing "no proxy, for sanity checking"
    (let [resp (http/get "http://127.0.0.1:4347/get")]
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp)))))
  (testing "no proxy + ssl, for sanity checking"
    (let [resp (http/get "https://127.0.0.1:9898/get" {:insecure? true})]
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp))))))


;;;;;;;;;;;;;;;


(deftest proxy-nonexistent
  (testing "test call nonexistent proxy and fail"
    (is (thrown-with-msg? java.net.ConnectException
                          #"Connection refused"
                          (http/get "http://127.0.0.1:4347/get"
                                    {:proxy-host         "127.0.0.1"
                                     :proxy-port         4346
                                     :proxy-ignore-hosts #{}})))))

(deftest http-to-http-proxy
  (testing "test call proxy successfully"
    (let [{:keys [status body]} (http/get "http://127.0.0.1:4347/get"
                                          {:proxy-host         "127.0.0.1"
                                           :proxy-port         4348
                                           :proxy-ignore-hosts #{}})]
      (is (= 200 status))
      (is (= "hello world" body)))))


(deftest configs
  (u/with-test-configs
    (testing "Various configs operations"
      (let [test-config-map {:time-interval 2}]
        (scan/save-logger-configs! test-config-map)
        (is (= test-config-map (scan/get-logger-configs)))))
    (testing "Proxy config"
      (let [m         {:proxy-host "127.0.0.1" :proxy-port 3030}
            m2        (merge m proxy-auth-data)
            m2-result (merge m m2)]
        (is (= m (scan/with-proxy-configs m)))
        (is (= m2-result (scan/with-proxy-configs m2)))))))

(deftest config-proxy-connection
  (testing "Simple proxy"
    (let [resp (http/get "http://localhost:4347/get"
                         (scan/with-proxy-configs
                           (merge proxy-auth-data
                                  {:proxy-host         "127.0.0.1"
                                   :proxy-port         4348
                                   :proxy-ignore-hosts #{}})))]
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp)))))
  (testing "proxy with authentication"
    (let [resp (http/get "http://localhost:4347/get"
                         (scan/with-proxy-configs
                           (merge proxy-auth-data
                                  {:proxy-host         "127.0.0.1"
                                   :proxy-port         4349
                                   :proxy-ignore-hosts #{}})))]    
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp)))))
  (u/with-test-configs
    (scan/save-logger-configs! (merge proxy-auth-data
                                      {:proxy-host         "127.0.0.1"
                                       :proxy-port         4349
                                       :proxy-ignore-hosts #{}}))
    (testing "proxy with authentication and saved configs"  
      (let [resp (http/get "http://localhost:4347/get"
                           (scan/with-proxy-configs {}))]
        (is (= 200 (:status resp)))
        (is (= "hello world" (:body resp)))))
    (testing "Various request functions"
      (is (scan/can-connect? "http://localhost:4347/get"))
      (is (scan/send-transit-request "http://localhost:4347/get"))
      ;; now we enter the wrong proxy password and test if we fail to connect
      (scan/save-logger-configs! (merge {:proxy-user "me" :proxy-pass "wrong"}
                                        {:proxy-host         "127.0.0.1"
                                         :proxy-port         4349
                                         :proxy-ignore-hosts #{}}))
      (is (not (scan/can-connect? "http://localhost:4347/get")))
      (is (= "access denied" (:body (scan/send-transit-request "http://localhost:4347/get")))))))


(comment
  (fixtures
   (fn []
     (u/with-test-configs
       ;; now we enter the wrong proxy password and test if we fail to connect
       (scan/save-logger-configs! (merge {:proxy-user "me" :proxy-pass "wrong"}
                                         {:proxy-host         "127.0.0.1"
                                          :proxy-port         4349
                                          :proxy-ignore-hosts #{}}))
       (scan/can-connect? "http://localhost:4347/get")
       (scan/send-transit-request "http://localhost:4347/get")))))
