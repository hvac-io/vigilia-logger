(ns vigilia-logger.test.proxy
  (:use clojure.test)
  (:require [vigilia-logger.scan :as scan]
            [compojure.core :refer [GET defroutes]]
            [org.httpkit.server :refer [run-server]]
            [org.httpkit.client :as http]
            [ring-request-proxy.core :as proxy]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.defaults :as defaults]
            [ring.middleware.basic-authentication :refer [wrap-basic-authentication]]            
            [clojure.java.io :as io]
            [clj-http.client :as clj-http]))


;; from the http-kit test directory
(def ^:private not-found-response {:status 404
                                   :body "{\"message\":\"Not found\"}"})

(defn- build-url [host path query-string]
  (let [url (.toString (java.net.URL. (java.net.URL. host) path))]
    (if (not-empty query-string)
      (str url "?" query-string)
      url)))

(defn- handle-not-found [request]
  not-found-response)

(defn- create-proxy-fn [handler opts]
  (let [identifier-fn (get opts :identifier-fn identity)
        server-mapping (get opts :host-fn {})
        insecure (get opts :allow-insecure-ssl false)]
    (fn [request]
      (let [request-key (identifier-fn request)
            host (server-mapping request-key)
            stripped-headers (dissoc (:headers request) "content-length")]
        (if host
          (select-keys (clj-http/request {:url              (build-url host (:uri request) (:query-string request))
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

(def proxy-handler (proxy-request {:identifer-fn :server-name
                                   :allow-insecure-ssl true
                                   :host-fn (fn [{:keys [remote-addr server-port scheme] :as opts}]
                                              (cond
                                                (= 4347 server-port)
                                                (str "http://" remote-addr ":" server-port)
                                                (= 9898 server-port)
                                                (str "https://" remote-addr ":" server-port)))}))

(use-fixtures :once
  (fn [f]
    (let [server (run-server
                  (defaults/wrap-defaults test-routes defaults/site-defaults)
                  {:port 4347})
          ssl-server (run-jetty
                      (defaults/wrap-defaults test-routes defaults/site-defaults)
                      {:port 14347 :join? false :ssl-port 9898 :ssl? true :http? false
                       :key-password "123456" :keystore "test/ssl_keystore"})
          ssl-proxy-server (run-jetty
                            proxy-handler
                            {:port 14348 :join? false :ssl-port 9899 :ssl? true :http? false
                             :key-password "123456" :keystore "test/ssl_keystore"})
          proxy-server (run-server
                        proxy-handler
                        {:port 4348 :join? false :ssl? false})
          auth-proxy-server (run-server
                             (-> proxy-handler
                                 (wrap-basic-authentication authenticated?))
                             {:port 4349})]
      (try (f) (finally (server) (proxy-server) (auth-proxy-server)
                        (.stop ssl-server) (.stop ssl-proxy-server))))))

(def proxy-auth-data {:proxy-user "me" :proxy-password "pass"})



(deftest test-control
  (testing "no proxy, for sanity checking"
    (let [resp @(http/get "http://127.0.0.1:4347/get")]
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp)))))
  (testing "no proxy + ssl, for sanity checking"
    (let [resp @(http/get "https://127.0.0.1:9898/get" {:insecure? true})]
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp))))))


;;;;;;;;;;;;;;;


(defn delete-recursively! [fname]
  (let [func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (io/delete-file f))]
    (func func (io/file fname))))

(defmacro with-test-configs [& body]
  `(with-redefs [scan/path "test-path/"]
     ;; save an initial config file
     (scan/save-logger-configs! {:time-interval 1
                                 :api-root "http://localhost:4347/get"
                                 :project-id 12
                                 :logger-key 12})
     ~@body
     (delete-recursively! scan/path)))

(deftest proxy-nonexistent
  (testing "test call nonexistent proxy and fail"
    (let [{:keys [error]} @(http/get "http://127.0.0.1:4347/get"
                                     {:proxy-host "http://127.0.0.1"
                                      :proxy-port 4346})]
      (is (= (type (java.net.ConnectException.)) (type error)))
      (is (= "Connection refused"
             (.getMessage ^java.net.ConnectException error))))))

(deftest http-to-http-proxy
  (testing "test call proxy successfully"
    (let [{:keys [status body]} @(http/get "http://127.0.0.1:4347/get"
                                           {:proxy-host "http://127.0.0.1"
                                            :proxy-port 4348})]
      (is (= 200 status))
      (is (= "hello world" body)))))


(deftest configs
  (with-test-configs
    (testing "Various configs operations"
      (let [test-config-map {:time-interval 2}]
        (scan/save-logger-configs! test-config-map)
        (is (= test-config-map (scan/get-logger-configs)))))
    (testing "Proxy config"
      (let [m {:proxy-host "http://127.0.0.1" :proxy-port 3030}
            m2 (merge m proxy-auth-data)
            m2-result (merge m {"Authorization" "Basic bWU6cGFzcw=="})]
        (is (= m (scan/proxy-configs m)))
        (is (= m2-result (scan/proxy-configs m2)))))))

(deftest config-proxy-connection
  (testing "Simple proxy"
    (let [resp @(http/get "http://localhost:4347/get"
                          (scan/with-proxy-configs {}
                            (merge proxy-auth-data
                                   {:proxy-host "http://127.0.0.1"
                                    :proxy-port 4348})))]
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp)))))
  (testing "proxy with authentication"
    (let [resp @(http/get "http://localhost:4347/get"
                          (scan/with-proxy-configs {}
                            (merge proxy-auth-data
                                   {:proxy-host "http://127.0.0.1"
                                    :proxy-port 4349})))]      
      (is (= 200 (:status resp)))
      (is (= "hello world" (:body resp)))))
  (with-test-configs
    (scan/save-logger-configs! (merge proxy-auth-data
                                      {:proxy-host "http://127.0.0.1"
                                       :proxy-port 4349}))
    (println (scan/with-proxy-configs {}))
    (testing "proxy with authentication and saved configs"  
      (let [resp @(http/get "http://localhost:4347/get"
                            (scan/with-proxy-configs {}))]
        (is (= 200 (:status resp)))
        (is (= "hello world" (:body resp)))))
    (testing "Various request functions"
      (is (scan/can-connect? "http://localhost:4347/get"))
      (is (scan/send-transit-request "http://localhost:4347/get"))
      ;; now we enter the wrong proxy password and test if we fail to connect
      (scan/save-logger-configs! (merge {:proxy-user "me" :proxy-password "wrong"}
                                        {:proxy-host "http://127.0.0.1"
                                         :proxy-port 4349}))
      (is (not (scan/can-connect? "http://localhost:4347/get")))
      (is (= "access denied" (:body (scan/send-transit-request "http://localhost:4347/get")))))))


