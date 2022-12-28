(ns vigilia-logger.test.scan
  (:require [bacure.local-save :as local]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [taoensso.timbre :as timbre]
            [vigilia-logger.configs :as configs]
            [vigilia-logger.http :as http]
            [vigilia-logger.scan :as scan]
            [vigilia-logger.test.util :as u]))

(deftest logs-path
  (u/with-test-configs
    ;; clear init config
    (configs/save! {})

    ;; default path
    (is (= (configs/logs-path)
           configs/path))

    (let [new-path "test-some-other-path/"]
      ;; new logs path
      (configs/save! {:logs-path new-path})

      (with-redefs [scan/send-to-remote-server (fn [_] :fail)]
        (scan/scan-and-send) ;; generate a log file
        (is (= (count (scan/find-unsent-logs)) 1)) ;; can we find it?
        (is (some? (seq (.listFiles (io/file new-path))))) ;; is it really where we expect it?

        ;; cleanup
        (u/delete-recursively! new-path)))))

(deftest read-logs
  (u/with-test-configs
    (let [logs-path (configs/logs-path)
          log-name "vigilia-test.log"]
      (testing "Corrupted log returns nil"
        (local/mkdir-spit (io/file logs-path log-name) "{:a ")
        (is (nil? (scan/read-log logs-path log-name))))

      (testing "Empty log returns nil"
        (local/mkdir-spit (io/file logs-path log-name) "")
        (is (nil? (scan/read-log logs-path log-name))))

      (testing "Normal log is read"
        (local/mkdir-spit (io/file logs-path log-name) "{:a 1}")
        (is (= {:a 1} (scan/read-log logs-path log-name)))))))



(defn- simple-vigilia-handler
  "Simulate the Vigilia API.
  - Project root: return a link to the logging endpoint.
  - Logging endpoint:
     - GET: :logging-allowed? and :href
     - POST: accepts logs"
  [req]
  (let [respond (fn [data] {:status  200
                            :headers {"content-type" "application/transit+json"}
                            :body    (http/transit-encode data)})
        handler {"/project/12"
                 {:get (respond {:logging {:href "http://localhost:4347/project/12/logging"}})}

                 "/project/12/logging"
                 {:get  (respond {:logging-allowed? true
                                  :href             "http://localhost:4347/project/12/logging"})
                  :post {:status 200
                         :body   "logs accepted"}}}]
    (get-in handler [(:uri req) (:request-method req)])))

(deftest local-logs-are-sent
  (timbre/with-level :fatal
    ;; Don't print out the logs
    (u/with-test-configs
      (let [logs-path (configs/logs-path)
            test-logs [{:a 1} ; normal
                       ""     ; empty
                       'a]   ; corrupted
            test-logs-qty (count test-logs)]
        (doseq [[idx data] (map-indexed vector test-logs)]
          (spit (io/file (str logs-path "vigilia-"idx ".log")) data))
        (is (= test-logs-qty (count (scan/find-unsent-logs))))
        (testing "Server error"
          (u/with-server (fn [req] {:status 500})
            (scan/send-local-logs)
            (is (= test-logs-qty (count (scan/find-unsent-logs))))))

        (testing "Server accepts logs"
          (u/with-server simple-vigilia-handler
            (scan/send-to-remote-server {:a 1})
            (scan/send-local-logs)
            (is (= 0 (count (scan/find-unsent-logs))))))))))
