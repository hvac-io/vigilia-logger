(ns vigilia-logger.test.scan
  (:require [vigilia-logger.test.util :as u]
            [vigilia-logger.scan :as scan]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(deftest logger-id
  (u/with-test-configs
    ;; init config without any logger-id
    (scan/save-logger-configs! {})

    ;; make sure we really don't have any ID
    (is (not (:logger-id (scan/get-logger-configs))))
    ;; now we should create one on-the-fly and it should always remain
    ;; the same.
    (let [id (scan/get-logger-id!)] ;; initial creation
      (is id)
      (is (= id (:logger-id (scan/get-logger-configs))))
      (is (= id (scan/get-logger-id!))))))

(deftest logs-path
  (u/with-test-configs
    ;; clear init config
    (scan/save-logger-configs! {})

    ;; default path
    (is (= (scan/get-logs-path)
           scan/path))

    (let [new-path "test-some-other-path/"]
      ;; new logs path
      (scan/save-logger-configs! {:logs-path new-path})

      (with-redefs [scan/send-to-remote-server (fn [_] :fail)]
        (scan/scan-and-send) ;; generate a log file
        (is (= (count (scan/find-unsent-logs)))) ;; can we find it?
        (is (some? (seq (.listFiles (io/file new-path))))) ;; is it really where we expect it?

        ;; cleanup
        (u/delete-recursively! new-path)))))
