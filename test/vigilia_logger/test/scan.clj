(ns vigilia-logger.test.scan
  (:require [vigilia-logger.test.util :as u]
            [vigilia-logger.scan :as scan]
            [clojure.test :refer :all]))

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
