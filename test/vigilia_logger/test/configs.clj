(ns vigilia-logger.test.configs
  (:require [clojure.test :refer :all]
            [vigilia-logger.configs :as configs]
            [vigilia-logger.test.util :as u]))

(deftest logger-configs
  (u/with-test-configs
    ;; Delete the configs
    (configs/delete!)
    (is (nil? (configs/delete!))
        "Do not throw if file is missing")

    (is (nil? (configs/fetch))
        "Do not throw if file is missing")

    (let [data {:a 1}]
      (configs/save! data)
      (is (= data (configs/fetch))))))

(deftest logger-id
  (u/with-test-configs
    ;; init config without any logger-id
    (configs/save! {})
    ;; make sure we really don't have any ID
    (is (not (:logger-id (configs/fetch))))
    ;; now we should create one on-the-fly and it should always remain
    ;; the same.
    (let [id (configs/get-logger-id!)] ;; initial creation
      (is id)
      (is (= id (:logger-id (configs/fetch))))
      (is (= id (configs/get-logger-id!))))))
