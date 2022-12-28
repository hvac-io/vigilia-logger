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
