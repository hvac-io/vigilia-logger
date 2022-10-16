(ns vigilia-logger.test.tools
  (:require [clojure.test :refer :all]
            [vigilia-logger.tools :as tools]))

(deftest timeout
  ;; body evaluates correctly
  (let [a* (atom 1)]
    (tools/with-timeout 5 "" (swap! a* inc))
    (is (= 2 @a*)))

  ;; timeout
  (let [a* (atom 1)]    
    (is (thrown? clojure.lang.ExceptionInfo
                 (tools/with-timeout 5 "msg"
                   (Thread/sleep 10)
                   (swap! a* inc))))
    (is (= 1 @a*))))
