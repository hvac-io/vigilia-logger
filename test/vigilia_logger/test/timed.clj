(ns vigilia-logger.test.timed
  (:require [bacure.local-save :as local]
            [bacure.remote-device :as rd]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [vigilia-logger.configs :as configs]
            [vigilia-logger.test.util :as u]
            [vigilia-logger.timed :as timed]
            [vigilia-logger.tools :as tools]))

(defn- with-configs!
  "Merge into the saved configs and make the perimeter re-read them."
  [m]
  (configs/save! (merge (configs/fetch) m)))

(deftest who-is-range
  (is (nil? (timed/who-is-range {})) "no range: global Who-Is")
  (is (= {:min-range 101 :max-range 199} (timed/who-is-range {:min-range 100 :max-range 200}))
      "logger ranges are exclusive, BACnet ranges are inclusive")
  (is (= {:min-range 101} (timed/who-is-range {:min-range 100})))
  (is (= {:max-range 199} (timed/who-is-range {:max-range 200}))))

(deftest discovery-interval
  (u/with-test-configs
    (let [who-is (atom 0)
          ranges (atom [])]
      (with-redefs [rd/find-remote-devices (fn [_ range] (swap! who-is inc) (swap! ranges conj range))]
        (testing "by default, at most one Who-Is per hour"
          (reset! timed/*last-execution-time 0)
          (timed/throttled-find-remote-devices)
          (timed/throttled-find-remote-devices)
          (is (= 1 @who-is))
          (is (= (* 60 60 1000) (timed/discovery-interval-ms (configs/fetch)))))

        (testing "configurable"
          (with-configs! {:discovery-interval 5})
          (reset! who-is 0)
          (reset! timed/*last-execution-time (- (System/currentTimeMillis) (* 4 60 1000)))
          (timed/throttled-find-remote-devices)
          (is (= 0 @who-is) "4 minutes ago: too soon")
          (reset! timed/*last-execution-time (- (System/currentTimeMillis) (* 6 60 1000)))
          (timed/throttled-find-remote-devices)
          (is (= 1 @who-is) "6 minutes ago: time to ask again"))

        (testing "restricted to the configured range"
          (is (= [nil nil] @ranges) "so far: global Who-Is")
          (with-configs! {:min-range 1000 :max-range 2000})
          (reset! timed/*last-execution-time 0)
          (timed/throttled-find-remote-devices)
          (is (= {:min-range 1001 :max-range 1999} (last @ranges))))

        (testing "0 disables it"
          (with-configs! {:discovery-interval 0})
          (reset! who-is 0)
          (reset! timed/*last-execution-time 0)
          (timed/throttled-find-remote-devices)
          (is (= 0 @who-is))
          (is (nil? (timed/discovery-interval-ms (configs/fetch)))))))))

(deftest auto-fetch-filter
  (u/with-test-configs
    (binding [timed/*perimeter-configs-max-age-ms* 0] ; always re-read
      (with-configs! {:id-to-remove [5] :min-range 2})
      (testing "when not logging, everything is fetched"
        (with-redefs [timed/is-logging? (constantly false)]
          (is (timed/auto-fetch-filter 5))
          (is (timed/auto-fetch-filter 1))))

      (with-redefs [timed/is-logging? (constantly true)]
        (testing "when logging, only the devices we scan"
          (is (not (timed/auto-fetch-filter 5)) "id-to-remove")
          (is (not (timed/auto-fetch-filter 1)) "min-range")
          (is (timed/auto-fetch-filter 6)))

        (testing "config changes are picked up"
          (with-configs! {:id-to-keep [5 7] :id-to-remove nil :min-range nil})
          (is (timed/auto-fetch-filter 5))
          (is (timed/auto-fetch-filter 7))
          (is (not (timed/auto-fetch-filter 6))))

        (testing "an unreadable config file keeps the last valid perimeter"
          (local/mkdir-spit (str configs/path "/configs.edn") "{:id-to-keep [5")
          (is (nil? (configs/fetch)) "sanity check: the file is unreadable")
          (is (timed/auto-fetch-filter 5))
          (is (not (timed/auto-fetch-filter 6))
              "nil configs would mean 'no restriction'"))))))

(deftest perimeter-configs-are-cached
  (u/with-test-configs
    (let [reads (atom 0)
          original configs/fetch]
      (with-redefs [configs/fetch (fn [] (swap! reads inc) (original))
                    timed/is-logging? (constantly true)]
        (binding [timed/*perimeter-configs-max-age-ms* 60000]
          ;; force a refresh with an expired cache
          (reset! @#'timed/perimeter-configs {:at 0 :configs nil})
          (dotimes [_ 50] (timed/auto-fetch-filter 1))
          (is (= 1 @reads) "50 I-Am shouldn't mean 50 reads of the config file"))))))

(deftest with-timeout-interrupted
  (testing "Interrupting the waiting thread cancels the body"
    (let [body-interrupted (promise)
          t (Thread. (fn []
                       (try (tools/with-timeout 10000 "test"
                              (try (Thread/sleep 10000)
                                   (catch InterruptedException e
                                     (deliver body-interrupted true))))
                            (catch InterruptedException e))))]
      (.start t)
      (Thread/sleep 100)
      (.interrupt t)
      (is (true? (deref body-interrupted 2000 :not-cancelled))))))
