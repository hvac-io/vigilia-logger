(ns vigilia-logger.test.util
  (:require [clojure.java.io :as io]
            [vigilia-logger.scan :as scan]))

(defn delete-recursively! [fname]
  (let [func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (io/delete-file f))]
    (func func (io/file fname))))

(defmacro with-test-configs
  "Temporarily redefine the config path and seed the config value.
  Everything in the temporary config path will be deleted afterward"
  [& body]
  `(with-redefs [scan/path "test-path/"]
     ;; save an initial config file
     (scan/save-logger-configs! {:time-interval 1
                                 :api-root "http://localhost:4347/get"
                                 :project-id 12
                                 :logger-key 12})
     (try ~@body          
          (finally
            (delete-recursively! scan/path)))))
