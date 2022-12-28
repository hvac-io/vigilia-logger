(ns vigilia-logger.test.util
  (:require [clojure.java.io :as io]
            [org.httpkit.server :as server]
            [vigilia-logger.configs :as configs]
            [clojure.string :as str]))

(defn delete-recursively! [fname]
  (let [func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (io/delete-file f))]
    (func func (io/file fname))))

(def test-port 4347)
(def project-id 12)
(def logger-key 12)

(defmacro with-test-configs
  "Temporarily redefine the config path and seed the config value.
  Everything in the temporary config path will be deleted afterward"
  [& body]
  `(with-redefs [configs/path "test-path/"]
     ;; save an initial config file
     (configs/save! {:time-interval 1
                     :api-root      (str "http://localhost:" test-port)
                     :project-id    project-id
                     :logger-key    logger-key})
     (try ~@body          
          (finally
            (delete-recursively! configs/path)))))


(defn ephemeral-server
  [handler f]
  (let [*called? (atom false)
        checked-handler (fn [req]
                          (reset! *called? true)
                          (handler req))
        stop-server (-> checked-handler
                        ;(defaults/wrap-defaults defaults/site-defaults)
                        (server/run-server {:port test-port}))]
    (try (let [return (f)]
           (when-not @*called?
             (throw (ex-info "Server was never reached" {:port test-port})))
           return)
         (finally (stop-server)))))

(defmacro with-server
  "Starts a server on port 4347 with the provided handler.
  Body is evaluated and then the server is closed with `finally`."
  [handler & body]
  `(ephemeral-server ~handler (fn [] ~@body)))


(defn path
  "Converts provided items into a path.

  (path :project 12)
  => \"/project/12\""
  [& items]
  (let [raw-path (->> (map #(if (keyword? %) (name %) (str %)) items)
                      (str/join "/"))]
    (if (str/starts-with? raw-path "/")
      raw-path
      (str "/" raw-path))))

(defn url
  "Construct a url to reach the test server.

  (url :project 12 :logging)
  => \"http://localhost:4347/project/12/logging\""
  [& items]
  (->> (apply path items)
       (str "http://localhost:" test-port)))
