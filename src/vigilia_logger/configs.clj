(ns vigilia-logger.configs
  (:require [bacure.local-save :as local]
            [clojure.java.io :as io]))

(def path "Logger path to save persistent data." (str local/path "logger/"))

(defn- configs-path
  []
  (str path "/configs.edn"))

(defn fetch
  "Return the logger configs form the disk with the nil entries removed.
  Nil on exception."
  []
  (let [remove-nil-in-maps #(into {} (remove (comp nil? val) %))]
    (try (->> (configs-path)
              slurp
              local/safe-read
              remove-nil-in-maps)
         (catch Exception e))))

(defn save!
  "Save data to configs file. Return data."
  [data]
  (let [pp-data (pr-str data)]
    (local/mkdir-spit (configs-path) pp-data) data))

(defn delete!
  "Delete the logger configs file, if found." []
  (try (io/delete-file (configs-path))
       (catch Exception e)))

(defn logs-path
  "Return the location where logs are saved."
  []
  (or (:logs-path (fetch))
      path))
