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


;;; logger ID generation

(def ^{:private true} constituent-chars
  "Characters: a-z, A-Z, 0-9."
  (->> [[\a \z] [\A \Z] [\0 \9]]
       (mapcat (fn [[x y]] (range (int x) (inc (int y)))))
       (map char)
       vec))

(defn- rand-string
  "Generates a random string of [A-z0-9] of length n."
  [n]
  (apply str (repeatedly n #(rand-nth constituent-chars))))

(defn- new-logger-id!
  "Generate a new logger-id, save it into the config file and return
  it."
  [logger-configs]
  (let [new-id (str "logger-" (rand-string 6))]
    (-> logger-configs
        (assoc :logger-id new-id)
        (save!))
    new-id))

(defn get-logger-id!
  "Get the existing logger id, or generate one and save it before
  returning it."
  []
  (let [configs (fetch)]
    (or (:logger-id configs)
        (new-logger-id! configs))))


(def default-api-root "https://vigilia.hvac.io/api/v1")

(defn api-root
  "Get the api root path. If nothing is found, default to
  https://vigilia.hvac.io/api/v1."
  []
  (or (:api-root (fetch))
      default-api-root))
