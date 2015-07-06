(defproject io.hvac.vigilia.logger/vigilia-logger "1.0.0"
  :description "A library/app to record a BACnet network"
  :url "https://hvac.io"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 
                 [bacure "0.5.3"]
                 [clj-time "0.9.0"]
                 [trptcolin/versioneer "0.2.0"]

                 ;; communication with remote servers
                 [http-kit "2.1.18"]
                 [com.cognitect/transit-clj "0.8.275"]

                 ;; recurring jobs
                 [overtone/at-at "1.2.0"]])
