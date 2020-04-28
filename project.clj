(defproject io.hvac.vigilia/vigilia-logger "1.0.16"
  :description "A library/app to record a BACnet network"
  :url "https://hvac.io"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 
                 [bacure "1.1.7"]
                 [clj-time "0.15.2"]
                 [trptcolin/versioneer "0.2.0"]

                 ;; communication with remote servers
                 [http-kit "2.4.0-alpha3"] ; alpha needed for java 11 support
                 [org.clojure/data.codec "0.1.1"]
                 [com.cognitect/transit-clj "1.0.324"]

                 ;; recurring jobs
                 [overtone/at-at "1.2.0"]

                 ;; Threadpool tools (pmap)
                 [com.climate/claypoole "1.1.4"]]

  :plugins [[lein-ancient "0.6.15"]]
  :test-paths ["test"]
  :profiles {:dev
             {:dependencies
              [[ring/ring-defaults "0.3.2"]
               [ring-request-proxy "0.1.11"]
               [ring-basic-authentication "1.0.5"]
               [compojure "1.6.1"]
               [ring/ring-jetty-adapter "1.8.0"]
               [org.slf4j/slf4j-log4j12 "1.7.30"]]}})
