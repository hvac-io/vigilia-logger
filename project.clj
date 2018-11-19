(defproject io.hvac.vigilia/vigilia-logger "1.0.14-alpha"
  :description "A library/app to record a BACnet network"
  :url "https://hvac.io"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 
                 [bacure "1.0.8"]
                 [clj-time "0.14.4"]
                 [trptcolin/versioneer "0.2.0"]

                 ;; communication with remote servers
                 [http-kit "2.3.0"]
                 [org.clojure/data.codec "0.1.1"]
                 [com.cognitect/transit-clj "0.8.309"]

                 ;; recurring jobs
                 [overtone/at-at "1.2.0"]]

  :plugins [[lein-ancient "0.6.15"]]
  :test-paths ["test"]
  :profiles {:dev
             {:dependencies
              [[ring/ring-defaults "0.3.2"]
               [ring-request-proxy "0.1.11"]
               [ring-basic-authentication "1.0.5"]
               [compojure "1.6.1"]
               [ring/ring-jetty-adapter "1.6.3"]]}})
