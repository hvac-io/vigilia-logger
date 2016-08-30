(defproject io.hvac.vigilia/vigilia-logger "1.0.9-alpha"
  :description "A library/app to record a BACnet network"
  :url "https://hvac.io"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 
                 [bacure "1.0.6"]
                 [clj-time "0.9.0"]
                 [trptcolin/versioneer "0.2.0"]

                 ;; communication with remote servers
                 [http-kit "2.2.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [com.cognitect/transit-clj "0.8.285"]

                 ;; recurring jobs
                 [overtone/at-at "1.2.0"]]
  
  :test-paths ["test"]
  :profiles {:dev
             {:dependencies
              [[ring/ring-defaults "0.2.1"]
               [ring-request-proxy "0.1.4"]
               [ring-basic-authentication "1.0.5"]
               [compojure "1.5.1"]
               [ring/ring-jetty-adapter "1.5.0"]
               ]}})
