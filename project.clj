(defproject io.hvac.vigilia/vigilia-logger "1.1.0"
  :description "A library/app to record a BACnet network"
  :url "https://hvac.io"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 
                 [bacure "1.1.9"]
                 [clj-time "0.15.2"]
                 [trptcolin/versioneer "0.2.0"]

                 ;; communication with remote servers
                 [clj-http "3.11.0"]
                 [org.clojure/data.codec "0.1.1"]
                 [com.cognitect/transit-clj "1.0.324"]

                 ;; recurring jobs
                 [overtone/at-at "1.2.0"]

                 ;; logging
                 [org.clojure/tools.logging "1.2.4"]

                 ;; Threadpool tools (pmap)
                 [com.climate/claypoole "1.1.4"]]

  :plugins [[lein-ancient "0.6.15"]]
  :test-paths ["test"]
  :profiles {:dev
             {:dependencies
              [[ring/ring-defaults "0.3.2"]
               [ring-request-proxy "0.1.11"]
               [http-kit "2.4.0"]
               [ring-basic-authentication "1.1.0"]
               [compojure "1.6.2"]
               [ring/ring-jetty-adapter "1.8.1"]

               ;; error logs
               [com.taoensso/timbre "5.1.0"]
               [com.fzakaria/slf4j-timbre "0.3.20"]]}})
