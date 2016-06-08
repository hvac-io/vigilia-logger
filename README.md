# Vigilia-Logger

A Clojure library built on top of
[Bacure](https://github.com/Frozenlock/bacure) with the purpose of
taking snapshots of a BACnet network at regular intervals.

One needs to have access to a Vigilia server to save the data.

![Logs](/logs.jpg)

## Usage

Add the following dependency in your `project.clj`:

[![Clojars Project](http://clojars.org/io.hvac.vigilia/vigilia-logger/latest-version.svg)](http://clojars.org/io.hvac.vigilia/vigilia-logger)

Require `logger.timed` and run `maybe-start-logging`. If a
`project-id` is found in the configuration file, the logger will start
to log the network.
8

## Configuration file

You can write a configuration file using


```clj

(save-logger-configs! {:project-id "my-project-id"})

```

There is many configuration options:



```clj

{:project-id "some-ID" ;; The project identifier
 
 :api-root "https://vigilia.hvac.io/api/v1"
 ;; The API root URL. Defaults to "https://vigilia.hvac.io/api/v1".
 ;; Change only if you are hosting your own Vigilia
 ;; server.

 :logger-id "logger-first-floor"
 ;; A project can be recorded by many loggers, for example when the
 ;; BACnet network is on multiple VLAN. The ID can help for
 ;; troubleshooting. "Why is this section not recorded? Which logger
 ;; is responsible for it?"

 :logger-version "vigilia-logger-1.0.2"
 ;; Can be used to know which features are available

 :logger-key
 ;; Each project will only accept payloads when given with a secret
 ;; key. This prevents unwanted writes into projects.

 :min-range ;; Only devices with IDs higher will be recorded.
 :max-range ;; Only devices with IDs lower will be recorded.
 :id-to-remove  ;; Remove devices by their ID
 :id-to-keep    ;; keep devices by their ID
 :time-interval ;; Time interval between each snapshots (in minutes)
 :criteria-coll ;; Advanced filtering options. See the Bacure library for details.
 :object-delay  ;; Delay (in ms) between each object scan of a device.
 :target-objects ;; Map of devices and there associated target object identifiers (if any).
 }

```


## License

Copyright © 2016 HVAC.IO

GNU General Public License V3
