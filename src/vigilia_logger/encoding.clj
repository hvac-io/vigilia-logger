(ns vigilia-logger.encoding
  (:require [bacure.core :as bac]
            [bacure.coerce :as c]
            [clj-time.core :as time] ;already required in bacure
            [clojure.string :as s]
            [clojure.walk :as w]))

(defn timestamp []
  (.getMillis (time/now)))
(defn iso-8601-timestamp []
  (str (time/now)))

;; ================================================================
;; ======================= Data encoding ==========================
;; ================================================================
(comment 
  ;; Here is how the data should be encoded

  ;; Firstly, as opposed to the BACnet object scheme, we will not hide
  ;; the device-id in an object property. Instead, it will be the key
  ;; by which we store the data.
{:some-device-id ;<------ device-id as the top key
 {:update "iso-string"
  :name "device-name" ;; so we don't have to dig in the properties to get it.
  :objects "<DATA>"}}

;; objects are then classified by their object-integer to make it
;; easier to sort and find them in databases.
;; --->
{:0 ;; object type 'analog-input'
 {:1 ;; object instance
  ;; then we take some of the properties
  {:Description "some description"
   :Out-of-service false
   :Present-value 13.648
   :Object-name "Some name"
   :Units "units in a string format"}}}
;; One must also notice that the properties keys are capitalized. This
;; is required to be compatible with the logger V1.
)

;; Some object types aren't very useful; we can ignore them.
(def ignored-object-types
  [:trend-log
   :trend-log-multiple
   :notification-class
   :event-log
   :event-enrollment])


;; We really don't need to keep every properties. However, those we
;; choose to keep are determined by the object type.

(defn prop-by-object-type
  "Properties to retrieve based on the object type."
  [object-type]
  (let [normal-IO-prop [:object-name :description :present-value
                        :units :status-flags :priority-array]
        desired-props {:analog-input normal-IO-prop
                       :analog-ouput normal-IO-prop
                       :binary-input normal-IO-prop
                       :binary-output normal-IO-prop
                       :device [:object-name :description :device-type :vendor-identifier :vendor-name :model-name]
                       :file [:object-name :description]
                       :loop [:object-name :description :present-value :manipulated-variable-reference :controlled-variable-reference
                              :controlled-variable-value :setpoint-reference :setpoint :status-flags]
                       :schedule [:object-name :description :weekly-schedule :exception-schedule :status-flags]}
        default-props [:object-name :description :present-value :status-flags :priority-array]]
    (get desired-props object-type
         default-props)))

(defn convert-units
  "Convert units keyword to their string value."[m]
  (if-let [unit (find m :units)]
    (-> (c/clojure->bacnet :engineering-units (val unit))
        ((fn [x] (assoc m :units (.toString x)))))
    m))

(defn capitalize-keys
  "Recursively transforms all map keys to their capitalized version."
  [m]
  (let [f (fn [[k v]] (if (keyword? k)
                        [(-> (name k) s/capitalize keyword) v]
                        [k v]))]
    ;; only apply to maps
    (w/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn encode-properties
  "Take bacure.core properties and convert them to the expected logger
  format." [m]
  (-> m convert-units capitalize-keys))


;; ================================================================
;; ======================= Data retrieval =========================
;; ================================================================
  

(defn get-properties-by-type
  "Get the properties and return them in the correct format.

   [{:<object-instance> properties-list}
    {:<object-instance> properties-list}...]"
  [device-id object-type object-identifier]
  (let [f (fn [m]
            (when m
              [(keyword (-> m :object-identifier last str)) ;; object instance
               (-> m (dissoc :object-identifier) encode-properties)]))]
    (->> (when-let [props (seq (prop-by-object-type object-type))]
           (bac/remote-object-properties device-id object-identifier props))
         (map f)
         (remove nil?)
         (into {}))))

(defn get-properties
  "Retrieve properties for each object.

   The different properties to retrieve are determined by the object
   type. Consequently, each request is divided by
   object-type.

   The returned properties are classed in a map, associated with the
   device instance as a key. It turns associated to the object type as
   a key.

   {:0                  --- the object type
     {:1                --- the object instance
       {:Description    --- properties"
  ([device-id object-identifiers] (get-properties device-id object-identifiers nil))
  ([device-id object-identifiers read-object-delay]
   (let [grouped (group-by first object-identifiers)] ;; group by object type
     (->> (mapcat (fn [[k v]]
                    (when read-object-delay
                      (Thread/sleep read-object-delay))
                    (try (let [props (get-properties-by-type device-id k v)]
                           (when (seq props)
                             {(keyword (str (.intValue (c/clojure->bacnet :object-type k))))
                              props}))
                         (catch Exception e))) grouped)
          (remove nil?)
          (into {})))))

;; Catch exceptions everywhere we can: the logging MUST NOT stop

(defn update-binary [properties object-type-key]
  (if-let [binary-objects (get properties object-type-key)]
    (->> {object-type-key (->> (for [[instance-key obj] binary-objects]
                                 [instance-key (merge obj
                                                      (when-let [pv (:Present-value obj)]
                                                        {:Present-value (if (= pv :inactive) 0 1)}))])
                               (into {}))}
         (merge properties))
    properties))

(defn update-all-binaries
  "Convert all :active and :inative values to 1s and 0s."
  [properties]
  (-> properties
      (update-binary :3)
      (update-binary :4)
      (update-binary :5)))


(defn scan-device
  "Return a map with the device-id as the key.

  {:some-device-id ;<------ device-id as the top key
   {:update \"iso-string\"
    :name \"device-name\" ;; so we don't have to dig in the properties to get it.
    :scan-duration <time in ms>
    :objects \"<DATA>\"}}
  
  Optional 'target-objects' is a collection of object
  identifiers (such as [:analog-input 1]) to restrict the scan to
  those objects.

  Optional 'read-object-delay' is the delay (in ms) between the scan
  of each object within a device.
  
  If the remote device doesn't exist or isn't answering, return nil."
  ([device-id] (scan-device device-id nil))
  ([device-id device-target-objects] (scan-device device-id device-target-objects nil))
  ([device-id device-target-objects read-object-delay]
   (try
     (let [start-time (timestamp)
           ;; if we don't have device-target-objects, just use the remote-objects list
           object-identifiers (-> (or device-target-objects
                                      (bac/remote-objects device-id))
                                  (conj [:device device-id]) ;; we always add the device object
                                  (distinct))
           properties (-> (get-properties device-id object-identifiers read-object-delay)
                          (update-all-binaries))]
       (when (seq properties) ;; only return something if we got some data
         {(keyword (str device-id))
          {:update (iso-8601-timestamp)
           :name (get-in properties [:8 (keyword (str device-id)) :Object-name])
           :objects properties
           :scan-duration (- (timestamp) start-time)}}))
     (catch Exception e))))
