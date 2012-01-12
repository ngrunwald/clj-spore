(ns clj-spore.middleware
  (:require [cheshire.core :as json]))

(defn get-charset
  "Extracts charset from Content-Type header. utf-8 by default."
  [{:keys [content-type] :as res}]
  (let [default-charset "utf-8"]
    (if content-type
      (or (second (re-find #";\s*charset=([^\s;]+)" content-type)) default-charset)
      default-charset)))

;; Response format handling

(defn make-type-response-pred
  "returns a predicate fn checking if Content-Type response header matches a specified regexp and body is set."
  [regexp]
  (fn [req {:keys [body headers] :as res}]
    (if-let [#^String type (get headers "content-type")]
      (and (string? body) (not (empty? (re-find regexp type)))))))

(defn wrap-format-response
    "Wraps a client such that response body is deserialized from the right format and added in the :decoded-body key. It takes 3 args:
:predicate is a predicate taking the request and response as arguments to test if deserialization should be used.
:decoder specifies a fn taking the body String as sole argument and giving back a hash-map.
:type specifies the type for the MIME Accept header."
  [client & {:keys [predicate decoder type]}]
  (fn [req]
    (let [res (client (assoc-in req [:headers "accept"] type))]
      (if (predicate req res)
        (if-let [body (:body res)]
          (let [fmt-body (decoder body)
                res* (assoc res :decoded-body fmt-body)]
            res*)
          res)
        res))))

(def json-response?
  (make-type-response-pred #"^application/(vnd.+)?json"))

(defn wrap-json-response
  "Handles body response in JSON format. See wrap-format-response for details."
  [client & {:keys [predicate decoder type keywordize]
             :or {keywordize false
                  predicate json-response?
                  decoder #(json/parse-string % keywordize)
                  type "application/json"}}]
  (wrap-format-response client :predicate predicate :decoder decoder :type type))

(def clojure-response?
  (make-type-response-pred #"^application/(vnd.+)?(x-)?clojure"))

(defn wrap-clojure-response
  "Handles body response in Clojure format. See wrap-format-response for details."
  [client & {:keys [predicate decoder type]
              :or {predicate clojure-response?
                   decoder read-string
                   type "application/clojure"}}]
  (wrap-format-response client :predicate predicate :decoder decoder :type type))

;; Request format handling

(defn wrap-format-request
    "Wraps a client such that the request body is serialized to the right format. It takes 2 args:
:type is a string specifying the MIME type sent.
:encoder specifies a fn taking the body (payload) structure as sole argument and giving back a string."
  [client & {:keys [encoder type]}]
  (fn [req]
    (if-let [body (:body req)]
      (let [fmt-body (encoder body)
            req* (-> req (assoc :body fmt-body) (assoc-in [:headers "content-type"] type))]
        (client req*))
      (client req))))

(defn wrap-json-request
  "Handles serialization of payload params in JSON format. See wrap-format-request for details."
  [client & {:keys [encoder type]
              :or {type "application/json"
                   encoder json/generate-string}}]
  (wrap-format-request client :type type :encoder encoder))

(defn wrap-clojure-request
  "Handles serialization of payload params in Clojure format. See wrap-format-request for details."
  [client & {:keys [encoder type]
              :or {type "application/clojure"
                   encoder prn-str}}]
  (wrap-format-request client :type type :encoder encoder))

;; All-in-one formats

(defn wrap-json-format
  "Handles serialization and deserialization of JSON."
  [client & [keywordize?]]
  (-> client
      (wrap-json-response :keywordize keywordize?)
      (wrap-json-request)))

(defn wrap-clojure-format
  "Handles serialization and deserialization of Clojure."
  [client]
  (-> client
      (wrap-clojure-request)
      (wrap-clojure-response)))

;; Others

(defn wrap-runtime
  "Records time spent in the request in the header x-xpore-runtime of the response (in millis)."
  [client]
  (fn [req]
    (let [start (System/currentTimeMillis)
          res (client req)
          end (System/currentTimeMillis)]
      (assoc-in res [:headers "x-spore-runtime"] (- end start)))))


