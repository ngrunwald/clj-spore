(ns clj-spore.middleware
  (:require [clojure.contrib.string :as str]
            [clj-json.core :as json]))

(defn get-charset
  "Extracts charset from Content-Type header. utf-8 by default."
  [{:keys [content-type] :as res}]
  (let [default-charset "utf-8"]
    (if content-type
      (or (second (re-find #";\s*charset=([^\s;]+)" content-type)) default-charset)
      default-charset)))

(defn make-type-response-pred
  "returns a predicate fn checking if Content-Type response header matches a specified regexp and body is set."
  [regexp]
  (fn [req {:keys [body headers] :as res}]
    (if-let [#^String type (get headers "content-type")]
      (and (string? body) (not (empty? (re-find regexp type)))))))

(defn wrap-format-response
    "Wraps a client such that response body is deserialized from the right format and added in the :decoded-body key. It takes 2 args:
:predicate is a predicate taking the request and response as arguments to test if deserialization should be used.
:decoder specifies a fn taking the body String as sole argument and giving back a hash-map."
  [client & {:keys [predicate decoder]}]
  (fn [req]
    (let [res (client req)]
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
  [client & {:keys [predicate decoder]
              :or {predicate json-response?
                   decoder json/parse-string}}]
  (wrap-format-response client :predicate predicate :decoder decoder))

