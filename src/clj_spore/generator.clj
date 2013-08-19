(ns clj-spore.generator
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:require [clojure.string :as str]
            [clj-http.core :as core]
            [clj-http.client :as client])
  (:import (java.net URL URLEncoder)))

(defn check-missing-params
  [required params]
  (let [str-params (into #{} (map name (keys params)))]
    (remove str-params required)))

(defn url-encode
  [string]
  (URLEncoder/encode (str string) "UTF-8"))

(defn interpolate-params
  [path params]
  (str/replace (reduce (fn [p [name val]] (str/replace p (str name) (url-encode val))) path params) #"/:[^/]+" ""))

(defn apply-middleware
  [cli m-spec spec]
  "apply a middleware to a client. A mw can be either a fn or something like:
 [wrap-clojure-request :enabled-if #(= (:path %)  \"/login\") :args [:type \"application/x-clojure\"]]
 :enabled-if is a predicate fn taking the spore method spec as sole param"
  (if (fn? spec)
    (spec cli)
    (let [[mw & {:keys [enabled-if args] :or {enabled-if (fn [_] true) args []}}] spec]
      (if (enabled-if m-spec)
        (apply mw cli args)
        cli))))

(defn wrap-middlewares
  [client m-spec & [middlewares]]
  (reduce (fn [cli mw] (apply-middleware cli m-spec mw)) client middlewares))

(defn wrap-interpolate-path-params
  [client]
  (fn [{:keys [path-info params script-name] :as request}]
    (let [path-params (:clj-spore-path-params request)
          fixed-path (interpolate-params path-info params)
          query-params (apply (partial dissoc params) path-params)
          uri (str script-name fixed-path)
          env* (assoc request :query-params query-params :uri uri :uri-string uri)]
      (client env*))))

(defn generate-query-string
  [params]
  (str/join "&"
            (map (fn [[k v]] (str
                             (url-encode (if (keyword? k) (name k) (str k)))
                             "="
                             (url-encode (str v))))
                   params)))

(defn wrap-allowed-query-params
  [client]
  (fn [{:keys [query-params clj-spore-all-params clj-spore-required-params] :as req}]
    (if query-params
      (client (-> req (dissoc :query-params)
                      (assoc :query-string
                             (generate-query-string (filter (fn [[k _]] (clj-spore-all-params k)) query-params)))))
      (client req))))

(defn- summarize-string
  [string]
  (if (or (nil? string) (empty? string) (< (count string) 100 ))
    string
    (str (apply str (take 100 string)) " [...]")))

(defn wrap-trace
  [client]
  (fn [req]
    (if (get (System/getenv) "SPORE_TRACE")
      (do
        (println "SPORE-REQUEST: " (assoc req :body (summarize-string (:body req))))
        (let [response (client req)]
          (println "SPORE-RESPONSE: " (assoc response :body (summarize-string (:body response))))
          response))
      (client req))))

(defn parse-integer
  [str]
  (try (Long/parseLong str)
       (catch Exception _
         (let [c (class str)]
           (cond (= c java.lang.Long) str
                 (= c java.lang.Integer) str
                 :else 0)))))

(defn wrap-timeout
  [client]
  (fn [{:keys [params] :as req}]
    (let [timeout (parse-integer (:timeout params))]
          (client (if (> timeout 0)
                    (merge req {:socket-timeout timeout
                                :conn-timeout timeout})
                    req)))))

(def base-client
  (-> #'core/request
      (client/wrap-output-coercion)
      (client/wrap-input-coercion)
      (client/wrap-content-type)
      (client/wrap-accept)
      (wrap-allowed-query-params)
      (wrap-interpolate-path-params)
      (wrap-trace)
      (wrap-timeout)))

(defn parse-code
  [code]
  (if (string? code)
    (Integer/parseInt code)
    (int code)))

(defn handle-error
  [throw-exceptions obj]
  (if throw-exceptions
    (throw+ obj)
    (if (map? obj)
      (merge {:status 599} obj)
      obj)))

(defn if-pos
  [v]
  (if (and v (pos? v)) v))

(defn generate-spore-method
  ([{:keys [name author version], api_base_url :base_url, api_format :format
     :or {api_format []}
     :as api}
   {:keys [method path required_params optional_params required_payload expected_status description authentication base_url format documentation]
    :or {description (str method-name " method")
         authentication false
         base_url api_base_url
         format (or api_format [])
	 optional_params []
	 required_params []
	 expected_status []
         documentation (str "no documentation for " method-name)}
    :as spec}
   method-name
   middlewares
   {:keys [throw-exceptions] :or {throw-exceptions true} :as options}]
     (let [wrapped-client (wrap-middlewares base-client spec middlewares)
           req-params (set  required_params)
           expected (set (map parse-code expected_status))
           throw-on-unexpected? (not (empty? expected))
           all-params (set (concat
                            (map #(keyword %) optional_params)
                            (map #(keyword %) req-params)))]
     (fn
       ^{:doc documentation, :method-name method-name, :authentication authentication}
       [& {:keys [payload] :as user-params}]
       (let [missing (check-missing-params req-params user-params)]
         (if-not (empty? missing)
           (handle-error throw-exceptions {:type :missing-params :params user-params :missing-params missing :required-params req-params})
           (let [base_uri (URL. base_url)
                 scheme (.getProtocol base_uri)
                 env {:request-method (keyword (str/lower-case method))
                      :script-name (.getPath base_uri)
                      :path-info path
                      :request-uri ""
                      :query-string ""
                      :server-name (.getHost base_uri)
                      :server-port (or (if-pos (.getPort base_uri)) (if (= scheme "https") 443 80))
                      :body payload
                      :params (dissoc user-params :payload)
                      :scheme scheme
                      :clj-spore-expected-status expected
                      :clj-spore-authentication authentication
                      :clj-spore-path-params (into [] (map #(-> % second keyword) (re-seq #":([^/]+)" path)))
                      :clj-spore-method-name method-name
                      :clj-spore-format (set format)
                      :clj-spore-required-params req-params
                      :clj-spore-all-params all-params
                      :clj-spore-required-payload required_payload}
                 res (wrapped-client env)]
             (if (and throw-on-unexpected? (not (contains? expected (int (:status res)))))
               (handle-error throw-exceptions {:type :unexpected-status :method method-name :params user-params :env env :response res :expected-status expected})
               res))))))))
