(ns clj-spore.generator
  (:require [clojure.contrib.string :as str]
            [clj-json.core :as json]
            [clj-http.core :as core]
            [clj-http.client :as client])
  (:import (java.net URL URLEncoder)))

(defn check-missing-params
  [required params]
  (let [str-params (map #(name %) (keys params) )]
    (filter #(not (.contains str-params %)) required)))

(defn url-encode
  [string]
  (URLEncoder/encode string "UTF-8"))

(defn interpolate-params
  [path params]
  (str/replace-re #"/:[^/]+" "" (reduce (fn [p [name val]] (str/replace-str (str name) (url-encode val) p)) path params)))

(defn apply-middleware
  [cli m-spec spec]
  (if (fn? spec)
    (spec cli)
    (let [[mw & {:keys [enabled-if args] :or {enabled-if (fn [_] true) args []}}] spec]
      (if (enabled-if m-spec)
        (apply mw cli args)))))

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

(def base-client
  (-> #'core/request
      (client/wrap-output-coercion)
      (client/wrap-input-coercion)
      (client/wrap-content-type)
      (client/wrap-accept)
      (wrap-allowed-query-params)
      (wrap-interpolate-path-params)))

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
   middlewares]
     (let [wrapped-client (wrap-middlewares base-client spec middlewares)
           req-params (set  required_params)
           expected (set expected_status)
           all-params (set (concat
                            (map #(keyword %) optional_params)
                            (map #(keyword %) req-params)))]
     (fn
       ^{:doc documentation, :method-name method-name, :authentication authentication}
       [& {:as user-params}]
       (let [missing (check-missing-params req-params user-params)]
         (if-not (empty? missing)
           nil
           ;; ({:status 599 :error (str "missing params calling " method_name ": " (str/join ", " missing))})
           (let [base_uri (URL. base_url)
                 scheme (.getProtocol base_uri)
                 env {:request-method (keyword (str/lower-case method))
                      :script-name (.getPath base_uri)
                      :path-info path
                      :request-uri ""
                      :query-string ""
                      :server-name (.getHost base_uri)
                      :server-port (or (client/if-pos (.getPort base_uri)) (if (= scheme "https") 443 80))
                      :body (:payload user-params)
                      :params (dissoc user-params :payload)
                      :scheme scheme
                      :clj-spore-expected-status expected
                      :clj-spore-authentication authentication
                      :clj-spore-path-params (into [] (map #(-> % second keyword) (re-seq #":([^/]+)" path)))
                      :clj-spore-method-name method-name
                      :clj-spore-format format
                      :clj-spore-required-params req-params
                      :clj-spore-all-params all-params
                      :clj-spore-required-payload required_payload}]
             (wrapped-client env))))))))
