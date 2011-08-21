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
  [cli spec]
  (if (fn? spec)
    (spec cli)
    (let [[mw & args] spec]
      (apply mw cli args))))

(defn wrap-middlewares
  [client & [middlewares]]
  (reduce (fn [cli mw] (apply-middleware cli mw)) client middlewares))

(defn wrap-interpolate-path-params
  [client]
  (fn [{:keys [path-info params script-name] :as request}]
    (let [path-params (:clj.spore.path-params request)
          fixed-path (interpolate-params path-info params)
          query-params (apply (partial dissoc params) path-params)
          uri (str script-name fixed-path)
          env* (assoc request :query-params query-params :uri uri :uri-string uri)]
      (client env*))))

(defn wrap-trace
  [client]
  (fn [req]
    (println req)
    (client req)))

(def base-client
  (-> #'core/request
      (wrap-trace)
      (client/wrap-query-params)
      (wrap-interpolate-path-params)
      (client/wrap-redirects)
      (client/wrap-output-coercion)
      (client/wrap-input-coercion)
      (client/wrap-content-type)
      (client/wrap-accept)))

(defn generate-spore-method
  ([{:keys [name author version], api_base_url :base_url, api_format :format
     :or {api_format []}
     :as api}
   {:keys [method path params required expected description authentication base_url format documentation]
    :or {description (str method-name " method")
         authentication false
         base_url api_base_url
         format (or api_format [])
	 params []
	 required []
	 expected []
         documentation (str "no documentation for " method-name)}
    :as spec}
   method-name
   middlewares]
     (let [wrapped-client (wrap-middlewares base-client middlewares)]
     (fn
       ^{:doc documentation, :method-name method-name, :authentication authentication}
       [& {:as user-params}]
       (let [missing (check-missing-params required user-params)]
         (if-not (empty? missing)
           nil
           ;; (Response. 599 {:error (str "missing params calling " method_name ": " (str/join ", " missing))})
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
                      :spore.expected-status expected
                      :clj.spore.authentication authentication
                      :clj.spore.path-params (into [] (map #(-> % second keyword) (re-seq #":([^/]+)" path)))
                      :clj.spore.method-name method-name
                      :clj.spore.format format}]
             (wrapped-client env))))))))
