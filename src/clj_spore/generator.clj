(ns clj-spore.generator
  (:require [clojure.contrib.string :as str]
            [clj-http.core :as c])
  (:import (java.net URL)))

(def *api*)

(def *middlewares* [])

(defn- check-missing-params
  [required params]
  (let [str-params (map #(name %) (keys params) )]
     (filter #(not (.contains str-params %)) required)))

(defn- wrap-response
  [response callbacks]
  response)

(defn interpolate-params
  [path params]
  (reduce (fn [p [name val]] (str/replace-str (str name) val p)) path params))

(defn finalize-request
  [{ path :PATH_INFO, params :spore.params :as env}]
  (assoc env :PATH_INFO (interpolate-params path params)))

(defn- send-request
  [{ method :METHOD_NAME, host :SERVER_HOST, port :SERVER_PORT :as env} callbacks]
  (println "sent")
  (c/request {}))

(defn- wrap-request
  [init-env middlewares]
  (loop [env init-env
	 mw middlewares
	 callbacks []]
    (if-let
	[middleware (first mw)]
      (let [res (middleware env)]
	(if-let [response (:response res)]
	  (wrap-response response callbacks)
	  (recur (:env res)
		 (rest mw)
		 (if-let [cb (:cb res)] (conj callbacks cb) callbacks))))
      (send-request (finalize-request env) callbacks))
    ))

(defn generate-spore-method
  ([method_name spec]
     (generate-spore-method *api* method_name spec))
  ([{:keys [name author version], api_base_url :base_url, api_format :format
     :or {api_format []}
     :as api}
   method_name
   {:keys [method path params required expected description authentication base_url format documentation]
    :or {description (str method_name " method")
         authentication false
         base_url api_base_url
         format (or api_format [])
	 params []
	 required []
	 expected []
         documentation (str "no documentation for " method_name)}
    :as spec }]
     (fn
       [& {:as user-params}]
       (let [missing (check-missing-params required user-params)]
         (if-not (empty? missing)
           nil
           ;; (Response. 599 {:error (str "missing params calling " method_name ": " (str/join ", " missing))})
           (let [base_uri (URL. base_url)
                 env {:METHOD_NAME method
                      :SCRIPT_NAME (.getPath base_uri)
                      :PATH_INFO path
                      :REQUEST_URI ""
                      :SERVER_HOST (.getHost base_uri)
                      :SERVER_PORT (.getPort base_uri) 
                      :QUERY_STRING ""
                      :spore.payload (:payload user-params)
                      :spore.params (dissoc user-params :payload)
                      :spore.scheme (.getProtocol base_uri)}]
;             env
             (wrap-request env *middlewares*)
             ))))))
