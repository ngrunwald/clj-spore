(ns clj-spore.generator
  (:use [clojure.contrib.str-utils :only [str-join]]))

(def *api*)

(defrecord Response [code env])

(defn- check-missing-params
  [required params]
  (let [str-params (map #(name %) (keys params) )]
     (filter #(not (.contains str-params %)) required)))

(defn generate-spore-method
  ([method_name spec]
     (generate-spore-method *api* method_name spec)) 
  ([{:keys [name author api_base_url api_format version]
     :or {api_format []}
     :as api}
   method_name
   {:keys [method path params required expected description authentication base_url format documentation]
    :or {description (str method_name " method")
         authentication false
	 params []
	 required []
	 expected []
         format []
         documentation (str "no documentation for " method_name)}
    :as spec }]
     (fn
       [& {:as user-params}]
       (let [missing (check-missing-params required user-params)]
	 (if-not (empty? missing)
	   (Response. 599 {:error (str "missing params calling " method_name ": " (str-join ", " missing))})
	   (println "gogogo!"))
	 ))))
