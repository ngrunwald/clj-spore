(ns clj-spore
  (:require [clojure.contrib.string :as str]
            [clj-json.core :as json]
            [clj-spore.generator :as gen]
            [clj-spore.middleware :as mid]))

(defn load-method
  [desc spec name & {:keys [middlewares overload] :or {middlewares [] overload {}}}]
  (let [merged-spec (merge spec overload)]
    (gen/generate-spore-method desc spec name middlewares)))

(defn load-spec
  [spec & {:keys [middlewares overload] :or {middlewares [] overload {}}}]
  (let
      [methods (spec :methods)
       api-desc (merge (dissoc spec :methods) overload)]
    (reduce (fn [coll [m-name m-spec]] (assoc coll (keyword m-name) (load-method api-desc m-spec m-name :middlewares middlewares :overload overload)))
            {}
            methods)))

(defn load-spec-from-json
  [json & {:keys [middlewares overload] :or {middlewares [] overload {}}}]
  (let
      [spec (json/parse-string json true)]
    (load-spec spec :middlewares middlewares :overload overload)))

(defn load-spec-from-file
  [filepath & {:keys [middlewares overload] :or {middlewares [] overload {}}}]
  (load-spec-from-json (slurp filepath) :middlewares middlewares :overload overload))
