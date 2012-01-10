(ns clj-spore
  (:require [cheshire.core :as json]
            [clj-spore.generator :as gen]
            [clj-spore.middleware :as mid]))

(defn load-method
  [desc spec name & {:keys [middlewares overload options] :or {middlewares [] overload {} options {}}}]
  (let [merged-spec (merge spec overload)]
    (gen/generate-spore-method desc spec name middlewares options)))

(defn load-spec
  [spec & {:keys [middlewares overload options] :or {middlewares [] overload {} options {}}}]
  (let
      [methods (spec :methods)
       api-desc (merge (dissoc spec :methods) overload)]
    (reduce (fn [coll [m-name m-spec]] (assoc coll (keyword m-name) (load-method api-desc m-spec m-name :middlewares middlewares :overload overload :options options)))
            {}
            methods)))

(defn load-spec-from-json
  [json & {:keys [middlewares overload options] :or {middlewares [] overload {} options {}}}]
  (let
      [spec (json/parse-string json true)]
    (load-spec spec :middlewares middlewares :overload overload :options options)))

(defn load-spec-from-file
  [filepath & {:keys [middlewares overload options] :or {middlewares [] overload {} options {}}}]
  (load-spec-from-json (slurp filepath) :middlewares middlewares :overload overload :options options))
