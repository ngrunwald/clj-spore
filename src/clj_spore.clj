(ns clj-spore
  (:require [clojure.contrib.string :as str]
            [clj-json.core :as json]
            [clj-spore.generator :as gen]
            [clj-spore.middleware :as mid]))

(defn load-spec-from-json
  [json & [mws]]
  (let
      [spec (json/parse-string json true)
       methods (:methods spec)
       api-desc (dissoc spec :methods)]
    (reduce (fn [coll [m-name m-spec]] (assoc coll m-name (gen/generate-spore-method api-desc m-spec m-name (or mws []))))
            {}
            methods)))

(defn load-spec-from-file
  [filepath & [mws]]
  (load-spec-from-json (slurp filepath) (or mws [])))
