(ns clj-spore
  (:require [clojure.contrib.string :as str]
            [clj-json.core :as json]
            [clj-spore.generator :as gen]
            [clj-spore.middleware :as mid]))

(defn load-spec-from-json
  [json & [mws]]
  (let
      [spec (json/parse-string json true)
       methods (spec :methods)
       api-desc (dissoc spec :methods)]
    (reduce (fn [coll [m-name m-spec]] (assoc coll (name m-name) (gen/generate-spore-method api-desc m-spec m-name (or mws []))))
            {}
            methods)))

(defn load-spec-from-file
  [filepath & [mws]]
  (load-spec-from-json (slurp filepath) (or mws [])))

(defn intern-spec
  [spec ns-name]
  (let [client-ns (create-ns ns-name)]
    (doseq [[m-name method] spec]
      (println m-name method)
      (intern client-ns (symbol m-name) method))
    client-ns))

(defn create-client-from-file
  ([filepath ns-name]
     (create-client-from-file filepath ns-name []))
  ([filepath ns-name mws]
     (let [spec (load-spec-from-file filepath mws)]
       (intern-spec spec ns-name))))

