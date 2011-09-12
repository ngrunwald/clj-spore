(ns clj-spore.test.core
  (:use [clj-spore] :reload)
  (:use [clj-spore.middleware] :reload)
  (:use [clojure.test]
        [clojure.contrib.with-ns]
        [clojure.contrib.mock.test-adapter]))

(defn utf8-bytes
  "Returns the UTF-8 bytes corresponding to the given string."
  [#^String s]
  (.getBytes s "UTF-8"))

(deftest test-load-spec
  (let [client (load-spec-from-file "test/ihackernews.json")]
    (expect [clj-http.core/request (calls (fn [arg] {:status 200}))]
            (testing "Mocked requests"
              (is (map? client) "client generated")
              (is (function? (client :askhn_posts)) "function generated")
              (is (=
                   ((client :askhn_posts))
                   {:status 200})
                  "request works")))))

(deftest test-client-creation
  (expect [clj-http.core/request (calls (fn [req] {:status 200 :body (:body req) :headers { "content-type" "application/json" } :request req}))]
          (testing "Simple middlewares"
            (let [client-simple (load-spec-from-file "test/ihackernews.json" :middlewares [wrap-json-format wrap-runtime])
                  res ((client-simple :vote) :payload {:bar "barbu"})]
              (is (= (:status res) 200) "request works")
              (is (and (get-in res [:headers "x-spore-runtime"])) "runtime middleware works")
              (is (= (get-in res [:headers "content-type"]) "application/json") "content-type response header is good")
              (is (= (get (:decoded-body res) "bar") "barbu") "decoded-body is good")
          (testing "Simple overload"
            (let [client-overload (load-spec-from-file "test/ihackernews.json" :overload {:base_url "http://grunwald.fr"})
                  res ((client-overload :vote))]
              (is (= (:status res) 200) "request works")
              (is (= (:server-name (:request res)) "grunwald.fr") "overloading works")))))))
