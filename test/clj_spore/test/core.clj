(ns clj-spore.test.core
  (:use [clj-spore] :reload)
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [clj-spore.middleware] :reload)
  (:use [clojure.test]))

(deftest test-load-spec
  (let [client (load-spec-from-file "test/ihackernews.json")]
    (with-redefs
      {#'clj-http.core/request (fn [arg] {:status 200})}
      (testing "Mocked requests"
        (is (map? client) "client generated")
        (is (function? (client :askhn_posts)) "function generated")))))

(deftest test-client-creation
  (with-redefs
    [clj-http.core/request
     (fn [{:keys [body] :as req}]
       (let [b (if body
                 (let [l (.getContentLength body)
                       arr (byte-array l)]
                   (with-open [stream (.getContent body)]
                     (.read stream arr))
                   arr))]
         {:status 200
          :body b
          :headers {"content-type" (get-in req [:headers "accept"])}
          :request req}))]
    (testing "Simple middlewares"
      (let [client-simple (load-spec-from-file "test/ihackernews.json" :middlewares [wrap-json-format wrap-runtime])
            res ((client-simple :vote) :payload {:bar "barbu"})]
        (is (= (:status res) 200) "request works")
        (is (= (:status res) 200) "request works")
        (is (and (get-in res [:headers "x-spore-runtime"])) "runtime middleware works")
        (is (= (get-in res [:headers "content-type"]) "application/json") "content-type response header is good")
        (is (= (get (:decoded-body res) "bar") "barbu") "decoded-body is good")
        (testing "Simple overload"
          (let [client-overload (load-spec-from-file "test/ihackernews.json" :overload {:base_url "http://grunwald.fr"})
                res ((client-overload :vote))]
            (is (= (:status res) 200) "request works")
            (is (= (:server-name (:request res)) "grunwald.fr") "overloading works")))
        (testing "Complex middleware"
          (let [client-complex (load-spec-from-file
                                "test/ihackernews.json"
                                :middlewares [[wrap-clojure-response
                                               :enabled-if #(not= (:path %) "/login")
                                               :args [:type "application/x-clojure"]]])
                res-vote ((client-complex :vote))
                res-login ((client-complex :auth_token))]
            (is (= (:status res-vote) 200) "request works")
            (is (= (get-in (:request res-vote) [:headers "accept"]) "application/x-clojure"))
            (is (not= (get-in (:request res-login) [:headers "accept"]) "application/x-clojure")))))))
  (with-redefs
    [clj-http.core/request
     (fn [req]
       {:status 205
        :body (:body req)
        :headers {"content-type" (get-in req [:headers "accept"])}
        :request req})]
    (testing "Exception handling"
      (let [client-simple (load-spec-from-file "test/ihackernews.json")]
        (try+ ((client-simple :askhn_posts))
              (catch Object o
                (is (= (:type o) :unexpected-status))))
        (try+ ((client-simple :user_profile))
              (catch Object o
                (is (= (:type o) :missing-params))
                (is (= (:missing-params o) '("userid")))))))))
