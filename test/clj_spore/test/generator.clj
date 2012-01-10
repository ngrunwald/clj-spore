(ns clj-spore.test.generator
  (:use [clj-spore.generator] :reload)
  (:use [clojure.test]
        [clojure.contrib.with-ns]
        [clojure.contrib.mock.test-adapter]))

(deftest params
  (testing "Missing Params"
    (is (= (check-missing-params ["foo" "bar"] {:foo 1 :bar 2 :baz 3}) '()) "None missing")
    (is (= (check-missing-params ["foo" "bar"] {:foo 1 :baz 3}) '("bar")) "bar missing"))
  (testing "Interpolation"
    (is (= (interpolate-params "/:foo/fifi/:baz" {:foo "riri" :baz "loulou"}) "/riri/fifi/loulou")
        "Interpolating params")))

(defn filter-keys
  [coll ks]
  (reduce (fn [hm [k v]] (if (k ks) hm (dissoc hm k)))
          coll
          coll))

(deftest gen-test
  (let [api {:name "test" :author "NG" :version 0.1 :base_url "http://example.com/api" :format [:json]}
        method_name "test_method"
        m-spec-get {:method "GET" :path "/:foo/path" :optional_params ["bar"] :required_params ["foo"] :expected_status [200]}
        func-get (generate-spore-method api m-spec-get (str method_name "_get") [] {})
        m-spec-post {:method "POST" :path "/:foo/path" :optional_params ["bar"] :required_params ["foo"] :expected_status [200]}
        func-post (generate-spore-method api m-spec-post (str method_name "_post") [] {})]
    (expect [clj-http.core/request (calls (fn [arg] (assoc (filter-keys arg #{:request-method :scheme :server-name :uri :query-string :body}) :status 200)))]
            (testing "Mocked requests"
              (is (function? func-get) "get function generated")
              (is (=
                   (func-get :foo "foo1" :bar "bar1")
                   {:request-method :get, :scheme "http", :server-name "example.com",
                    :uri "/api/foo1/path", :query-string "bar=bar1" :body nil :status 200})
                  "get request params are good")
              (is (function? func-post) "post function generated")
              (is (=
                   (func-post :foo "foo1" :bar "bar1" :payload "toto")
                   {:request-method :post, :scheme "http", :server-name "example.com",
                    :uri "/api/foo1/path", :query-string "bar=bar1" :body "toto" :status 200})
                  "post request params are good")))))
