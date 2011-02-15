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
        "Interpolating params"))
  (testing "make-query"
    (is (= (make-query {:foo 1, 2 3, "baz" "tété" }) "foo=1&2=3&baz=t%C3%A9t%C3%A9"))))

(deftest gen-test
  (let [api {:name "test" :author "NG" :version 0.1 :base_url "http://example.com/api" :format [:json]}
        method_name "test_method"
        m-spec {:method "GET" :path "/:foo/path" :params ["bar"] :required ["foo"] :expected 200}
        func (generate-spore-method api m-spec method_name [])]
    (expect [clj-http.core/request (calls (fn [arg] arg))] 
            (testing "Mocked request"
              (is (function? func) "function generated")
              (is (=
                   (func :foo "foo1" :bar "bar1")
                   {:request-method :get, :scheme "http", :server-name "example.com",
                    :uri "/api/foo1/path", :query-string "bar=bar1"})
                  "request params are good")
              ))))
