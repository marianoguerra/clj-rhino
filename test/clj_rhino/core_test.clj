(ns clj-rhino.core-test
  (:import [org.mozilla.javascript Context UniqueTag EvaluatorException NativeArray]
           [org.marianoguerra.rhino TimeOutError])
  (:use clojure.test)
  (:require [clj-rhino :as js]))

(defn- assert-var-undefined [scope]
  (fn [name]
    (js/eval scope (str "a = typeof " name))
    (is (= (js/get scope "a") "undefined"))))

(defn- check-array-to-js [arr scope ctx]
  (let [js-arr (js/to-js arr scope ctx)]
    (is (= (count js-arr) (count arr)))
    (is (= (class js-arr) NativeArray))
    (dorun (map (fn [[item js-item]]
                  (is (= (js/to-js item scope ctx) js-item)))
                (map vector arr js-arr)))))

(defn- check-array-to-js-no-ctx [arr scope]
  (let [js-arr (js/to-js arr scope)]
    (is (= (count js-arr) (count arr)))
    (is (= (class js-arr) NativeArray))
    (dorun (map (fn [[item js-item]]
                  (is (= (js/to-js item scope) js-item)))
                (map vector arr js-arr)))))

(defn- is-identity [value]
  (is (= (js/from-js value) value)))

(defn- assert-simetric-convertion [obj1 scope ctx obj2]
  (is (= (js/from-js (js/to-js obj1 scope ctx)) obj2)))

(deftest js-test
  (testing "undefined? works"
           (is (not (js/undefined? 1)))
           (is (js/undefined? (. UniqueTag NOT_FOUND))))

  (testing "get returns undefined if get'ing inexistent var"
           (let [scope (js/new-scope)]
             (is (js/undefined? (js/get scope "foo")))))

  (testing "get returns default if get'ing inexistent var and default provided"
           (let [scope (js/new-scope)]
             (is (= (js/get scope "foo" 4) 4))))

  (testing "safe scope doesn't have dangerous references"
           (let [scope (js/new-safe-scope)
                 unsafe-scope (js/new-scope)]

             (js/eval scope "a = typeof With")
             (js/eval unsafe-scope "a = typeof With")
             (is (= (js/get scope "a") "undefined"))
             (is (= (js/get unsafe-scope "a") "function"))

             (dorun (map (assert-var-undefined scope) js/insecure-vars))))

  (testing "set function sets a variable in the scope"
           (let [scope (js/new-safe-scope)]
             (js/set! scope "a" 2)
             (is (= (js/get scope "a") 2))))

  (testing "functions can be compiled"
           (let [scope (js/new-safe-scope)
                 code "function (a, b) { return a + b; }"
                 compiled-fun (js/compile-function scope code :filename "foo.js")]
             (js/set! scope "add" compiled-fun)
             (is (= (js/eval scope "add(1, 2)") 3.0))))

  (testing "root scope doesn't allow var modifications"
           (let [scope (js/new-safe-root-scope)]
             (is (thrown? EvaluatorException (js/eval scope "Object = null")))))

  (testing "code can be evaled"
           (let [scope (js/new-scope)]
             (js/eval scope "a = 1;")
             (is (= (js/get scope "a") 1.0))

             (js/eval scope "s = 'hello';")
             (is (= (js/get scope "s") "hello"))

             (js/eval scope "o = {name: 'mariano'};")
             (is (= (js/get (js/get scope "o") "name") "mariano"))
             (is (= (js/get-in scope [:o :name]) "mariano"))

             (js/eval scope "b = false")
             ; TODO: this fails
             ; (is (= (js/get-in scope [:b :name] :not-found) :not-found))
             (is (= (js/get scope "b") false))))

  (testing "to-js works"
           (js/with-context
             (fn [ctx]
               (let [scope (js/new-safe-scope)]

                 (is (= (js/to-js nil scope ctx) nil))
                 (is (= (js/to-js 1 scope ctx) 1))
                 (is (= (js/to-js 1/2 scope ctx) 0.5))
                 (is (= (js/to-js true scope ctx) true))
                 (is (= (js/to-js "foo" scope ctx) "foo"))
                 (is (= (js/to-js :foo scope ctx) "foo"))

                 (check-array-to-js [] scope ctx)
                 (check-array-to-js [nil 1 1/2 true "foo" :foo] scope ctx)
                 (check-array-to-js '(nil 1 1/2 true "foo" :foo) scope ctx)

                 ; check to-js-generic
                 (check-array-to-js (to-array [nil 1 1/2 true "foo" :foo]) scope ctx)

                 (is (= (get (js/to-js {:name "mariano"} scope ctx) "name") "mariano"))
                 (is (= (get (js/to-js {:name "mariano" :age 27} scope ctx) "age") 27))))))

  (testing "to-js works without ctx"
           (let [scope (js/new-safe-scope)]

             (is (= (js/to-js nil scope) nil))
             (is (= (js/to-js 1 scope) 1))
             (is (= (js/to-js 1/2 scope) 0.5))
             (is (= (js/to-js true scope) true))
             (is (= (js/to-js "foo" scope) "foo"))
             (is (= (js/to-js :foo scope) "foo"))

             (check-array-to-js-no-ctx [] scope)
             (check-array-to-js-no-ctx [nil 1 1/2 true "foo" :foo] scope)
             (check-array-to-js-no-ctx '(nil 1 1/2 true "foo" :foo) scope)

             ; check to-js-generic
             (check-array-to-js-no-ctx (to-array [nil 1 1/2 true "foo" :foo]) scope)

             (is (= (get (js/to-js {:name "mariano"} scope) "name") "mariano"))
             (is (= (get (js/to-js {:name "mariano" :age 27} scope) "age") 27))))

  (testing "to-js works on complex nested structures"
           (js/with-context
             (fn [ctx]
               (let [scope (js/new-safe-scope)
                     obj {:name "bob"
                          :age 27
                          :tags ["yellow" :sponge ["a" :b 3.2]]
                          :friends {:sandy "squirrel"
                                    :patrick "star"}}
                     js-obj (js/to-js obj scope ctx)]

                 (is (= (get js-obj "name") "bob"))
                 (is (= (get js-obj "age") 27))
                 (is (= (.get (get js-obj "tags") 1) "sponge"))
                 (is (= (.get (.get (get js-obj "tags") 2) 1) "b"))
                 (is (= (get-in js-obj ["friends" "sandy"]) "squirrel"))))))

  (testing "unknown types throw exception when converting to-js"
           (js/with-context
             (fn [ctx]
               (let [scope (js/new-safe-scope)]
                 (is (thrown? Exception (js/to-js (atom {}) scope ctx)))))))

  (testing "from-js works"
           (is-identity nil)
           (is-identity 1)
           (is-identity 1.2)
           (is-identity "asd")
           (is-identity true)

           (js/with-context
             (fn [ctx]
               (let [scope (js/new-safe-scope)]
                 (is (= (js/from-js UniqueTag/NOT_FOUND) nil))
                 (assert-simetric-convertion [] scope ctx [])
                 (assert-simetric-convertion [1] scope ctx [1])
                 (assert-simetric-convertion [1 nil] scope ctx [1 nil])
                 (assert-simetric-convertion [1 nil 1.2] scope ctx [1 nil 1.2])
                 (assert-simetric-convertion [1 nil 1.2 "asd"] scope ctx 
                                             [1 nil 1.2 "asd"])
                 (assert-simetric-convertion [1 nil 1.2 "asd" true] scope ctx
                                             [1 nil 1.2 "asd" true])

                 (assert-simetric-convertion {} scope ctx {})
                 (assert-simetric-convertion {:b 1} scope ctx {:b 1})
                 (assert-simetric-convertion {:b 1 "c" true} scope ctx
                                             {:b 1 :c true})
                 (assert-simetric-convertion {:b 1 "c" true :foo nil} scope ctx
                                             {:b 1 :c true :foo nil})
                 (assert-simetric-convertion {:b {:c {:d 4}}} scope ctx
                                             {:b {:c {:d 4}}})
                 (assert-simetric-convertion {:b {"c" {:d 4}}} scope ctx
                                             {:b {:c {:d 4}}})
                 (assert-simetric-convertion {:b {"c" {:d [{"e" 4}]}}} scope ctx
                                             {:b {:c {:d [{:e 4}]}}})
                 ))))

  (testing "simple clojure functions can be added to js env"
           (js/with-context
             (fn [ctx]
               (let [scope (js/new-safe-scope)
                     add (fn [a b] (+ a b))]
                 
                 (js/set! scope "add" (js/wrap-plain-fn add))
                 (is (= (js/eval scope "add(1, 2)") 3.0))))))

  (testing "eval-timeout times out on infinite loop"
           (let [scope (js/new-safe-scope)]
             (is (thrown? TimeOutError (js/eval-timeout scope "while (true);" 1000)))))

  (testing "call-timeout works"
           (let [scope (js/new-safe-scope)
                 fun-code "function (a, b) { return a + b; }"
                 compiled-fun (js/compile-function scope fun-code)
                 result (js/call-timeout scope compiled-fun 1000 2 3)]

             (is (= result 5.0))))

  (testing "call-timeout timeouts correctly"
           (let [scope (js/new-safe-scope)
                 fun-code "function (a, b) { while(true); }"
                 compiled-fun (js/compile-function scope fun-code)]

             (try
               (js/call-timeout scope compiled-fun 1000 2 3)
               (throw (Exception. "shouldn't reach this point, should timeout"))
               (catch TimeOutError error
                 (is (= (.-timeoutMillis error) 1000))))))

  (testing "native clojure functions can be added"
           (js/with-context
             (fn [ctx]
               (let [scope (js/new-safe-scope)
                     add (fn [ctx scope this [a b]] (+ a b))
                     multiply (fn [ctx scope this [a b]] (* a b))
                     api {:add add :multiply multiply}]
                 (js/set! scope "add" (js/make-fn add))
                 (js/set! scope "api" (js/to-js api scope ctx))

                 (is (= (js/eval scope "add(1, 2)") 3.0))
                 (is (= (js/eval scope "api.add(1, 2) + api.multiply(2, 3)") 9.0)) 
                 ))))

)

