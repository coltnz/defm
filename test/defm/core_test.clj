(ns defm.core-test
  (:require [clojure.test :refer :all]
            [defm.core :refer :all]))

(deftest test-expr-fn
  (testing "Normal function."
    (defm square
      "Square function"
      [x]
      (* x x))
    (is (= 4 (square 2)))
    (is (= 9 (square 3)))
    (is (thrown? IllegalArgumentException (square 3 4)))))

(deftest test-exprs
  (testing "Dispatch on expr value"
    (defm echo
      "Echo."
      ([:a] "a")
      ([:b] "b")
      ([:c] "c")
      ([other] (str "-" other)))
    (is (= "a" (echo :a)))
    (is (= "b" (echo :b)))
    (is (= "c" (echo :c)))
    (is (= "-other" (echo "other")))))

;(deftest test-recursive-function
;  (testing "accum"
;    (defm accum
;      ([0 ret] ret)
;      ;([n ret] (recur (dec n) (+ n ret)))
;      ([n] (recur n 0)))
;    (is (= 6 (accum 3)))
;    (is (= 5050 (accum 100))))
;  (testing "fib"
;    (defm fib
;      ([0] 0)
;      ([1] 1)
;      ([n] (+ (fib (- n 1)) (fib (- n 2)))))
;    (is (= 55 (fib 10)))))
;
;(deftest test-guards
;  (testing "funny"
;    (defm funny
;      ([(N :guard #(= 42 %))] true)
;      ([_] false))
;    (is (funny 42))
;    (is (not (funny 43))))
;  (testing "valid-geopoint?"
;    (defm valid-geopoint?
;      ([(_ :guard #(and (> % -180) (< % 180)))
;        (_ :guard #(and (> % -90) (< % 90)))] true)
;      ([_ _] false))
;    (is (valid-geopoint? 30 30))
;    (is (not (valid-geopoint? -181 30)))))

(deftest test-match-literals
  (testing "test1"
    (defm test1
    ([true false] 1)
    ([true true] 2)
    ([false true] 3)
    ([false false] 4))
    (is (= 2 (test1 true true)))
    (is (= 4 (test1 false false)))))

;(deftest test-match-vector
;  (testing "test3"
;    (defm test3
;    ([[_ _ 2]] :a0)
;    ([[1 1 3]] :a1)
;    ([[1 2 3]] :a2))
;    (is (= :a2 (test3 [1 2 3])))
;    (is (= :a0 (test3 [3 3 2])))
;    (is (= :a1 (test3 [1 1 3])))))
;  (testing "test2"
;    (defm test2
;    ([([1] :seq)] :a0)
;    ([([1 2] :seq)] :a1)
;    ([([1 2 nil nil nil] :seq)] :a2))
;    (is (= :a2 (test2 [1 2 nil nil nil])))))

(deftest test-private
  (testing "private macro"
    (defm- test1
      ([_]))
    (is (:private (meta #'test1)))))

(deftest test-side-effects
  (testing "side-effects"
    (defm.core/defm test-se
      ([1] 1)
      ([x] (println "squaring")
           (* 2 x)))
    (with-out-str
      (is (= 4 (test-se 2))))
    (is (= "squaring") (with-out-str (test-se 2)))))

(deftest test-meta
  (testing "meta"
    (defm hello
      "hello world"
      ([name] (str "hello," name))
      ([a b] "unknown."))
    (is (= "hello world" (-> #'hello meta :doc)))
    (is (= '([name] [a b])) (-> #'hello meta :arglists))))
