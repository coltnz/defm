(ns defm.core-test
  (:import (java.io File)
           (java.net URL)
           [clojure.lang Keyword])
  (:require [clojure.test :refer :all]
            [defm.core :refer :all]))

(set! *warn-on-reflection* true)

(deftest test-exprs
  (testing "Dispatch on value"
    (defm exprs
          "Exprs."
          ([:a] "a")
          ([:b] "b")
          ([:c] "c")
          ([_] (str _1)))
    (is (= "a" (exprs :a)))
    (is (= "b" (exprs :b)))
    (is (= "c" (exprs :c)))
    (is (= "other" (exprs "other")))))

(deftest test-fn
  (testing "Normal function."
    (defm square
          "Square function"
          [x]
          (* x x))
    (is (= 4 (square 2)))
    (is (= 9 (square 3)))
    (is (thrown? IllegalArgumentException (square 3 4)))))

(deftest test-bindings
  (testing "symbols are bound to params"
    (defm symbol-fn
          (["f"] "F")
          (["f" s] (str "f" s))
          ([s "f"] (str s "f"))
          ([s1 "f" s2] (str s1 "f" s2))
          ([s] s))
    (is (= "F" (symbol-fn "f")))
    (is (= "fS" (symbol-fn "f" "S")))
    (is (= "Sf" (symbol-fn "S" "f")))
    (is (= "thisiss" (symbol-fn "thisiss")))))

(deftest test-types
  (testing "symbols can be bound to types to restrict args"
    (defm binds-fn
          ([String] _1)
          ([s :- String "S"] (str s "S"))
          ([d1 :- Double Double] (str d1 _2)))
    (is (= "F" (binds-fn "F")))
    (is (= "fS" (binds-fn "f" "S")))
    (is (= "thisiss" (binds-fn "thisiss")))))

(deftest test-nil
  (testing "Match on nil"
    (defm nil-chk
          ([:a] "a")
          ([nil] (str _1))
          ([[a b]] _1))
    (is (= "" (nil-chk nil)))
    (is (= ["a" nil] (nil-chk ["a" nil])))))

(deftest test-rest-param
  (testing "Match & to rest of args"
    (defm restp
          ([Keyword] (str _1))
          ([a & b] [a b]))
    (is (= ":a" (restp :a)))
    (is (= [:b [:c]] (restp :b :c)))
    (is (= [:d [:e :f]] (restp :d :e :f)))))

(deftest test-shape
  (testing "Test clause configuration"
    ;(is (thrown? ArityException (eval '(defm))))
    ;(is (thrown? IllegalArgumentException (eval '(defm []))))
    (is (= :b (do (defm blah [:a] :b) (blah :a))))
    ;(is (thrown? IllegalArgumentException (eval '(defm blah2 []))))
    (is (= ((defm blah [] [])) []))))

;(is (thrown? RuntimeException (eval '(defm un1 (["C" :a] 1) ([:b] 2) (["C" :a] 2)))))))

(deftest test-patterns
  (testing "Patterns work as a match and error if not str expr"
    (defm patmatch
          [#"x"] :x))
  (is (= :x (patmatch "x")))
  (is (thrown? ClassCastException (patmatch 4))))

(deftest test-default-else
  (testing "If no default clause we'll add one"
    (defm defaults-fn
          ([:a] :A))
    (is (= :A (defaults-fn :a)))
    (is (thrown? IllegalArgumentException (defaults-fn :b)))))

(deftest mixed
  (testing "Mixed and anon types"
    (defm mixed-fn
          ([File] (str "File " _1))
          ([String] (str "String " _1))
          ([_] (.getName ^Class (type _1))))
    (is (= "String aString" (mixed-fn "aString")))
    (is (= "File aFile" (mixed-fn (File. "aFile"))))
    (is (= "java.net.URL" (mixed-fn (URL. "http://url"))))))

(deftest test-match-literals
  (testing "test 1iterals"
    (defm test1
          ([true false] 1)
          ([true true] 2)
          ([false true] 3)
          ([false false] 4))
    (is (= 2 (test1 true true)))
    (is (= 4 (test1 false false)))))

(deftest test-private
  (testing "private macro"
    (defm- test1
           ([_] :blah))
    (is (:private (meta #'test1)))))

(deftest test-side-effects
  (testing "side-effects"
    (defm test-se
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

(deftest test-wildcards
  (testing "multi-wildcards"
    (defm mw
          ([_ _] 2)
          ([a _ b] 3)
          ([_] 1))
    (is (= 2 (mw 1 2)))
    (is (= 3 (mw 1 2 3)))
    (is (= 1 (mw 4)))))

(deftest test-:seq
  (testing ":seq tests"
    (defm mseq
          ([:seq] "s")
          ([h :seq] [[h] _2])
          ([:seq t] [[_1] t]))
    (is (= "s" (mseq [:a])))
    (is (= [[:a] [:b :c]] (mseq :a [:b :c])))
    (is (= [[:d :e] :f] (mseq [:d :e] :f)))))

(deftest test-recursive
  (testing "count down"
    (defm count-down
          ([0] "zero!")
          ([n] (println n)
           (recur (dec n))))
    (is (= (count-down 0) "zero!"))
    (with-out-str (is (= (count-down 6) "zero!"))))
  (testing "fib"
    (defm fib
          ([0] 0)
          ([1] 1)
          ([n] (+ (fib (- n 1)) (fib (- n 2)))))
    (is (= (fib 0) 0))
    (is (= (fib 1) 1))
    (is (= (fib 10) 55))))

(deftest test-typed-vectors
  (testing "typed vectors"
    (defn walk-types [f]
      (clojure.walk/postwalk
        (fn [x]
          (if (not (or (map? x) (sequential? x)))
            (type x)
            (if (type? x)
              (ns-resolve *ns* x) x))) f))
    (defm typed-vecs
          ([[String]] {:strings (walk-types _1)})
          ([[Number]] {:numbers (walk-types _1)})
          ([[Long] [Number]] {:numberss (map walk-types [_1 _2])})
          ([[Long] [Number] [[[Double]]]] {:numberssss (map walk-types [_1 _2 _3])}))
    (is (= {:strings [String String]} (typed-vecs ["a" "b"])))
    (is (= {:numbers [Long Long Long Long]} (typed-vecs [4 3 2 1])))
    (is (= {:numberss [[Long Long] [Long Long]]} (typed-vecs [1 2] [3 4])))
    (is (= {:numberssss [[Long Long] [Long] [[[Double]]]]} (typed-vecs [1 2] [3] [[[4.0]]])))))

(deftest test-value-vectors
  (testing "value vectors"
    (defm val-vecs
          ([[a]] a)
          ([[a b]] [a b])
          ([[a b & c]] [a b c]))
    (is (= 1 (val-vecs [1])))
    (is (= [:a :b] (val-vecs [:a :b])))
    (is (= [1 2 [3 4 5 6]] (val-vecs [1 2 3 4 5 6])))))

;;;;;;;;;
;; Reachability checking
;;

(deftest test-unreachable-exprs
  (testing "unreachable clauses with symbols"
    (is (thrown? RuntimeException (eval '(defm un1 ([:a] 1) ([:b] 2) ([:a] 2)))))
    (is (thrown? RuntimeException (eval '(defm un1 (["C" :a] 1) ([:b] 2) (["C" :a] 2)))))))

(deftest test-unreachable-symbols
  (testing "unreachable clauses with symbols"
    (is (thrown? RuntimeException (eval '(defm un2 ([a] 1) ([:a] 2)))))
    (is (thrown? RuntimeException (eval '(defm un3 ([a] 1) ([b] 2)))))
    (is (thrown? RuntimeException (eval '(defm un3 ([_] 1) ([b] 2)))))
    (is (thrown? RuntimeException (eval '(defm un3 ([b] 1) ([_] 2)))))
    (is (thrown? RuntimeException (eval '(defm un3 ([:else] 1) ([_] 2)))))
    (is (thrown? RuntimeException (eval '(defm un3 ([:a] 0) ([:else] 1) ([:else] 2)))))))

(deftest test-unreachable-types
  (testing "unreachable clauses with symbols"
    (is (thrown? RuntimeException (eval '(defm un1 ([Long] 1) ([Long] 2) ([:a] 2)))))
    (is (thrown? RuntimeException (eval '(defm un2 ([a] 1) ([Long] 2)))))
    (is (thrown? RuntimeException (eval '(defm un2 ([a :- Double] 1) ([a :- Double] 2)))))
    (is (thrown? RuntimeException (eval '(defm un2 ([a :- Long] 1) ([Long] 2)))))
    (is (thrown? RuntimeException (eval '(defm un3 ([:seq] 1) ([:seq] 2)))))))


(deftest test-map-types
  (testing "map types"
    (defm maptypes
          ([{String Long}] [(map type (keys _1)) (map type (vals _1))]))
    (is (= [[String String String] [Long Long Long]] (maptypes {"a" 4 "b" 5 "c" 6})))))

(deftest test-map-values
  (testing "map values"
    (defm mapvalues
          ([{:a 1}] "a1")
          ([{:b b}] (str "b" b))
          ([{Integer String}] (str "x")))
    (is (= "a1" (mapvalues {:a 1})))
    (is (= "b2" (mapvalues {:b 2})))))

(deftest test-vector-dest
  (testing "test3"
    (defm test3
          ([[_ _ 2]] :a0)
          ([[1 1 3]] :a1)
          ([[1 2 3]] :a2))
    (is (= :a2 (test3 [1 2 3])))
    (is (= :a0 (test3 [3 3 2])))
    (is (= :a1 (test3 [1 1 3])))))

(deftest test-as-dest                                             ;not supporting nested as for now
  (testing "as"
    (defm astest
          ([Long :as l] (str "l is " l))
          ([[4 5 6] :as nums] (str "nums is " nums))
          ([[Long] :as ls] (str "ls is " ls))
          ([{String String} :as m] (str "m is " m))
          ([{:c :d} :as m2] (str "m2 is " m2))
          )
    (is (= "l is 1" (astest 1)))
    (is (= "nums is [4 5 6]" (astest [4 5 6])))
    (is (= "ls is [7 8]" (astest [7 8])))
    (is (= "m is {\"a\" \"b\"}" (astest {"a" "b"})))
    (is (= "m2 is {:c :d}" (astest {:c :d})))))
