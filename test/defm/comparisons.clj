(ns defm.comparisons
  (:require
    [defm.core :refer :all]
    [schema.core :as s]
    [defun :refer :all]
    [clojure.core.match :as m]))

;(def data (doall (repeatedly 1000000 #(if (> (rand) 0.5) (str (System/currentTimeMillis)) (System/currentTimeMillis)))))
;
;(defm hi
;      ([String] "s")
;      ([Long] 4))
;
;(dotimes [_ 3] (time (doall (map hi data))))
;
;(defun hi2
;       ([String] "s")
;       ([Long] 4))
;
;(dotimes [_ 3] (time (doall (map hi2 data))))
;
;
;(def data2 (doall (repeatedly 1000000 #(vector (str (System/currentTimeMillis)) (System/currentTimeMillis)))))

;(defm hi3
;      ([a :- String] a)
;      ([a :- String
;        b :- Long] b))
;(dotimes [_ 3] (time (doall (map #(apply hi3 %) data2))))
;
;(s/defn ^:always-validate hi4
;  ([a :- String] a)
;  ([a :- String
;    b :- Long] b))
;(dotimes [_ 3] (time (doall (map #(apply hi4 %) data2))))

(defn testa
  ([a] a)
  ([a b] b)
  ([a b & c] c)
  )


(let [x {:a 1 :b 1}]
  (println
    (m/match [x]
             [{:a 1 :b b}] :a1
             [{:a 1 :b 1}] :a0
             :else nil)))

(let [x {:a 1 :b 1}]
  (println
    (m/match [x]
             [{:a 1 :b _}] :a1
             [{:a 1 :b 1}] :a0
             :else nil)))

(let [x {:a 1 :b 1}]
  (println
    (m/match [x]
             [{:a 1 :b _1}] _1                              ; not _ though
             [{:a 1 :b 1}] :a0
             :else nil)))


(let [x {:a '_1 :b '_2}]
  (println
    (m/match [x]
             [{:a _1 :b _2}] 1)))

; DOESNT WORKx`
;(let [x {:a 1 :b 1}]
;  (println
;    (m/match [x]
;             [{_1 1 _2 2}] :a0
;             :else nil)))

;(s/validate StringScoreMap {1 {"2" 3.0 "3" [5.0]} 4.0 {}})

(let [x [1 2 ]]
  (m/match [x]
         [([1] :seq)] :a0
         [([1 2] :seq)] :a1
         [([1 2 nil nil nil] :seq)] :a2
         :else nil
         ))

(m/match [x] [([1] :seq)] :a0 [([1 2] :seq)] :a1 [([1 2 nil nil nil] :seq)] :a2 :else nil)
