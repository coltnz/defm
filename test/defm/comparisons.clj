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