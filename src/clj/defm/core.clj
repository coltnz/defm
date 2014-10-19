(ns defm.core
  (:import (java.util Comparator))
  (:require [clojure.tools.macro :refer [name-with-attributes]]
            [clojure.walk :refer [postwalk]]))

(def primitive-sym? #{float double boolean byte char short int long
                      floats doubles booleans bytes chars shorts ints longs})

(clojure.core/defn valid-tag? [env tag]
  (and (symbol? tag) (or (primitive-sym? tag) (class? (resolve env tag)))))

(defn params-vec [size]
  "[_1 _2 .. _`size`]"
  (vec (for [i (range size)] (symbol (str "_" (inc i))))))

(defn type? [match]
  (and (symbol? match)
       (or (class? (ns-resolve *ns* match))
           (primitive-sym? match))))

(defn ->test [match args]
  (println "match is " match)
  (println "args is " args)
  (let [test (map
               (fn [m a]
                 (cond
                   (type? m) (list 'instance? m a)
                   (symbol? m) :else
                   :else (list '= a m)))
               match args)
        test (if (second test) (remove :else test) test)
        test (if (second test) (cons 'and test) (first test))]
    (println "test is " test)
    test))

(defn ->locals [match params]
  (vec (mapcat
         (fn [m p]
           (if (and (not (type? m)) (symbol? m)) [m p] []))
         match params)))

(defn bounds-err-> [match]
  (throw (RuntimeException. (str "Unreachable match " match))))

(defn check-bounds [matches]
  (loop [m (first matches)
         ms (next matches)
         bs #{}]
    (let [b (cond
              (and (symbol? m) (not-empty ms)) (bounds-err-> (first ms))
              (symbol? m) ::symbol
              (and (type? m) (contains? bs)) (bounds-err-> m)
              (type? m) m
              :else (if (contains? bs m) (bounds-err-> m) m))]
      (if (some? ms)
        (recur (first ms) (next ms) (conj bs b))))))

(defn matches->tests&locals [matches params]
  (check-bounds matches)
  (let [ts&ls (map
                (fn [match]
                  (if (= :- (second match))
                    [(->test (nnext match))
                     (->locals (first match) params)]
                    [(->test match params)
                     (->locals match params)]))
                matches)]
    (if-not (filter  #(or (true? %) (= :else %)) (first (butlast ts&ls)))
      (conj ts&ls [(list :else `(throw (IllegalArgumentException. (str "No match for " ~params)))) []])
      ts&ls)))

(defn ->cond [arity matches exprs]
  "Each arity shares a cond.
   [ [String] [Integer] ] => (instance "
  (let [params (params-vec arity)
        ts&ls (doall (matches->tests&locals matches params))
        _ (println "ts&ls" ts&ls)
        clauses (mapcat
                  (fn [[t ls] expr]
                    (println "LS " ls)
                    (list t (if (empty? ls) (cons 'do expr) (concat `(let ~ls) expr))))
                  ts&ls exprs)]
    (println  "C " clauses " C")
    (list params (conj clauses 'cond))))

(defmacro defm
  [name & fdecl]
  (let [[name body] (name-with-attributes name fdecl)
        body (if (vector? (first body))
               (list body)
               body)
        m (-> name
              meta
              (assoc :arglists (list 'quote (@#'clojure.core/sigs body))))
        _ (println "Body from " body)
        body (postwalk
               (fn [form]
                 (if (and (list? form) (= 'recur (first form)))
                   (list 'recur (cons 'vector (next form)))
                   form))
               body)
        _ (println "Body to " body)
        _ (println "GB " (sort (vec (group-by #(count (first %)) body))))
        arity->m-pairs (vec (sort (vec (group-by #(count (first %)) body))))
        conds (map (fn [[a pairs]]
                     (->cond a (map first pairs) (map rest pairs)))
                   arity->m-pairs)]
    (clojure.pprint/pprint conds)
    `(defn ~name ~m
       ~@conds)))

(defmacro defm-
  "As defm, but not public"
  [name & decls]
  (list* `defm (vary-meta name assoc :private true) decls))
