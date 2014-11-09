(ns defm.core
  (:require [clojure.walk :as walk]
            [clojure.pprint :as ppint])
  (:import [java.util.regex Pattern]
           [clojure.lang Symbol ILookup Sequential]))

;; # Motivations
;;
;; Fast, 'light' pattern dispatching for fns
;; Safe by default
;;  - reject if unreachable arg pattern discovered
;;  - if no default :else pattern provided one is generated to throw IllegalArgumentExcepton
;;  - if pattern used on items of large seq, stop checking after *max-same-matches* to prevent head holding

;; # Implementation
;;
;; * x and y are called _occurrences_
;; * 1, 2, 3 and 4 are _arg patterns_
;; * [1 2] and [3 4] are _pattern rows_
;; * :a0 and :a1 are _actions_
;; * _1 _2 _3 .. _n are the implicit parameters for an n-arg match


;inlined from tools.macro
(defn name-with-attributes
  [name macro-args]
  (if (instance? Symbol name)
    nil
    (throw (IllegalArgumentException. "First argument to defm must be a symbol")))
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
        [attr macro-args] (if (map? (first macro-args))
                            [(first macro-args) (next macro-args)]
                            [{} macro-args])
        attr (if docstring
               (assoc attr :doc docstring)
               attr)
        attr (if (meta name)
               (conj (meta name) attr)
               attr)]
    [(with-meta name attr) macro-args]))

(def primitive-sym? #{float double boolean byte char short int long
                      floats doubles booleans bytes chars shorts ints longs})

(defn valid-tag? [env tag]
  (and (symbol? tag) (or (primitive-sym? tag) (class? (resolve env tag)))))

(defn to-arg [idx]
  (symbol (str "_" idx)))

(defn default-args [size]
  "[_1 _2 .. _`size`]"
  (vec (for [i (range size)] (to-arg (inc i)))))

(defn pattern? [match]
  (= Pattern (type match)))

(defn map-like? [match]
  (instance? ILookup match))

(defn type? [match]
  (and (symbol? match)
       (or (class? (ns-resolve *ns* match))
           (primitive-sym? match))))


(defn bounds-err-> [match]
  (throw (RuntimeException. (str "Unreachable match " match))))

(defn subsumes [prior bounds]
  (every? true?
          (map
            (fn [p b]
              (flush)
              (cond
                (and (class? b) (class? p) (.isAssignableFrom b p)) true
                (= b p) true
                (= p ::symbol) true
                :else false))
            prior
            bounds)))

(defn process-bounds [bounded bounds matcher]
  (doseq [prior bounded]
    (if (subsumes prior bounds)
      (bounds-err-> (:params matcher))))
  (conj bounded bounds))

(defn check-bounds [matchers]
  (reduce #(process-bounds %1 (:bounds %2) %2) [] matchers))

;; To make a valid test from mexprs we merely need to `and` if plural and add :else if no default clause.
;; Additionally we remove redundant `true` matches.
(defn ->test [mexprs]
  (let [mexprs (if (second mexprs) (remove #(true? %) mexprs) mexprs)]
    (if (second mexprs)
      (cons 'and mexprs)
      (or (first mexprs) true))))

(defn ->cond [arity matchers]
  (check-bounds matchers)
  (let [args (default-args arity)
        cond-clauses (mapcat
                       (fn [matcher]
                         (let [matches (:matches matcher)
                               exprs (:exprs matcher)
                               test (->test (map :mexpr matches))
                               locals (vec (:locals matcher))]
                           (if (seq locals)
                             (concat (list test (concat (list 'let locals) exprs)))
                             (concat (list test) exprs))))
                       matchers)]
    (let [last-test (first (butlast cond-clauses))]
      (if (or (= last-test [:else]) (= last-test true))
        (list args (conj cond-clauses 'cond))
        (list args (conj (concat cond-clauses
                                 [:else `(throw (IllegalArgumentException.
                                                  (str "No match for " ~args)))]) 'cond))))))

(defn mask [what when? with]
  (walk/postwalk #(if (when? %) with %) what))


(defn type-check [type coll] (every? true? (map #(instance? type %) coll)))
(defn types-check [types coll] '(and (= (count types) (count coll)) (every? true? (map #(instance? %1 %2) types coll))))

;
;[a :- Long b :- String]
;[String Long]
;[a b [String]]
;[a :- Long b :- Long (or c String Long)]


(defn seq-match [arg param matches]
  (println arg param)
  (let [
        seq-mexprs (list 'and (list 'sequential? param) (list '= (list 'count param) (count matches)))
        mexprs (remove true? (map :mexpr matches))          ;guaranteed at least sequential condition
        seq-mexprs (if (seq mexprs)
                     (if (second mexprs)
                       (list 'and (conj mexprs seq-mexprs))
                       (first mexprs))
                     seq-mexprs)
        locals (remove nil? (mapcat :locals matches))]
    {:mexpr     seq-mexprs
     :locals    (seq locals)
     :bounds    arg
     :unmatched (:unmatched (last matches))}))

(declare type-match)

(defn next-match [args next-param]
  (let [[arg & more] args
        name (if (and (symbol? arg) (not (type? arg))) arg nil)
        match (or
                ;(type-match next-param arg more)
                (cond
                  (= '_ arg) {:mexpr true :bounds ::symbol :unmatched more}
                  (= '& arg) (if (second more) :error {:locals [(first more) next-param] :mexpr ::restarg :bounds ::restarg})
                  (= arg :else) {:mexpr :else :bounds ::symbol :unmatched more}
                  (= (first more) :-) {:locals [name next-param] :mexpr (list 'instance? (second more) next-param) :bounds (second more) :unmatched (drop 2 more)}
                  (some? name) {:locals [arg next-param] :mexpr true :bounds ::symbol :unmatched more}
                  (type? arg) {:mexpr (list 'instance? arg next-param) :bounds arg :unmatched more}
                  (pattern? arg) {:mexpr (list 're-matches arg next-param) :bounds arg :unmatched more}
                  (sequential? arg) (if (type? (first arg))
                                      (if (second arg)
                                        (throw (IllegalArgumentException. "Illegal form"))
                                        (seq-match arg next-param [{:mexpr `(type-check (first ~arg) ~next-param) :bounds arg :unmatched more}]))
                                      (seq-match arg next-param
                                                 (loop [ms []
                                                        arg arg
                                                        idx 0]
                                                   (let [m (next-match arg `(nth ~next-param ~idx))
                                                         um (:unmatched m)]
                                                     (if (empty? um)
                                                       (conj ms m)
                                                       (recur (conj ms m) um (inc idx)))))))
                  (map-like? arg) {:locals [arg next-param] :mexpr (list 'instance? ILookup next-param) :bounds (mask arg symbol? ::symbol) :unmatched more}
                  (= :seq arg) {:mexpr (list 'not (list 'instance? Sequential next-param)) :bounds ::seq :unmatched more}
                  :else {:mexpr (list '= next-param arg) :bounds arg :unmatched more}))]
    (if (nil? match) {:mexpr (list '= next-param arg) :bounds arg }  match)))

(defn ->matcher
  "For a match extract the type hints and annotations, and names if supplied."
  [params exprs]
  (println "\n")
  (let [[arity matches] (loop [ps params
                               ms []
                               a 0]
                          (if (empty? ps)
                            [a ms]
                            (let [m (next-match ps (to-arg (inc a)))]
                              (assert some? (:unmatched m))
                              (recur (:unmatched m) (conj ms m) (inc a)))))
        locals (mapcat :locals matches)
        bounds (map :bounds matches)]
    {:arity arity :matches matches :bounds bounds :params params :locals locals :exprs exprs}))



(defmulti type-match
          "Given a list of params extracts next match map with -
  :mexpr the expression to execute the match
  :bounds the scope of the match
  :unmatched the remainder of the params"
          (fn [next-arg next-param more]
            [next-arg next-param more]))


(defmacro defm
  [name & fdecl]
  (let [[name body] (name-with-attributes name fdecl)
        body (if (vector? (first body))
               (list body)
               body)
        fn-meta (-> name
                    meta
                    (assoc :arglists (list 'quote (@#'clojure.core/sigs body))))
        ;_ (map #(if (next body)
        ;         (second body)
        ;         (throw (IllegalArgumentException. "defm requires an even number of forms"))))
        _ (println "**** matching *** ")
        _ (clojure.pprint/pprint body)
        matchers (reduce (fn [matchers match-clause]
                           (let [[match-params & exprs] match-clause]
                             (conj matchers (->matcher match-params exprs))))
                         [] body)
        arity->matchers (group-by #(:arity %) matchers)
        conds (for [a (sort (keys arity->matchers))
                    :let [matchers (arity->matchers a)]]
                (->cond a matchers))]
    (clojure.pprint/pprint conds)
    `(defn ~name ~fn-meta
       ~@conds)))

(defmacro defm-
  "same as defm, yielding non-public def"
  [name & decls]
  (list* `defm (vary-meta name assoc :private true) decls))

(defn expand [a-defm]
  (clojure.pprint/pprint (macroexpand-1 a-defm)))

