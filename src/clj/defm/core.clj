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

(defn to-param [idx]
  (symbol (str "_" idx)))

(defn params-vec [size]
  "[_1 _2 .. _`size`]"
  (let [params (for [i (range size)]
                 (to-param (inc i)))]
    (vec params)))

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
  (let [params-vec (params-vec arity)
        rest-params (if (:rest-param (last matchers)) (conj (conj (vec (butlast params-vec)) '&) (last params-vec)))
        params (if rest-params rest-params params-vec)
        cond-clauses (mapcat
                       (fn [matcher]
                         (flush)
                         (let [matches (:matches matcher)
                               exprs (:exprs matcher)
                               test (->test (map :mexpr matches))
                               locals (:locals matcher)]
                           (if (seq locals)
                           ;if one local we can shed a vec
                             (if (second locals)
                               (concat (list test (concat (list 'let (vector locals params-vec)) exprs)))
                               (concat (list test (concat (list 'let (vector (first locals) (first params-vec))) exprs))))
                             (concat (list test) exprs))))
                       matchers)]
    (let [last-test (first (butlast cond-clauses))]
      (if (or (= last-test [:else]) (= last-test true))
        (list params (conj cond-clauses 'cond))
        (list params (conj (concat cond-clauses
                                   [:else `(throw (IllegalArgumentException.
                                                    (str "No match for " ~params)))]) 'cond))))))

(defn mask [what when? with]
  (walk/postwalk #(if (when? %) with %) what))


(defn type-check [type coll] (every? true? (map #(instance? type %) coll)))
(defn types-check [types coll] '(and (= (count types) (count coll)) (every? true? (map #(instance? %1 %2) types coll))))


(defn seq-types-match [arg param more]
  {:mexpr     `(type-check (first ~arg) ~param)
   :locals    nil
   :bounds    arg
   :unmatched more})

(defn seq-values-match [matches arg param]
  (let [rest-param (some #(= (:bounds %) ::rest-param) matches)
        arg-count-test (list (if rest-param '>= '=) (list 'count param) (count matches))
        seq-mexprs (list 'and (list 'sequential? param)
                         arg-count-test)
        mexprs (remove true? (map :mexpr matches))          ;guaranteed at least sequential condition
        seq-mexprs (if (seq mexprs)
                     (concat seq-mexprs mexprs)
                     seq-mexprs)
        sub-locals (vec (remove nil? (mapcat :locals matches)))
        locals (if (empty? sub-locals) nil [sub-locals])]
    (println "seq-match locals  " locals)
    (println " seq-match matches " seq-mexprs)
    {:mexpr     seq-mexprs
     :locals    locals
     :bounds    arg
     :unmatched (:unmatched (last matches))}))

(defn map-values-match [m key-matches val-matches param more]
  (println  "key matches " key-matches)
  (println  "val matches " val-matches)
  (let [arg-count-test (list '= (list 'count param) (count m))
        locals (dissoc (zipmap (mapcat :locals val-matches) (map :bounds key-matches)) '_ nil)

        locals (if (empty? locals) nil [locals])
        _ (println "Locals is " locals)
        map-mexprs (list 'and (list 'instance? ILookup param)
                         arg-count-test)
        map-mexprs (concat map-mexprs
                     (map (fn [k v]
                            (if (= ::symbol (:bounds v))
                              (list '.valAt (with-meta param {:tag 'clojure.lang.ILookup}) (:bounds k))
                              (list '= (list '.valAt (with-meta param {:tag 'clojure.lang.ILookup}) (:bounds k)) (:bounds v))))
                          key-matches val-matches))]
    {:mexpr     map-mexprs
     :locals    locals
     :bounds    m
     :unmatched more}))

(declare type-match)

(defn next-match [args next-param nested]
  (let [[arg & more] args
        sub-matches (fn [arg param]
                      (loop [ms []
                             arg arg
                             idx 0]
                        (prn "ARG HERE " arg)
                        (let [m (next-match arg `(nth ~param ~idx) true)
                              um (:unmatched m)]
                          (if (empty? um)
                            (conj ms m)
                            (recur (conj ms m) um (inc idx))))))
        name (if (and (symbol? arg) (not (type? arg))) arg nil)
        match (or
                (cond
                  (= '_ arg) {:mexpr true :bounds ::symbol :unmatched more}
                  (= '& arg) (if (second more)
                               :error
                               {:locals (if nested ['& (first more)] [(first more)]) :mexpr true :bounds ::rest-param :unmatched nil})
                  (= arg :else) {:mexpr :else :bounds ::symbol :unmatched more}
                  (= (first more) :-) {:locals [arg] :mexpr (list 'instance? (second more) next-param) :bounds (second more) :unmatched (drop 2 more)}
                  (= (first more) :as) (assoc (next-match [arg] next-param false) :locals [(second more)] :bounds (second more) :unmatched (drop 2 more))
                  (some? name) {:locals [arg] :mexpr true :bounds ::symbol :unmatched more}
                  (type? arg) {:locals ['_] :mexpr (list 'instance? arg next-param) :bounds arg :unmatched more}
                  (pattern? arg) {:locals ['_] :mexpr (list 're-matches arg next-param) :bounds arg :unmatched more}
                  (sequential? arg) (if (type? (first arg))
                                      (if (second arg)
                                        (throw (IllegalArgumentException. "Illegal form"))
                                        (seq-types-match arg next-param more))
                                      (seq-values-match (sub-matches arg next-param) arg next-param))
                  (map-like? arg) (if (every? type? (first (seq arg)))
                                    {:mexpr
                                     `(and (list instance? ILookup ~next-param)
                                           (type-check (first (keys ~arg)) (keys ~next-param))
                                           (type-check (first (vals ~arg)) (vals ~next-param))) :bounds arg :unmatched more}
                                    (map-values-match arg (sub-matches (keys arg) next-param) (sub-matches (vals arg) next-param) next-param more))
                  (= :seq arg) {:locals ['_] :mexpr (list 'instance? Sequential next-param) :bounds ::seq :unmatched more}
                  :else {:locals ['_] :mexpr (list '= next-param arg) :bounds arg :unmatched more}))]
    (if (nil? match) {:locals ['_] :mexpr (list '= next-param arg) :bounds arg} match)))

(defn ->matcher
  "For a match extract the type hints and annotations, and names if supplied."
  [params exprs]
  (println "\n")
  (prn "Matching " params " against " exprs)
  (let [[arity matches] (loop [ps params
                               ms []
                               a 0]
                          (if (empty? ps)
                            [a ms]
                            (let [m (next-match ps (to-param (inc a)) false)]
                              (assert some? (:unmatched m))
                              (recur (:unmatched m) (conj ms m) (inc a)))))
        _ (println "matcher mtaches Is " matches)
        locals (vec (remove nil? (mapcat :locals matches)))
        _ (println "Locals in Is " locals)
        bounds (map :bounds matches)
        rest-param (true? (some #(= (:bounds %) ::rest-param) matches))]
    {:arity arity :matches matches :bounds bounds :params params :locals locals :exprs exprs :rest-param rest-param}))



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


