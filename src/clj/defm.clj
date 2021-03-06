(ns defm
  (:require [clojure.pprint :as pprint])
  (:import [java.util.regex Pattern]
           [clojure.lang Symbol ILookup ISeq]))

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
  ;(println "pb " prior bounds)
  (flush)
  (every? true?
          (map
            (fn [p b]
              (cond
                (and (class? b) (class? p) (.isAssignableFrom b p)) true
                (= b p) true
                (= p ::symbol) true
                :else false))
            prior
            bounds)))

(defn process-bounds [bounded bounds matcher]
  ;(println "bounded" bounded)
  ;(println "bounds" bounds)
  (doseq [prior bounded]
    (if (subsumes prior bounds)
      (bounds-err-> (:params matcher))))
  (conj bounded bounds))

(defn check-bounds [matchers]
  (reduce #(process-bounds %1 (:bounds %2) %2) [] matchers))

;; To make a valid test from mexpr s we merely need to `and` if plural and add :else if no default clause.
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
                         (let [matches (:matches matcher)
                               expr (:expr matcher)
                               test (->test (map :mexpr matches))
                               locals (:locals matcher)]
                           (if (seq locals)
                             ;if one local we can shed a vec
                             (if (second locals)
                               (concat (list test (concat (list 'let (vector locals params-vec)) expr)))
                               (concat (list test (concat (list 'let (vector (first locals) (first params-vec))) expr))))
                             (concat (list test) expr))))
                       matchers)]
    (let [last-test (first (butlast cond-clauses))]
      (if (or (= last-test :else) (= last-test true))
        (list params (conj cond-clauses 'cond))
        (list params (conj (concat cond-clauses
                                   [:else `(throw (IllegalArgumentException.
                                                    (str "No match for " ~params)))]) 'cond))))))

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
    {:mexpr     seq-mexprs
     :locals    locals
     :bounds    arg
     :unmatched (:unmatched (last matches))}))

(defn map-values-match [m key-matches val-matches param more]
  (let [arg-count-test (list '= (list 'count param) (count m))
        locals (dissoc (zipmap (mapcat :locals val-matches) (map :bounds key-matches)) '_ nil)

        locals (if (empty? locals) nil [locals])
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
                  (= :seq arg) {:locals ['_] :mexpr (list 'instance? ISeq next-param) :bounds ::seq :unmatched more}
                  :else {:locals ['_] :mexpr (list '= next-param arg) :bounds arg :unmatched more}))]
    (if (nil? match) {:locals ['_] :mexpr (list '= next-param arg) :bounds arg} match)))

(defn ->matcher
  "For a match extract the type hints, annotations, and names if supplied."
  [match-args expr]
  (let [[arity matches] (loop [ma match-args
                               ms []
                               a 0]
                          (if (empty? ma)
                            [a ms]
                            (let [m (next-match ma (to-param (inc a)) false)]
                              (assert some? (:unmatched m))
                              (recur (:unmatched m) (conj ms m) (inc a)))))
        locals (vec (remove nil? (mapcat :locals matches)))
        bounds (map :bounds matches)
        rest-param (true? (some #(= (:bounds %) ::rest-param) matches))]
    {:arity  arity :matches matches :bounds bounds :match-args match-args
     :locals locals :expr expr :rest-param rest-param}))



(defmulti type-match
          "Given a list of params extracts next match map with -
  :mexpr the expression to execute the match
  :bounds the scope of the match
  :unmatched the remainder of the params"
  (fn [next-arg next-param more]
    [next-arg next-param more]))

(defmacro fm
  [& body]
  (let [
        _ (map #(if (next body)
                 (second body)
                 (throw (IllegalArgumentException. "fm requires an even number of forms"))) body)
        body (if (vector? (first body)) (map list* (partition 2 body)) body)
        [else? else-expr] [(= (first (last body)) :else) (last body)]
        body (if else? (butlast (rest body)) body)
        matchers (reduce (fn [matchers match-clause]
                           (let [[match-args & expr] match-clause]
                             (if (= match-args :else)
                               (throw (IllegalArgumentException. ":else can only be last match")))
                             (conj matchers (->matcher match-args expr))))
                         [] body)
        arity->matchers (group-by #(:arity %) matchers)
        _ (clojure.pprint/pprint arity->matchers)
        conds (for [a (sort (keys arity->matchers))
                    :let [matchers (if else?
                                     (conj (arity->matchers a)
                                           {:match-args :else
                                            :expr else-expr
                                            :matches [(next-match [:else] (to-param a) false)]
                                            :bounds [::symbol]})
                                     (arity->matchers a))]]
                (->cond a matchers))]
    `(fn ~@conds)))


(defmacro letfm
  {:forms '[(letfm [fnspecs*] exprs*)]}
  [fnspecs & body]
  `(letfn* ~(vec (interleave (map first fnspecs)
                             (map #(cons `fm %) fnspecs)))
           ~@body))

(defmacro defm
  [name & fdecl]
  "Same as defm, yielding non-public def"

  (let [[name body] (name-with-attributes name fdecl)
        name (vary-meta name assoc :argslist (list 'quote (@#'clojure.core/sigs body)))]
    `(def ~name (fm ~@body))))

(defmacro defm-
  "Same as defm, yielding non-public def"
  [name & decls]
  (list* `defm (vary-meta name assoc :private true) decls))

(defmacro match
  "Matches `expr` against "
  [expr & body]
  (list `(fm ~@body) expr))

(defn debug [a-fm]
  "Pretty prints macroexpansion of `a-fm`"
  (pprint/pprint (macroexpand-1 a-fm)))


