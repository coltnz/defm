(ns defm.core
  (:require [clojure.walk :as walk])
  (:import [java.util.regex Pattern]
           [clojure.lang Symbol ILookup Sequential]))

(defn debug [a-defm]
  (clojure.pprint/pprint (macroexpand-1 a-defm)))

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

(defn seq-like? [match]
  (instance? Sequential match))

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
              (cond
                (and (class? b) (class? p) (.isAssignableFrom b p)) true
                (= b p) true
                (= p ::symbol) true
                :else false))
            prior
            bounds)))

(defn process-bounds [bounded bounds matcher]
  ;(println "porcessing " bounded bounds)
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

(defn chk-type [type coll] (every? true? (map #(instance? %1 %2) type coll)))
(defn chk-types [types coll] (and (= (count types) (count coll)) (every? true? (map #(instance? %1 %2) types coll))))

;
;[a :- Long b :- String]
;[String Long]
;[a b [String]]
;[a :- Long b :- Long (or c String Long)]


(defn next-match [params a]
  (let [[p & more] params
        name (if (and (symbol? p) (not (type? p))) p nil)]
    (cond
      (= '_ p) [{:mexpr true :bounds ::symbol} more]
      (= p :else) [{:mexpr :else :bounds ::symbol} more]
      (= (first more) :-) [{:local [name a] :mexpr (list 'instance? (second more) a) :bounds (second more)} (drop 2 more)]
      (some? name) [{:local [p a] :mexpr true :bounds ::symbol} more]
      (type? p) [{:mexpr (list 'instance? p a) :bounds p} more]
      (pattern? p) [{:mexpr (list 're-matches p a) :bounds p} more]
      (and (seq-like? p) (type? (first p))) [{:mexpr (list 'and (list 'seq-like? a) (list 'defm.core/chk-types p a)) :bounds p} more]
      (and (seq-like? p) (= :- (second p))) [{:mexpr  (list 'and (list 'seq-like? a) (list 'defm.core/chk-type (mapv last (partition 3 p) 'a)))
                                              :bounds (map last (partition 3 more) p)} more]
      (map-like? p) [{:local [p a] :mexpr (list 'instance? ILookup a) :bounds (mask p symbol? ::symbol)} more]
      (= :seq p) [{:mexpr (list 'not (list 'instance? Sequential a)) :bounds ::seq} more]
      :else [{:mexpr (list '= a p) :bounds p} more])))

(defn ->matcher
  "For a match extract the type hints and annotations, and names if supplied."
  [params exprs]

  (let [[arity matches]
        (loop [ps params
               ms []
               a 0]
          (if (empty? ps)
            [a ms]
            (let [[m more] (next-match ps (to-arg (inc a)))]
              (assert some? more)
              (recur more (conj ms m) (inc a)))))
        locals (mapcat :local matches)
        bounds (map :bounds matches)]
    {:arity arity :matches matches :bounds bounds :params params :locals locals :exprs exprs}))

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
        matchers (reduce (fn [matchers match-clause]
                           (let [[match-params & exprs] match-clause]
                             (conj matchers (->matcher match-params exprs))))
                         [] body)
        arity->matchers (group-by #(:arity %) matchers)
        conds (for [a (sort (keys arity->matchers))
                    :let [matchers (arity->matchers a)]]
                (->cond a matchers))]
    `(defn ~name ~fn-meta
       ~@conds)))

(defmacro defm-
  "same as defm, yielding non-public def"
  [name & decls]
  (list* `defm (vary-meta name assoc :private true) decls))


