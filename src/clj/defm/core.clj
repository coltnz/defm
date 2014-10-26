(ns defm.core
  (:require [clojure.walk :refer [postwalk]]))

;inlined from tools.macro
(defn name-with-attributes
  [name macro-args]
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

(defn type? [match]
  (and (symbol? match)
       (or (class? (ns-resolve *ns* match))
           (primitive-sym? match))))

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

(defn ->test [mexprs]
  ;only need 1 true
  (println "mexprs " mexprs)
  (let [mexprs (if (second mexprs) (remove #(true? %) mexprs) mexprs)]
    (if (second mexprs)
      (cons 'and mexprs)
      (first mexprs))))

(defn ->cond [arity matchers]
  "Each arity shares a cond.
   [ [String] [Integer] ] => (instance "
  (println "am " arity matchers)
  (let [args (default-args arity)
        cond-clauses (mapcat
                       (fn [matcher]
                         (let [matches (:matches matcher)
                               exprs (:exprs matcher)
                               test (->test (map :mexpr matches))
                               locals (:locals matcher)]
                           (if (seq locals)
                             (concat (list test (concat (list 'let locals) exprs)))
                             (concat (list test) exprs))))
                       matchers)]
    (list args (conj cond-clauses 'cond))))


(defn next-match [params a]
  (let [[p & more] params
        name (if (and (symbol? p) (not (type? p))) p nil)]
    (cond
      (= '_ p) [{:mexpr true} more]
      (= (first more) :-) [{:local [name a] :mexpr (list 'instance? (first (rest more)) a)} (drop 2 more)]
      (some? name) [{:mexpr true :local [p a]} more]
      (type? p) [{:mexpr (list 'instance? p a)} more]
      (= :seq p) [{:mexpr (list 'not (list 'empty? a))}]
      :else [{:mexpr (list '= a p)} more])))

(defn ->matcher
  "For a match extract the type hints and annotations, and names if supplied.
  Nil serves as a placeholder"
  [params exprs]
  (let [[arity matches]
        (loop [ps params
               ms []
               a 0]
          (if (empty? ps)
            [a ms]
            (let [[m more] (next-match ps (to-arg (inc a)))]
              (recur more (conj ms m) (inc a)))))]
    {:arity arity :params params :matches matches :locals (vec (mapcat :local matches)) :exprs exprs}))

(defmacro defm
  [name & fdecl]
  (let [[name body] (name-with-attributes name fdecl)
        body (if (vector? (first body))
               (list body)
               body)
        fn-meta (-> name
                    meta
                    (assoc :arglists (list 'quote (@#'clojure.core/sigs body))))
        body (postwalk                                      ;inline?
               (fn [form]
                 (if (and (list? form) (= 'recur (first form)))
                   (list 'recur (cons 'vector (next form)))
                   form))
               body)
        _ (println body)
        ;todo check shape
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
  "As defm, but not public"
  [name & decls]
  (list* `defm (vary-meta name assoc :private true) decls))


