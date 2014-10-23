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

(defn ->cond [arity matchers]
  "Each arity shares a cond.
   [ [String] [Integer] ] => (instance "
  (println "TLE" arity matchers)
  (let [args (default-args arity)
        cond-clauses (mapcat
                       (fn [matcher]
                         (let [matches (:matches matcher)
                               test (if (second matches)
                                      (cons 'and (map :match matches))
                                      (:match (first matches)))
                               locals (:locals matcher)]
                           (list test (if (empty? locals) (cons 'do (:expr matcher)) (concat `(let ~locals) (:expr matcher))))))
                       matchers)]
    (list args (conj cond-clauses 'cond)))
  ;(if-not (filter #(or (true? %) (= :else %)) (first (butlast ts&ls)))
  ;      (conj ts&ls [(list :else `(throw (IllegalArgumentException. (str "No match for " ~params)))) []])
  ;      ts&ls))
  )


(defn next-match [params a]
  (let [[p & more] params
        name (if (and (symbol? p) (not (type? p))) p nil)]
    (cond
      (= (first more) :-) [{:local [name a] :match (list 'instance? a (first (rest more)))} (drop 2 more)]
      (some? name) [{:match :else} more]
      (type? p) [{:match (list 'instance? p a)} more]
      (= :else p) [{:match :else} more]
      :else [{:match (list '= p a)} more])))

(defn ->matcher
  "For a match extract the type hints and annotations, and names if supplied.
  Nil serves as a placeholder"
  [params expr]
  (let [[arity matches]
        (loop [ps params
               ms []
               a 0]
          (if (empty? ps)
            [a ms]
            (let [[m more] (next-match ps (to-arg (inc a)))]
              (recur more (conj ms m) (inc a)))))]
    {:arity arity :matches matches :locals (vec (mapcat :local matches)) :expr expr}))

(defmacro defm
  [name & fdecl]
  (let [[name body] (name-with-attributes name fdecl)
        body (if (vector? (first body))
               (list body)
               body)
        fn-meta (-> name
                    meta
                    (assoc :arglists (list 'quote (@#'clojure.core/sigs body))))
        body (postwalk                                      ;todo inline
               (fn [form]
                 (if (and (list? form) (= 'recur (first form)))
                   (list 'recur (cons 'vector (next form)))
                   form))
               body)
        ;todo check shape
        matchers (reduce (fn [matchers match-clause]
                           (let [[match-params expr] match-clause]
                             (conj matchers (->matcher match-params expr))))
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
