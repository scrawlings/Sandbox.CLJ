(defmacro foreach [[sym coll] & body]
  `(loop [coll# ~coll]
    (when-let [[~sym & xs#] (seq coll#)]
      ~@body
      (recur xs#))))
;(foreach [x [1 2 3 4]] (println x))

(defmacro print-keyword [x]
  `(println (keyword ~x)))

(require '(clojure [string :as str]
                   [walk :as walk]))

(defmacro reverse-it
  [form]
  (walk/postwalk 
    #(if (symbol? %)
      (symbol (str/reverse (name %)))
      %)
    form))

;(reverse-it
;  (qesod [gra (egnar 5)]
;    (nltnirp (cni gra))))

;(macroexpand-1 '(reverse-it
;  (qesod [gra (egnar 5)]
;    (nltnirp (cni gra)))))

;(macroexpand-1 '(cond a b c d))
;(walk/macroexpand-all '(cond a b c d))
;(require '[clojure.walk :as w])
;(w/macroexpand-all '(cond a b c d))

(defmacro while-a
  [test & body]
  (list 'loop []
    (concat (list 'when test) body)
    '(recur)))

(defmacro while-b
  [test & body]
  `(loop []
    (when ~test
    ~@body
    (recur))))

;(= (macroexpand-1 '(while-a (= a b) (println "abc"))) (macroexpand-1 '(while-b (= a b) (println "abc"))))
; false because loop != clojure.core/loop etc...

(def foo 123)
[foo (quote foo) 'foo `foo]

(= `(map println [~foo]) (list `map `println [foo]) `(map println ~[foo]))

`(println ~(keyword (str foo)))

(=
  (let [defs '((def x 123)
               (def y 456))]
    (concat (list 'do) defs))

  (let [defs '((def x 123)
               (def y 456))]
    `(do ~@defs)))

(defmacro foo
  [& body]
  `(do-something ~@body))
(macroexpand-1 '(foo (inc 1) (+ 2 3)))


(defmacro hygienic
  [& body]
  (let [sym (gensym)]
    `(let [~sym :macro-value] ;; note that sym can't interfer with anything input in body
      ~@body)))

; [`x# `x#] ;; unique per quote so different values here
; `[x# x#]  ;; one quoted form so same gensym value

(defmacro our-doto [expr & forms]
  (let [obj (gensym "obj")]
    `(let [~obj ~expr]
      ~@(map 
        (fn [[f & args]] `(~f ~obj ~@args)) 
        forms)
      ~obj)))
;(our-doto :abc (println :X)  (println :Y))

(defmacro spy-avoiding-double-evaluation [x]
  `(let [x# ~x]
    (println "spied" '~x x#)
    x#))

(defn spy-helper [expr value]
  (println expr value)
  value)
(defmacro spy [x]
  `(spy-helper '~x ~x))

(defmacro spy-env []
  (let [ks (keys &env)]
    `(prn (zipmap '~ks [~@ks]))))
;(let [x 1 y 2]
;  (spy-env)
;  (+ x y))

(defmacro simplify
  [expr]
  (let [locals (set (keys &env))]
    (if (some locals (flatten expr))
      expr
      (do
        (println "Precomputing: " expr)
        (list `quote (eval expr))))))
;(defn f
;  [a b c]
;  (+ a b c (simplify (apply + (range 5e7)))))
;(f 1 2 3)
;(defn f'
;  [a b c]
;  (simplify (apply + a b c (range 5e7))))
;(f' 1 2 3)


(defmacro ontology
  [& triples]
  (every? 
    #(or 
      (== 3 (count %))
      (throw (IllegalArgumentException. (format "`%s` provided to `%s` on line %s has < 3 elements"
                                          %
                                          (first &form)
                                          (-> &form meta :line)))))
    triples)
  ;; build and emit pre-processed ontology here...
)
; (ontology ["Boston" :capital-of])
; (ns com.clojurebook.macros)
; (refer 'user :rename '{ontology triples})
; (triples ["Boston" :capital-of])

(defn ensure-seq [x]
  (if (seq? x) x (list x)))
; (ensure-seq 'x)
; (ensure-seq '(x))
(defn insert-second
  "Insert x as the second item in seq y."
  [x ys]
  (let [ys (ensure-seq ys)]
    (list* (first ys) x (rest ys))))
(defmacro thread
  "Thread x through successive forms."
  ([x] x)
  ([x form] (insert-second x form))
  ([x form & more] `(thread (thread ~x ~form) ~@more)))
;(thread [1 2 3] (conj 4) reverse println)
;(thread [1 2 3] .toString (.split " ") seq)
(defn thread-fns
  ([x] x)
  ([x form] (form x))
  ([x form & more] (apply thread-fns (form x) more)))
;(thread-fns [1 2 3] reverse #(conj % 4) prn)
; fails (thread-fns [1 2 3] .toString #(.split % " ") seq)
;(thread-fns [1 2 3] #(.toString %) #(.split % " ") seq)
