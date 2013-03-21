(defn X ([] (println 42)) ([x] (println x) (inc x)))
(defn Y ([] 42) ([y] (inc y)))
(defn ex0-1 [] (X))
(defn ex0-2 [] (dotimes [_ 10] (X)))
(defn ex0-3 [] (take 10 (iterate Y 0)))

(map deref [(agent {:c 42}) (atom 12) (ref "http://clojure.org") (var +)])

(def s-n-s (comp str - +))
(s-n-s 1 4 5.5)

(def n (atom 1 :validator pos?))
(swap! n + 500)
(swap! n - 1000)


;; vars and binding and stuff
(def ^:private everything 42)
(def otherthings 41)
(meta #'everything)
(ns other-namespace)
(refer 'user)
everything
otherthings
@#'user/everything
(ns user)

(defn ^:dynamic *woot* [a] (+ a 3))
(meta #'*woot*)
(defn blah [b] (println (*woot* (* 2 b))))
(blah 3)
(binding [*woot* #(* % 3)] (blah 3))

(ns-unmap (find-ns 'user) 'otherthings)

(declare a b)
(defn a [x] (if (> x 0) (b (dec x)) :a))
(defn b [x] (if (> x 0) (a (dec x)) :b))

