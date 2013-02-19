(def sample-graph-connected
	{:nodes #{:a1 :a2 :b1 :b2},
	 :edges #{[:a1 :b1] [:a1 :b2] [:a1 :a2]} })
(def sample-graph-disjoint
	{:nodes #{:a1 :a2 :b1 :b2},
	 :edges #{[:a1 :b1] [:a1 :b2]} })


(defn add-node [graph node] 
	(assoc graph :nodes (conj (:nodes graph) node)) )
(defn add-edge [graph edge]
	(assoc 
		(reduce add-node graph edge) 
			:edges (conj (:edges graph) edge)) )
(defn add-edge-bi-di [graph [a b]]
	(add-edge (add-edge graph [a b]) [b a]) )


(defn random-pair [source] [(rand-nth source) (rand-nth source)])
(defn random-edges [nodes]
	(let [nodes-vector (vec nodes)]
		(filter (fn [[a b]] (not (= a b)))
			(repeatedly 
				#(random-pair nodes-vector) ))))
(defn double-edge [[a b]]
	[[a b] [b a]])
(defn bi-di-edges [edges]
	(if (empty? edges) 
		nil
		(lazy-cat (double-edge (first edges)) (bi-di-edges (rest edges)))))

(defn pair-with [xs x] (map #(vector % x) xs))
(defn pair-wise [bounded infinite] (mapcat #(pair-with bounded %) infinite))
(defn seq-to-str [xs] (reduce #(str %1 %2) "" xs))
(defn labels [] (map (comp keyword seq-to-str) (pair-wise "abcdefg" (iterate inc 0))))
(defn all-pairs [xs] (if (empty? xs) nil (concat (pair-with (rest xs) (first xs)) (all-pairs (rest xs)))))

(defn factorial [n] (reduce * (range 1 (inc n))))

(defn random-graph-directed [n k]
	(let [nodes (set (take n (labels)))
	      edges (set (take (* k (factorial (dec n))) (random-edges nodes))) ]
	     {:nodes nodes, :edges edges}))
(defn random-graph-non-directed [n k]
	(let [nodes (set (take n (labels)))
	      edges (set (take (* 2 k (factorial (dec n))) (bi-di-edges (random-edges nodes)))) ]
	     {:nodes nodes, :edges edges}))
(defn probabilistic-graph [n p]
	(let [nodes (set (take n (labels)))
		  edges (set (filter #(let [_ %] (< (rand) p)) (all-pairs nodes)))]
	     {:nodes nodes, :edges edges}))
(defn probabilistic-graph-non-directed [n p]
	(let [nodes (set (take n (labels)))
		  edges (set (bi-di-edges (filter #(let [_ %] (< (rand) p)) (all-pairs nodes))))]
	     {:nodes nodes, :edges edges}))


(require 'clojure.set)

(defn following-nodes [graph node]
	(set (map second (filter (fn [[a b]] (= a node)) (:edges graph)))))

(defn connected-for-non-directed 
	([graph] (connected-for-non-directed graph #{} (conj (clojure.lang.PersistentQueue/EMPTY) (first (:nodes graph)))))
	([graph marks queue]
		(cond 
			(empty? queue) (empty? (clojure.set/difference (set (:nodes graph)) marks))
			:else (let [current-node (peek queue)
						next-marks (conj marks current-node)
						following-queue (into (pop queue) (clojure.set/difference (following-nodes graph current-node) next-marks))]
				(recur graph next-marks following-queue)))))