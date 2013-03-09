(defn empty-board
	"Creates a rectangular board of the given dimensions"
	[w h]
	(vec (repeat w (vec (repeat h nil)))))

(defn populate
	"Turns :on cells as the specified [x,y] coordinates"
	[board living-cells]
	(reduce 
		(fn [board coordinates] (assoc-in board coordinates :on))
		board
		living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(defn neighbours
	[[x y]]
	(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
		[(+ dx x) (+ dy y)]))

(defn count-neighbours 
	[board loc]
	(count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step-manual-iteration
	"Next value of a g-o-l board, using indices to identify neighbours, liveness..."
	[board]
	(let [w (count board)
		  h (count (first board))]
		(loop [new-board board, x 0, y 0]
			(cond
				(>= x w) new-board
				(>= y h) (recur new-board (inc x) 0)
				:else
					(let [new-liveness
							(case (count-neighbours board [x y])
								2 (get-in board [x y])
								3 :on
								nil)]
						(recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))
;; (-> (iterate indexed-step-manual-iteration glider) (nth 7) pprint)

(defn indexed-step-loop-to-reduction
	"Next value of a g-o-l board, using indices to identify neighbours, liveness..."
	[board]
	(let [w (count board)
		  h (count (first board))]
		(reduce
			(fn [new-board x]
				(reduce 
					(fn [new-board y]
						(let [new-liveness
							(case (count-neighbours board [x y])
								2 (get-in board [x y])
								3 :on
								nil)]
						 (assoc-in new-board [x y] new-liveness)))
					new-board (range h)))
			board (range w))))
;; (-> (iterate indexed-step-loop-to-reduction glider) (nth 7) pprint)

(defn indexed-step-flatten-reductions
	"Next value of a g-o-l board, using indices to identify neighbours, liveness..."
	[board]
	(let [w (count board)
		  h (count (first board))]
		(reduce
			(fn [new-board [x y]]
				(let [new-liveness
					(case (count-neighbours board [x y])
						2 (get-in board [x y])
						3 :on
						nil)]
					(assoc-in new-board [x y] new-liveness)))
			board (for [x (range w) y (range h)] [x y]))))
;; (-> (iterate indexed-step-flatten-reductions glider) (nth 7) pprint)

(defn window
	"returns a lazy sequence of three item windows, around each item of a col"
	([coll] (window nil coll))
	([pad coll] (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block
	"creates series of 3x3 windows from a triple of 3 sequences"
	[[left mid right]]
	(window (map vector left mid right)))

(defn liveness
	"Returns the liveness (nil or :on) of the center cell for the next step"
	[block]
	(let [[_ [_ center _] _] block]
		(case (- (count (filter #{:on} (apply concat block)))
				 (if (= :on center) 1 0))
			2 center
			3 :on
			nil)))

(defn- step-row
	"Yields the next state of the center row."
	[row-triples]
	(vec (map liveness (cell-block row-triples))))

(defn index-free-step
	"Yields the next state of the board"
	[board]
	(vec (map step-row (window (repeat nil) board))))
;; (-> (iterate index-free-step glider) (nth 7) pprint)

;;(= (nth (iterate index-free-step glider) 7)
;;   (nth (iterate indexed-step-flatten-reductions glider) 7))


(defn step
	[cells]
	(set (for [[loc n] (frequencies (mapcat neighbours cells))
			:when (or (= n 3) (and (= n 2) (cells loc)))]
			loc)))
;;(->> 
;;	(iterate step #{[2 0] [2 1] [2 2] [1 2] [0 1]})
;;	(drop 7)
;;	first
;;	(populate (empty-board 6 6))
;;	pprint)

(defn stepper 
	[neighbours birth? survive?]
	(fn [cells]
		(set (for [[loc n] (frequencies (mapcat neighbours cells))
				:when (if (cells loc) (survive? n) (birth? n))]
				loc))))
;;(->> 
;;	(iterate (stepper neighbours #{3} #{2 3}) #{[2 0] [2 1] [2 2] [1 2] [0 1]})
;;	(drop 7)
;;	first
;;	(populate (empty-board 6 6))
;;	pprint)

(defn hex-neighbours
	[[x y]]
	(for [dx [-1 0 1] dy (if (zero? dx) [-2 2] [-1 1])]
		[(+ dx x) (+ dy y)]))

(def hex-step (stepper hex-neighbours #{2} #{3 4}))
(hex-step #{[0 0] [1 1] [1 3] [0 4]})
;(hex-step *1)
;(hex-step *1)
;(hex-step *1)
