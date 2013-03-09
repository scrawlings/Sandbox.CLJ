(defn maze
  [walls]
  (let [paths (reduce 
                (fn [index [a b]] (merge-with into index {a [b], b [a]}))
                {} (map seq walls))
        start-loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start-loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walk) (next walk))]
          (recur 
            (reduce disj walls (map set steps))
            (reduce disj unvisited (keys steps))))
        walls))))

(defn grid 
  [w h]
  (set (concat
          (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
          (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(defn draw
  [w h maze]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
      (doto (proxy [javax.swing.JPanel] []
              (paintComponent [^java.awt.Graphics g]
                (let [g (doto ^java.awt.Graphics2D 
                          (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (java.awt.BasicStroke. 0.4)))]
                  (.drawRect g -1 -1 w h)
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (let [[xc yc] (if (= xa xb)
                                    [(dec xa) ya]
                                    [xa (dec ya)])]
                      (.drawLine g xa ya xc yc))))))
        (.setPreferredSize 
          (java.awt.Dimension. (* 10 (inc w)) (* 10 (inc h))))))
     .pack
     (.setVisible true)))
;;(draw 40 40 (maze (grid 40 40)))

(defn hex-grid
  [w h]
  (let [vertices (set (for [y (range h) x (range (if (odd? y) 1 0) (* 2 w) 2)] 
                           [x y]))
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v vertices d deltas f [+ -]
            :let [w (vertices (map f v d))]
            :when w] #{v w}))))

(defn- hex-outer-walls
  [w h]
  (let [vertices (set (for [y (range h) x (range (if (odd? y) 1 0) (* 2 w) 2)]
                        [x y]))
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v vertices d deltas f [+ -]
            :let [w (map f v d)]
            :when (not (vertices w))] #{v (vec w)}))))

(defn hex-draw
  [w h maze]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
      (doto (proxy [javax.swing.JPanel] []
              (paintComponent [^java.awt.Graphics g]
                (let [maze (into maze (hex-outer-walls w h))
                      g (doto ^java.awt.Graphics2D 
                          (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (java.awt.BasicStroke. 0.4
                                        java.awt.BasicStroke/CAP_ROUND
                                        java.awt.BasicStroke/JOIN_MITER)))
                      draw-line (fn [[[xa ya] [xb yb]]]
                                  (.draw g
                                    (java.awt.geom.Line2D$Double.
                                      xa (* 2 ya) xb (* 2 yb))))]
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (draw-line
                      (cond
                        (= ya yb) [[(inc xa) (+ ya 0.4)] [(inc xa) (- ya 0.4)]]
                        (< ya yb) [[(inc xa) (+ ya 0.4)] [xa (+ ya 0.6)]]
                        :else [[(inc xa) (- ya 0.4)] [xa (- ya 0.6)]]))))))
        (.setPreferredSize (java.awt.Dimension.
          (* 20 (inc w)) (* 20 (+ 0.5 h))))))
    .pack
    (.setVisible true)))
;;(hex-draw 40 40 (maze (hex-grid 40 40)))

;; maze defines the walls, so grid minus maze is a set of corridors...
(def labyrinth (let [g (grid 10 10)] (reduce disj g (maze g))))

(def theseus (rand-nth (distinct (apply concat labyrinth))))
(def minotaur (rand-nth (distinct (apply concat labyrinth))))

(require '[clojure.zip :as z])

(defn ariadne-zip
  [labyrinth loc]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b], b [a]}))
                {} (map seq labyrinth))
        children (fn [[from to]]
                    (seq (for [loc (paths to)
                               :when (not= loc from)]
                          [to loc])))]
    (z/zipper 
      (constantly true)
      children
      nil
      [nil loc])))
;(->> theseus
;  (ariadne-zip labyrinth)
;  (iterate z/next)
;  (filter #(= minotaur (second (z/node %))))
;  first z/path
;  (map second))

(defn draw-maze-and-path
  [w h maze path]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
      (doto (proxy [javax.swing.JPanel] []
              (paintComponent [^java.awt.Graphics g]
                (let [g (doto ^java.awt.Graphics2D 
                          (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (java.awt.BasicStroke. 0.4)))]
                  (.drawRect g -1 -1 w h)
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (let [[xc yc] (if (= xa xb)
                                    [(dec xa) ya]
                                    [xa (dec ya)])]
                      (.drawLine g xa ya xc yc)))
                      (.translate g -0.5 -0.5)
                      (.setColor g java.awt.Color/RED)
                      (doseq [[[xa ya] [xb yb]] path]
                        (.drawLine g xa ya xb yb)))))
        (.setPreferredSize (java.awt.Dimension. (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(defn legend 
  [w h] 
  (let [grid (grid w h)
        walls (maze grid)
        labyrinth (reduce disj grid walls)
        places (distinct (apply concat labyrinth))
        theseus (rand-nth places)
        minotaur (rand-nth places)
        path (->> theseus
             (ariadne-zip labyrinth)
             (iterate z/next)
             (filter #(= minotaur (first (z/node %))))
             first z/path rest)]
    (draw-maze-and-path w h walls path)))