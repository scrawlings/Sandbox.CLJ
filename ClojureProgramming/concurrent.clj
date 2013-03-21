;(def d (delay (println "Running...") :done!))
;d
;(realized? d)
;(deref d)
;@d
;d
;(realized? d)

(def long-calculation (future (apply + (range 1e8))))
;(deref long-calculation 100 :impatient)
;(deref long-calculation 10000 :impatient)
(defn zero-arg [] :woot!)
(def zero-arg-future (future-call zero-arg))

(def p (promise))
;(realized? p)
;(deliver p :woot!)
;(realized? p)
;@p

(defn ex1-1-a [] 
  (def a (promise))
  (def b (promise))
  (def c (promise)))
(defn ex1-1-b []
  (future
    (deliver c (+ @a @b))
    (println "Delivery complete!")))
(defn ex1-1-c [A B] 
  (deliver a A) 
  (deliver b B))

(defn call-service
  [arg1 arg2 callback-fn]
  ; ...perform service call, eventually invoking callback-fn with results...
  (future (callback-fn (+ arg1 arg2) (- arg1 arg2))))
(defn sync-fn
  [async-fn]
  (fn [& args]
    (let [result (promise)]
      (apply async-fn (conj (vec args) #(deliver result %&)))
      @result)))
;((sync-fn call-service) 8 7)

(defn phone-numbers
  [string]
  (re-seq #"(\d{3})[\.-]?(\d{3})[\.-]?(\d{4})" string))
;(phone-numbers " Sunil: 617.555.2937, Betty: 508.555.2218")
;(def files (repeat 10000
;  (apply str
;    (concat (repeat 10000 \space)
;      "Sunil: 617.555.2937, Betty: 508.555.2218"))))
;(time (dorun (map phone-numbers files)))
;(time (dorun (pmap phone-numbers files)))
;(time (->> files
;  (partition-all 250)
;  (pmap (fn [chunk] (doall (map phone-numbers chunk))))
;  (apply concat)
;  dorun))


;(map deref [(agent {:c 42}) (atom 12) (ref "http://clojure.org") (var +)])

(defmacro futures
  [n & exprs]
    (vec (for [_ (range n)
      expr exprs]
      `(future ~expr))))

(defmacro wait-futures
  [& args]
    `(doseq [f# (futures ~@args)]
      @f#))

(defn ex2-1 []
  (let [sarah (atom {:name "Sarah" :age 25 :wears-glasses? false})]
    (pprint [
      (swap! sarah update-in [:age] + 3)
      (swap! sarah (comp 
                      #(update-in % [:age] inc)
                      #(assoc % :wears-glasses? true)))
      (compare-and-set! sarah :wrong "new value")
      (compare-and-set! sarah @sarah {:name "Sarah" :age 25})
      (reset! sarah :go-nuclear) 
    ])))


(defn echo-watch
  [key identity old new]
  (println key old "=>" new))
(defn ex2-2 []
  (let [sarah (atom {:name "Sarah" :age 25})]
    (pprint [
      (add-watch sarah :echo echo-watch)
      (swap! sarah update-in [:age] inc)
      (add-watch sarah :echo2 echo-watch)
      (swap! sarah update-in [:age] inc)
      (remove-watch sarah :echo2)
      (swap! sarah update-in [:age] inc)
    ])))

(defn log->list
  [dest-atom key source old new]
  (when (not= old new)
    (swap! dest-atom conj new)))
(defn ex2-3 []
  (let 
    [history (atom ())
     sarah (atom {:name "Sarah" :age 25})]
    (pprint [
      (add-watch sarah :record (partial log->list history))
      (swap! sarah update-in [:age] inc)
      (swap! sarah update-in [:age] inc)
      (swap! sarah identity)
      (swap! sarah assoc :wears-glasses? true)
      (swap! sarah update-in [:age] inc)
      :history @history
    ])))

(defn ex2-4 []
  (let 
    [sarah (atom {:name "Sarah" :age 25})]
    (pprint [
      ;(set-validator! sarah :age)
      (set-validator! sarah #(or (:age %) (throw (IllegalStateException. "People must have ':age's!"))))
      (swap! sarah dissoc :age)
      ])))

(defn ex3-1 []
  (let
    [ x (ref 0)]   
    (comment (println :alter) (time 
      (wait-futures 5
        (dotimes [_ 1000]
          (dosync (alter x + (apply + (range 1000)))))
         (dotimes [_ 1000]
           (dosync (alter x - (apply + (range 1000))))))))
    (do (println :commute) (time 
      (wait-futures 5
        (dotimes [_ 1000]
          (dosync (commute x + (apply + (range 1000)))))
         (dotimes [_ 1000]
           (dosync (commute x - (apply + (range 1000)))))))) ))

(defn- enforce-max-health
  [{:keys [name health]}]
  ;[name health]
  (fn [character-data]
    (or (<= (:health character-data) health)
      (throw (IllegalStateException. (str name " is already at max health!"))))))
;(defn character
;  [name & {:as opts}]
;    (ref (merge {:name name :items #{} :health 500} opts)))
(defn character
  [name & {:as opts}]
    (let [cdata (merge {:name name :items #{} :health 500} opts)
          cdata (assoc cdata :max-health (:health cdata)) 
          validators (list* (enforce-max-health {:name name :health (:health cdata)}) (:validators cdata))]
          ;validators (list* (enforce-max-health name (:health cdata)) (:validators cdata))]
    (ref 
      (dissoc cdata :validators)
      :validator #(every? (fn [v] (v %)) validators))))

(defn init-me []
  (def smaug (character "Smaug" :health 200 :strength 500 :items (set (range 50)))) 
  (def bilbo (character "Bilbo" :health 100 :strength 100))
  (def gandalf (character "Gandalf" :health 75 :mana 750)) )
(init-me)

(defn loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (alter to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))
(defn flawed-loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (commute to update-in [:items] conj item)
      (commute from update-in [:items] disj item))))
(defn fixed-loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (commute to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))

(defn ex4-1 [looter]
  (wait-futures 1
    (while (looter smaug bilbo))
    (while (looter smaug gandalf)))
  (pprint [
    (map (comp count :items deref) [bilbo gandalf])
    (filter (:items @bilbo) (:items @gandalf)) ]))

(defn attack
  [aggressor target]
  (dosync
    (let [damage (* (rand 0.1) (:strength @aggressor))]
      (commute target update-in [:health] #(max 0 (- % damage)))
      true
      )))
(defn heal
  [healer target]
    (dosync
      (let [aid (min 
                  (* (rand 0.1) (:mana @healer)) 
                  (- (:max-health @target) (:health @target)))]
        (when (pos? aid)
          (commute healer update-in [:mana] - (max 5 (/ aid 5)))
          (alter target update-in [:health] + aid))
        true)))

(def alive? (comp pos? :health))
(defn play
  [character action other delay & desc]
  (while 
    (and 
      (alive? @character)
      (alive? @other)
      (alive? @smaug) ;; gandalf hangs on without this, consider it a bug in the application
      (action character other))
    ;;(println desc)
    (Thread/sleep (rand-int delay))))

(defn ex4-2 []
  (wait-futures 1
    (play bilbo attack smaug 50 "bilbo attacks smaug")
    (play gandalf heal bilbo 20 "gandalf heals bilbo")
    (play smaug attack bilbo 50 "smaug attacks bilbo"))
  ;;just for giggles (dosync (ref-set bilbo {:name "Bilbo"}))
  (pprint (map (comp #(select-keys % [:name :health :max-health :mana]) deref) [smaug bilbo gandalf])))


(require '[clojure.java.io :as io])
(def console (agent *out*))
(def character-log (agent (io/writer "character-states.log" :append true)))

(defn write
  [^java.io.Writer w & content]
  (doseq [x (interpose " " content)]
    (.write w (str x)))
    (doto w
    (.write "\n")
    .flush))
(defn log-reference
  [reference & writer-agents]
  (add-watch reference :log
    (fn [_ reference old new]
      (doseq [writer-agent writer-agents]
        (send-off writer-agent write new)))))
(defn ex5-1 []
  (log-reference bilbo console character-log)
  (log-reference smaug console character-log)
  (wait-futures 1
    (play bilbo attack smaug 50 "bilbo attacks smaug")
    (play gandalf heal bilbo 20 "gandalf heals bilbo")
    (play smaug attack bilbo 50 "smaug attacks bilbo")))