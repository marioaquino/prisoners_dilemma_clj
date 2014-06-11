(ns prisoners-dilemma-clj.strategies)

(defn starts-friendly [player]
  ((fn starts-friendly-impl [my-last-move their-last-move]
     (let [my-current-move (memoize #(player my-last-move their-last-move))]
       (fn
         ([] (my-current-move))
         ([their-last] (starts-friendly-impl (my-current-move) their-last))))) true true))

(def tit-for-tat
  (starts-friendly (fn [_ their-last-move]
         their-last-move)))

(defn always [val]
  (fn always-impl
    ([] val)
    ([_] always-impl)))

(def cooperate
  (always true))

(def defect
  (always false))

;; I cooperate until I get defected against. Then I always defect
;; Did I cooperate last time and did my opponent cooperate last time? I will this time.
;; Did I defect last time? If I did, then I will this time.
(def grudger
  (starts-friendly (fn [my-last their-last]
         (and my-last their-last))))

(def strategies
  [#'tit-for-tat
   #'cooperate
   #'defect
   #'grudger])

(defn moves [p1 p2]
  (let [p1m (p1)
        p2m (p2)]
    (cons [p1m p2m] (lazy-seq (moves (p1 p2m) (p2 p1m))))))

(def my-grid (g/grid 10))
(def scores (map (fn [{points :points strat :strategy}] {:name (:name (meta strat)) :points points}) (p/round my-grid)))
(sort-by :points scores)
=>  ({:name tit-for-tat, :points 3} {:name grudger, :points 3} {:name cooperate, :points 3} {:name tit-for-tat, :points 3} {:name cooperate, :points 6} {:name cooperate, :points 6} {:name cooperate, :points 6} {:name grudger, :points 6} {:name defect, :points 10} {:name defect, :points 10})


(map 
  (fn [{points :points strat :strategy} b] {:name (:name (meta strat)) :points points}) (p/round my-grid))

