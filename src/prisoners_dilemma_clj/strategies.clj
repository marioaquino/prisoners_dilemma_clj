(ns prisoners-dilemma-clj.strategies)

(defn strategy-based-on-the-last-round [starting-move move-pair-to-next-move]
  (fn
    ([] starting-move)
    ([their-move] (let [my-next-move (move-pair-to-next-move starting-move their-move)]
                   (strategy-based-on-the-last-round my-next-move move-pair-to-next-move)))))

(defn starts-friendly [player]
 (strategy-based-on-the-last-round true player))


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

