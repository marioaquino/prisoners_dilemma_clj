(ns prisoners-dilemma-clj.core)

(def tit-for-tat
  ((fn tfti [_ prior]
     (fn
       ([] prior)
       ([my-last-move their-last-move] (tfti my-last-move their-last-move)))) true true))

(defn always [val]
  (fn always-impl
    ([] val)
    ([_ _] always-impl)))

(def cooperate
  (always true))

(def defect
  (always false))

;; I cooperate until I get defected against. Then I always defect
;; Did I cooperate last time and did my opponent cooperate last time? I will this time.
;; Did I defect last time? If I did, then I will this time.
(def grudger
  ((fn grudge [my-last their-last]
    (fn
      ([] (and my-last their-last))
      ([my-last-move their-last-move] (grudge my-last-move their-last-move)))) true true))

(defn moves [p1 p2]
  (let [p1m (p1)
        p2m (p2)]
    (cons [p1m p2m] (lazy-seq (moves (p1 p1m p2m) (p2 p2m p1m))))))


