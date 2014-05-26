(ns prisoners-dilemma-clj.core)

(def tit-for-tat
  ((fn tfti [prior]
     (fn
       ([] prior)
       ([n] (tfti n)))) true))

(defn always [val]
  (fn always-impl
    ([] val)
    ([_] always-impl)))

(def cooperate
  (always true))

(def defect
  (always false))

(def grudger
  ((fn gi [always-cooperated]
    (fn
     ([] always-cooperated)
     ([m] (gi (and always-cooperated m))))) true))

(defn moves [p1 p2]
  (let [p1m (p1)
        p2m (p2)]
    (cons [p1m p2m] (lazy-seq (moves (p1 p2m) (p2 p1m))))))


