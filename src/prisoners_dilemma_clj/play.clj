(ns prisoners-dilemma-clj.play
  (:require [prisoners-dilemma-clj.grid :as g])
  )


;; this should be in a bird file but I don't know how
;; to import a neighbor file dammit
(defn apply-to [ m k f]
  (assoc m k (f (get m k))))

;; bird messages
;; birds have a name that is unique
(defn identifier [bird]
  (:name bird))

;; birds have a map of strategies per bird
;; birds have a strategy for new bird interactions
(defn move-for [this-bird other-bird]
  (let [default-strategy (:strategy this-bird)
        this-strategy (or (get  (or (:strategies this-bird) {} ) (identifier other-bird)) default-strategy)]
    (this-strategy) ;;my silly no-args-means-next-move design
    ))
(defn receive-points [this-bird points]
  (apply-to this-bird :points #(+ (or % 0) points)))

;; birds can learn about an opponent's move
(defn opponent-moved [this-bird other-bird move ]
  (let [ other-bird-name (identifier other-bird)]
  (apply-to this-bird :strategies
       (fn [all-strategies]
           ( apply-to (or all-strategies {}) other-bird-name
       (fn [strat]
          ((or strat (:strategy this-bird)) move)))))))

(defn log [this-bird message]
  (apply-to this-bird :log #(cons message %))
  )
;; end bird messages



(defn pairinate
    [leftmost half-of-pair pairs-so-far rest-of-list]
    (if (empty? rest-of-list)
      (cons [half-of-pair leftmost] pairs-so-far)
      (let [e (first rest-of-list)
            others (rest rest-of-list)]
        (pairinate leftmost e (cons [half-of-pair e] pairs-so-far) others))))

(defn pair-ring [stuff] (reverse ( pairinate (first stuff) (first stuff) [] (rest stuff))))

(defn unpairinate [leftmost half-of-pair output-so-far rest-of-list]
  (if (empty? rest-of-list)
    (cons [half-of-pair leftmost] (reverse output-so-far))
    (let [next-pair (first rest-of-list)
          left-before (first next-pair)
          right-before (last next-pair)
          new-pair  [half-of-pair left-before]
          pairs-so-far (cons new-pair output-so-far)]
      (unpairinate leftmost right-before pairs-so-far (rest rest-of-list)))))

(defn swap-partners [seq-of-pairs]
  (let [first-pair (first seq-of-pairs)]
    (unpairinate (first first-pair) (second first-pair) [] (rest seq-of-pairs))))


(defn bird-moves [left-bird right-bird]
  [(move-for left-bird right-bird) (move-for right-bird left-bird)])

(defn fn1-and-then-fn2 [fnOf1 fnOf2]
  (fn [one] (fn [two] (fnOf2 one (fnOf1 two)))))

(defn transpose-int [output-so-far seq1 seq2]
  (if (empty? seq1)
    output-so-far
    (transpose-int (cons [(first seq1) (first seq2)] output-so-far) (rest seq1) (rest seq2))))

(defn transpose [seq1 seq2]
  (reverse  (transpose-int [] seq1 seq2)))

(defn apply-sideways [fnOf2 seq1 seq2]
  (map #(apply fnOf2 %) (transpose seq1 seq2)))

(defn moves-to-ops-that-need-opponent [move-pair]
  "returns a pair of functions from opponent to bird to new-bird"
  (let [scores (apply g/score move-pair)
        point-ops (map (fn [points] #(receive-points % points)) scores)
        notify-ops (map (fn [opp-move] (fn [opp bird] ( opponent-moved bird opp opp-move))) (reverse move-pair))]
 (apply-sideways fn1-and-then-fn2 point-ops notify-ops)))

(defn pair-of-birds-to-ops [left-bird right-bird]
  (let [moves (bird-moves left-bird right-bird)
        function-application (fn [f a] (f a)) ;; gotta be a better way
        ops (apply-sideways function-application (moves-to-ops-that-need-opponent moves) [right-bird left-bird])]
    ops))

;; if I were writing in haskell I'd have some hope of this working
(defn round [grid]
  (let [pairs (pair-ring grid)
        ops (map #(apply pair-of-birds-to-ops %) pairs)
        pairs-of-ops (swap-partners ops)
        bird-ops (map #(apply comp %) pairs-of-ops)]
   (apply-sideways (fn [f a] (f a)) bird-ops grid)))

