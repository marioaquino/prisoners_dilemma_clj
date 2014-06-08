(ns prisoners-dilemma-clj.grid
  (:require [clojure.core.match :refer [match]]
            [prisoners-dilemma-clj.strategies :refer [strategies]]))

(defn name-factory []
  (let [seed "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        character #(rand-nth seed)
        triple #(reduce str (repeatedly 3 character))]
    (str (triple) "-" (triple))))

(defn bird-hatcher []
  {:name (name-factory) :strategy (rand-nth strategies)})

(defn grid [dimensions]
  (repeatedly dimensions bird-hatcher))

(defn grids-per-round [function-that-returns-a-grid initial-grid]
  (iterate function-that-returns-a-grid initial-grid))

(def rules
  {:t 5 ; temptation to defect
   :r 3 ; reward for mutual cooperation
   :p 1 ; punishment for mutual defection
   :s 0 ; suckers payoff
   })

(defn score [move-left move-right]
  (map rules (match [move-left move-right]
                    [true true] [:r :r]
                    [true false] [:s :t]
                    [false true] [:t :s]
                    [false false] [:p :p])))


