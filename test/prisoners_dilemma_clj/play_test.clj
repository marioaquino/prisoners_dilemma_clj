(ns prisoners-dilemma-clj.play-test
  (use midje.sweet)
  (:require [clojure.test :refer :all]
            [prisoners-dilemma-clj.play :refer :all]))

(fact "scoring a grid yields a map of names and points"
      (let [sucker-strategy "pretend I am a sucker"
            named-strategy (with-meta 'sucker-strategy {:name "sucker"})
            my-grid [{:points 10 :strategy named-strategy}]]
        (score-grid my-grid) => '({:name "sucker" :points 10})))

(facts "pair-ring"
       (fact "returns a pair of nils when given an empty vector"
             (pair-ring []) => '([nil nil]))
       
       (fact "returns a pair of the same thing when given a vector of 1"
             (pair-ring [:a]) => '([:a :a]))
       
       (fact "returns two pairs with the same things when given a vector of 2"
             (pair-ring [:a :b]) => '([:a :b] [:b :a]))
       
       (fact "returns pairs of three different items"
             (set (pair-ring [:a :b :c])) => #{[:a :b] [:b :c] [:c :a]}))

