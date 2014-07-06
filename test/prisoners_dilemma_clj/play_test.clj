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
       (fact "returns a pair of nils when given an empty sequence"
             (pair-ring []) => '([nil nil])))

