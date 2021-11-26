(ns hackerrank.happy-ladybugs
  (:require [hackerrank.util :as u]))

(defn happy?
  [bugs]
  (boolean (reduce (fn [{:keys [single last]}, bug]
                     (if (= single 2)
                       (reduced false)
                       {:last bug
                        :single (if (= last bug)
                                  0
                                  (inc single))})) {:single 0} bugs)))

(defn happyLadybugs [b]
  (let [occs  (u/coll->occurrences-map b)
        no-moves (not (contains? occs \_))
        letters-count (dissoc occs \_)
        has-single (some (fn [[_ count]]
                           (= count 1)) letters-count)]
    (if (or has-single (and no-moves (not (happy? b))))
      "NO"
      "YES")))