(ns hackerrank.e-cavity-map
  (:require [hackerrank.util :as u]))

(defn parse-grid
  [str-grid]
  (into [] (map #(mapv u/parse-char %)) str-grid))

;; (def adj-fns
;;   `([0 identity 1 dec] ;; u
;;     [0 inc, 1 identity] ;; r
;;     [0 identity, 1 inc] ;; d
;;     [0 dec, 1 identity])) ;; l

(defn get-adjacent-coords
  [[x y]]
  ;; (map #(u/evolve % coord) adj-fns)
  (list [x (dec y)]
        [(inc x) y]
        [x (inc y)]
        [(dec x) y]))

(defn cavity?
  [grid coord]
  (let [depth (grid coord)
        adjs (get-adjacent-coords coord)
        adj-depths (map grid adjs)]
    (every? #(and (number? %)
                  (> depth %)) adj-depths)))

(defn mark-cavities
  [grid coord]
  (if (cavity? (partial get-in grid) coord)
    (assoc-in grid coord "X")
    grid))

(defn cavity-map
  [length str-grid]
  (let [grid (parse-grid str-grid)
        inner-indices (range 1 (dec length))
        possible-cavity-coords (for [y inner-indices
                                     x inner-indices]
                                 [x y])]
    (reduce mark-cavities grid possible-cavity-coords)))

(defn output
  [grid]
  (map #(apply str %) grid))

(defn -main 
  [grid]
  (->> grid
       (cavity-map (count grid))
       output))