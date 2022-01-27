(ns hackerrank.m-grid-search
  (:require [hackerrank.util :as u]
            [clojure.string :as str]))

(defn index-ofs
  "takes a string s and a search value, returns matches indexes"
  [s value]
  (let [find-from (partial str/index-of s value)]
    (loop [acc []
           from 0]
      (if-let [i (find-from from)]
        (recur (conj acc i) (inc i))
        acc))))

(defn row-match
  "checks if value matches in string s at the index i"
  [s value i]
  (= (str/index-of s value i) i))

(defn matches
  "checks if all pattern strings matches at index i in grid"
  [grid pattern i]
  (loop [grid grid
         row-patterns pattern]
    (cond
      (not row-patterns) true ;; pattern finished matching, all good
      (not grid) false ;; grid finished before pattern
      (not (row-match (first grid) (first row-patterns) i)) false ;; row doesn't match
      :else (recur (next grid) (next row-patterns)))))

(defn matches? [grid pattern]
  "checks if the given pattern is fully included on the grid"
  (let [pattern-start (first pattern)
        pattern-rest (rest pattern)
        xf (comp (map-indexed vector) ;; adds y to each grid row => [y "row string"]
                 ; u/xf-println
                 (map (u/use-i #(index-ofs % pattern-start) 1)) ;; gets matches x's (indexes) => [y [x1 x2...]]
                 ; u/xf-println
                 (filter #(pos? (count (second %)))) ;; filter in first row matches
                 ; u/xf-println
                 (mapcat (fn [[y xs]]
                           (mapv #(vector % y) xs))) ;; formats as individuals [x y] pairs
                 ; u/xf-println
                 (map (u/use-i #(drop (inc %) grid) 1)) ;; substitutes y for corresponding grid
                 ; u/xf-println
                 (filter (fn [[x grid]]
                           (matches grid pattern-rest x))) ;; filter in full matches
                 ; u/xf-println
                 (map (constantly true)))] ;; returns true!
    (transduce xf u/rf-first false grid)))

(defn gridSearch
  [grid pattern]
  (if (matches? grid pattern)
    "YES"
    "NO"))

(def grid ["7283455864"
           "6731158619"
           "8988242643"
           "3830589324"
           "2229595953"
           "5632338454"
           "6493735302"
           "7053106601"
           "0834282956"
           "4607924137"])

(def pattern ["9595"
              "3845"
              "3530"])