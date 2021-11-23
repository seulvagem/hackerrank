(ns hackerrank.m-grid-search
  (:require [hackerrank.util :as u]
            [clojure.string :as str]))

;; (def grid ["7283455864"
;;            "6731158619"
;;            "8988242643"
;;            "3830589324"
;;            "2229505813"
;;            "5633845374"
;;            "6473530293"
;;            "7053106601"
;;            "0834282956"
;;            "4607924137"])

;; (def pattern ["9505"
;;               "3845"
;;               "3530"])

(defn index-ofs
  [s value]
  (let [find-from (partial str/index-of s value)]
    (loop [acc []
           from 0]
      (if-let [i (find-from from)]
        (recur (conj acc i) (inc i))
        acc))))

(defn row-match
  [s value i]
  (= (str/index-of s value i) i))

(defn matches
  [grid pattern x]
  (loop [grid grid
         pattern pattern]
    (cond
      (not pattern) true ;; pattern finished matching
      (not grid) false ;; grid finished before pattern
      (not (row-match (first grid) (first pattern) x)) false ;; row dont match
      :else (recur (next grid) (next pattern)))))

(defn matches? [grid pattern]
  (let [pattern-start (first pattern)
        pattern-rest (rest pattern)
        xf (comp (map-indexed vector) ;; adds y
                 (map (u/use-i #(index-ofs % pattern-start) 1)) ;; gets x's
                 (filter #(pos? (count (second %)))) ;; filter in first row matches
                 (mapcat (fn [[y xs]]
                           (mapv #(vector % y) xs))) ;; formats as [x y]
                 (map (u/use-i #(drop (inc %) grid) 1)) ;; substitutes y for corresponding grid
                 (filter (fn [[x grid]]
                           (matches grid pattern-rest x))) ;; filter in full matches
                 (map (constantly true)))] ;; returns true!
    (transduce xf u/reduce-first false grid)))

(defn gridSearch
  [grid pattern]
  (if (matches? grid pattern)
    "YES"
    "NO"))
