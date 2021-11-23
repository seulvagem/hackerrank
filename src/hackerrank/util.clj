(ns hackerrank.util)

(defn get-parse-fn
  [length]
  (condp > length 
   10 #(Integer/parseInt %)
   19 #(Long/parseLong %)
   ##Inf bigint))

(defn parse-num
  [num-str]
  (let [parse-fn (get-parse-fn (count num-str))]
    (parse-fn num-str)))

(def parse-char #(Character/digit % 10))

(defn evolve
  "takes a map of fns (fn-map) and a map of data (d-map), for every key on fn-map and d-map applies the corresponding fn and updates d-map"
  [fn-map d-map]
  (let [fn-map-keys (set (keys fn-map))
        d-map-keys (set (keys d-map))
        relevant-keys (clojure.set/intersection fn-map-keys d-map-keys)
        relevant-fn-map (select-keys fn-map relevant-keys)]
    (merge-with #(%1 %2) relevant-fn-map d-map)))

;; pass-value-through
(defn use-i
  ([f]
   (use-i f 0))
  ([f n]
   (fn [arg-vec]
     (update arg-vec n f))))


;; transduce first
(def reduce-first
  "reducing function that returns the first item (useful on tranducers)"
  (completing (fn [_ x]
                (reduced x))))