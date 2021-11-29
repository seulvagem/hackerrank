(ns hackerrank.core)

(defn rf-pangram
  ([res]
;;    (println res)
   (not res))
  ([letter-set letter-c]
   (let [n-set (conj letter-set letter-c)]
                    ;;   (println letter-c)
     (if (> (count n-set) 26)
       (reduced false)
       n-set))))

(defn pangram?
  [s]
  (transduce (map #(Character/toLowerCase %))
             rf-pangram #{\ } s))

(defn pangrams [s]
  (if (pangram? s)
    "pangram"
    "not pangram"))
