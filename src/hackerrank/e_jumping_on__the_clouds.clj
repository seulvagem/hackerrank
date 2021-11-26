(ns hackerrank.jump-on-the-clouds)

(defn bad?
  [cloud]
  (= cloud 1))

(defn main
  [path]
  (let [length (count path)]
    (loop [i 0
           count 0]
      (if (>= i length)
        count
        (let [nn-i (+ i 2)
              nn-cloud (get path nn-i)]
          (if (bad? nn-cloud)
            (recur (+ i 3) (+ count 2))
            (if (and (nil? nn-cloud) (nil? (get path (inc i))))
              (recur nn-i count)
              (recur nn-i (inc count)))))))))