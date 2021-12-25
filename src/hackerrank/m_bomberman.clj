(ns hackerrank.m-bomberman
  (:require [hackerrank.util :as u]
            [clojure.string :as str]))

(defn get-in-grid
  [grid coord]
  (get-in grid (reverse coord)))


(defn bomberman-turn?
  [i]
  (even? i))

(def nothing 0)

(defn nothing?
  [val]
  (= val nothing))

(def bomb 4)

(defn put-bombs
  [grid]
  (mapv #(mapv (fn [val]
                 (if (nothing? val)
                   bomb
                   val)) %) grid))

(defn bomberman-do-its-thing
  [i grid]
  (if (bomberman-turn? i)
    (put-bombs grid)
    grid))

;; a cada ação salvar quais locais apagar (set?), depois limpar todos juntos
(defn explode
  [grid coords]
  (let [xf (comp (map (juxt identity #(get-in-grid grid %)))
                 (filter #(= (second %) 1))
                 (map first)
                 (mapcat #(conj (u/get-adjacent-coords %) %)))]
    (into #{} xf coords)))

(defn sec
  [grid]
  (let [coords (for [y (range (count grid))
                     x (range (count (first grid)))]
                 [x y])
        coords-to-clear (explode grid coords)]
    (reduce (fn [grid coord]
              (update-in grid (reverse coord)
                         (fn [val]
                           (if (coords-to-clear coord)
                             0
                             (if (pos? val)
                               (dec val)
                               val))))) grid coords)))
(def full-sec
  (comp sec bomberman-do-its-thing))

(defn bomberman
  [n grid]
  (reduce (fn [grid i]
            (let [n-grid (full-sec i grid)]
              (if (= n i)
                (reduced n-grid)
                n-grid))) grid (range n)))

(defn char->val
  [c]
  (if (= c \O)
    (dec (dec bomb))
    nothing))

(defn input->grid
  [strs]
  (let [xf (map #(mapv char->val %))]
    (into [] xf strs)))

(defn val->char
  [val]
  (if (pos? val)
    "O"
    "."))

(defn grid->output
  [grid]
  (mapv #(str/join (map val->char %)) grid))


(defn bomberMan
  [a input]
  (let [b (dec a)
        c (if (< b 7)
            b
            (+ 2 (rem b 4)))]
    (grid->output (bomberman c (input->grid input)))))

(defn bomberMano
  [n input]
  (grid->output (bomberman (dec n) (input->grid input))))

(def input ["......."
            "...O..."
            "....O.."
            "......."
            "OO....."
            "OO....."])

(def input2  ["......."
              "...O.O."
              "....O.."
              "..O...."
              "OO...OO"
              "OO.O..."])

(defn -main
  []
  (bomberMan 3 input))

(defn some-ret
  [pred coll]
  (some #(when (pred %)
           %) coll))

(defn =sec
  [s1 s2]
  (apply = (map (u/use-i even?) (list s1 s2))))

(defn get-cycle-size
  [grid]
  (transduce (comp (map #(bomberman % grid))
                   (map-indexed vector))
             (completing
              (fn [prevs [i, :as tup]]
                (if-let [[prev-i] (some-ret #(=sec tup %) prevs)]
                  (reduced [prev-i i])
                  (conj prevs tup))))
             '()
             (range 0 20)))