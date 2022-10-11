(ns hackerrank.e-string-codification
  (:require
   [clojure.string :as str]))

(def test-cases [["HELLO WORLD" 2]
                 ["HELLO WORLD" 3]
                 ["my name is" 3]])

(defn pad-join [length vec count]
  (let [diff       (- length count)
        padding    (repeat diff \_)
        padded-vec (into vec padding)]
    (str/join padded-vec)))

(defn string->code
  ([n vecs]
   (let [counts (map count vecs)
         length (apply max counts)]
     (mapv (partial pad-join length) vecs counts)))
  ([n vecs [ix char]]
   (let [vec-ix (rem ix n)]
     (update vecs vec-ix conj char))))

(defn get-initial-vecs [n]
  (mapv #(into [] (repeat % \_)) (range n)))

(defn string-codification [s n]
  (let [initial-vecs     (get-initial-vecs n)
        xf-add-index     (map-indexed vector)
        xf-replace-space (map #(if (= % \ )
                                 \_
                                 %))
        xf               (comp xf-replace-space xf-add-index)
        rf-string->code  (partial string->code n)]
    (transduce xf rf-string->code initial-vecs s)))


(defn -main
  ([] (-main test-cases))
  ([input-test-cases]
   (map #(apply string-codification %) input-test-cases)))
