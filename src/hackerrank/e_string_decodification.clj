(ns hackerrank.e-string-decodification
  (:require
   [clojure.string :as str]))

(def test-cases
  '(["HLOWRD" "_EL_OL"] ["HLWL_" "_EOOD" "__L_R"] ["mnes_" "_ya__" "___mi"]))

(defn rem+quot [x y]
  (+ (rem x y) (quot x y)))

(defn rf-char->string
  ([v char]
   (conj v char ))
  ([v]
   (-> v str/join str/trimr)))

(defn string-decodification [strs]
  (let [n            (count strs)
        ;;
        y-seq        (cycle (range n))
        x-seq        (map #(rem+quot % n) (range))
        yx-seq       (map vector y-seq x-seq)
        ;;
        xf-replace-_ (map #(if (= % \_)
                             \ ;; preventing space removal
                             %))
        xf-take      (take-while identity)
        xf-get-char  (map #(get-in strs %))
        xf           (comp xf-get-char xf-take xf-replace-_)]
    (transduce xf rf-char->string [] yx-seq)))

(defn -main
  ([] (-main test-cases))
  ([input-test-cases]
   (map string-decodification input-test-cases)))
