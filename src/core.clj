(ns core
  (:use [util]
        [pallet thread-expr]
        [clojure.contrib
         [seq-utils :only [indexed]]
         [pprint :only [pprint-map]]]
        [clojure.java io]))

(def d (filter not-empty
               (map #(->> % (re-matches #"\d+\s([a-z]+).+")
                          (drop 1) (first))
                    (line-seq (reader "dict.txt")))))

(def d$ (take 200 d))

(def len->dict (group-by count d$))

(defn bits-to-int [bits] (-> (int 0) (for-> [[k v] (indexed bits)]
                                            (when-> v (bit-set k)))))

(def bit-size 31)

(defnl to-bit-array [is-bit-set?]
  (int-array (map bits-to-int seq-32-bits))
  
  :where [
          bit->idx #(int (Math/floor (/ % bit-size)))
          idx->bit #(* % bit-size)


          seq-32-bits (partition bit-size bit-size (repeat false) is-bit-set?)

          asize (+ 1 (bit->idx (count is-bit-set?)))])

(defnl build-index [dict letter pos]
  (to-bit-array (map #(= (nth % pos) letter) dict)))


(def alpha (map char (range (int \a) (+ (int \z) 1))))

#_ (def agg
     (into {} (for [[len, dict] len->dict]
                [len (into {} (for [a alpha]
                                [a (into {} (for [p (range len)]
                                              [p (build-index dict a p)]))]))])))
(defnl and-arrays [& as]
  (doseq [a as]
    (doseq [[idx, elem] (indexed a)]
      (aset ret idx (bit-and (aget ret idx) (aget a idx)))))
  ret
  :where [sz (alength (first as))
          max-int (int (bits-to-int (range bit-size)))
          ret (int-array sz max-int)])





#_ (let [letter \o
      dict (cd 3)
      is-bit-set? (for [w dict] (= (nth w pos) letter))
      pos 0
      w "is"])




