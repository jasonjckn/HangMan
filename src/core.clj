(ns core
  (:use [util]
        [pallet thread-expr]
        [clojure.contrib
         [seq-utils :only [indexed]]
         [map-utils :only [deep-merge-with]]]
        [clojure.java io]))

(def alpha (map char (range (int \a) (+ (int \z) 1))))

(defrecord DictEntry [word freq])

(defn lazy-dict! [] (line-seq (reader "dict.txt")))

(defnl parsed-dict [lazy-dict]
  (->> lazy-dict
       (map parse)
       (filter not-empty)
       (map record-fmt)
       (arg->> [recs]
               (map (normalize-freq-fn (max-freq recs)))))
  :where
  [parse (fn [line] (->> line (re-matches #"(\d+)\s([a-z]+).+") (drop 1)))
   record-fmt (fn [[a b]] (DictEntry. b (Integer/parseInt a)))

   max-freq #(:freq (first %))
   normalize-freq-fn (fn [max-freq]
                       (fn [rec]
                         (update-in rec [:freq] #(float (/ % max-freq)))))])

(defn meta-dict [lazy-dict trim-size]
  (->> lazy-dict
       (parsed-dict)
       (take trim-size)
       (group-by #(count (:word %)))))

(defnl bit-vector-to-num [bit-vector]
  (-> (bigint 0) (for-> [[k v] (indexed bit-vector)]
                        (when-> v (bit-set k)))))

(defnl build-index [meta-dict {:keys [len letter pos]}]
  (with-tag (index-data))

  :where [index-data (fn [] (->> (meta-dict len)
                                (map #(= (nth (:word %) pos) letter))
                                (bit-vector-to-num)))

          with-tag (fn [index] {len {letter {pos index}}})])

(defnl build-meta-index [meta-dict]
  (->> supported-index-types
       (map (partial build-index meta-dict))
       (apply deep-merge-with (fn [_ _] (throw (Exception.)))))
  :where
  [supported-index-types (for [len (keys meta-dict)
                               a alpha
                               p (range len)]
                           {:len len :letter a :pos p})])

(defnl index-unique [meta-index cnst]
  (reduce #(.and %1 %2) partial-cnst-ba)
  :where [partial-cnst-ba (for [[pos letter] cnst] ((meta-index letter) pos))])

(defnl bit-array->idxs [num]
  (->> (indexed-bit-seq num) (filter bit-set?) (map first))
  :where [indexed-bit-seq (fn [num] (for [i (range (.bitLength num))]
                                     [i (bit-test num i)]))
          bit-set? (fn [[idx bit]] (true? bit))])

(defnl ba->words [dict ba]
  (map dict idxs)
  :where [idxs (bit-array->idxs ba)])

(defnl extract-unbound-chars [word cnstr]
  (->> (indexed word)
       (filter unbound-pos)
       (map remove-indices))
  :where
  [bound-pos? (set (keys cnstr))
   unbound-pos (fn [[idx ch]] (not (bound-pos? idx)))
   remove-indices (fn [[_ ch]] ch)])

(defnl dict-to-ch-freq [dict cnstr]
  (->> dict
       (map ch-freq)
       (apply merge-with +)
       (sort-by (fn [[k v]] v))
       (reverse))
  :where
  [ch-freq #(zipmap (extract-unbound-chars (:word %) cnstr) (repeat (:freq %)))])

