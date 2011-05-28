(ns core
  (:use [util]
        [pallet thread-expr]
        [clojure.contrib
         [seq-utils :only [indexed]]
         [map-utils :only [deep-merge-with]]]
        [clojure.java io]))

(def bit-size 63)
(def alpha (map char (range (int \a) (+ (int \z) 1))))

(defrecord DictEntry [word freq])

(defn lazy-dict! []
  ;; Get it from: http://www.wordfrequency.info/500k_words.asp
  ;;
  ;; The first 3 lines of dict.txt should look like:
  ;; 22995878	the	at	169011
  ;; 11239776	and	cc	168844
  ;; 10246048	of	io	168743
  (line-seq (reader "dict.txt")))

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

(defn bits-to-long [bits]
  (-> (long 0) (for-> [[k v] (indexed bits)]
                     (when-> v (bit-set k)))))

(defnl to-bit-array [is-bit-set?]
  (->> is-bit-set?
       (partition bit-size bit-size (repeat false))
       (map bits-to-long)
       (long-array)))

(defnl build-index [meta-dict {:keys [len letter pos]}]
  (with-tag (index-data))

  :where [index-data (fn [] (->> (meta-dict len)
                                (map #(= (nth (:word %) pos) letter))
                                (to-bit-array)))

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

(defnl and-arrays [[a1 & as]]
  (and-arrays-into-x!) x
  :where
  [x (aclone a1)
   and-arrays-into-x! #(doseq [a as
                               [idx, elem] (indexed a)]
                         (aset-long x idx (bit-and (aget x idx) (aget a idx))))])

(defnl index-unique [index cnst]
  (and-arrays partial-cnst-ba)
  :where [partial-cnst-ba (for [[pos letter] cnst]
                            ((index letter) pos))])

(defnl bit-array->idxs [ba]
  (->> (bit-seq ba)
       (indexed)
       (filter (fn [[_ b]] b))
       (map (comp long first)))
  :where [bit-seq (fn [array] (for [elem array i (range bit-size)]
                               (bit-test elem i)))])

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

