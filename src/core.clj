(ns core
  (:use [util]
        [pallet thread-expr]
        [clojure.contrib
         [seq-utils :only [indexed]]]
        [clojure.java io]))

(def bit-size 63)
(def alpha (map char (range (int \a) (+ (int \z) 1))))

(defrecord DictEntry [word freq])


(defn lazy-dict! []
  (line-seq (reader "dict.txt")))

(defnl parsed-dict [lazy-dict]
  parsed-&-normal
  :where
  [parsed (filter not-empty
                  (map #(->> % (re-matches #"(\d+)\s([a-z]+).+") (drop 1))
                       lazy-dict))

   record-fmt (map (fn [[a b]] (DictEntry. b (Integer/parseInt a))) parsed)

   max-freq (:freq (first record-fmt))
   normalize-freq #(float (/ % max-freq))

   parsed-&-normal (map #(update-in % [:freq] normalize-freq) record-fmt)])

(defn meta-dict [lazy-dict trim-size]
  (group-by #(count (:word %)) (take trim-size (parsed-dict lazy-dict))))

(defn bits-to-long [bits]
  (-> (int 0) (for-> [[k v] (indexed bits)]
                     (when-> v (bit-set k)))))

(defnl to-bit-array [is-bit-set?]
  (long-array (map bits-to-long seq-63-bits))
  
  :where [bit->idx #(int (Math/floor (/ % bit-size)))
          idx->bit #(* % bit-size)


          seq-63-bits (partition bit-size bit-size (repeat false) is-bit-set?)

          asize (+ 1 (bit->idx (count is-bit-set?)))])

(defnl build-index [dict letter pos]
  (to-bit-array (map #(= (nth (:word %) pos) letter) dict)))

(defnl build-meta-index [meta-dict]
  (len->a->pos->index)
  
  :where [pos->index (fn [dict len a]
                       (into {} (for [p (range len)]
                                  [p (build-index dict a p)])))

          a->pos->index (fn [dict len]
                          (into {} (for [a alpha]
                                     [a (pos->index dict len a)])))
          
          len->a->pos->index (fn [] (into {} (map
                                             (fn [[len dict]]
                                               [len (a->pos->index dict len)])
                                             meta-dict)))])

(defnl and-arrays [as]
  (doseq [a as]
    (doseq [[idx, elem] (indexed a)]
      (aset ret idx (bit-and (aget ret idx) (aget a idx)))))
  ret
  :where [sz (alength (first as))
          max-long (long (bits-to-long (range bit-size)))
          ret (long-array sz max-long)])


(defnl index-unique [index cnst]
  (and-arrays partial-cnst-ba)
  :where [partial-cnst-ba (for [[pos letter] cnst]
                            ((index letter) pos))])

(defnl bit-array->idxs [ba]
  (map (comp long first) (filter #(let [[a b] %] b) (indexed expanded)))
  :where [expanded (for [i (range (alength ba)) j (range bit-size)]
                     (bit-test (aget ba i) j))])

(defnl ba->words [dict ba]
  (map dict idxs)
  :where [idxs (bit-array->idxs ba)])

(defnl extract-unbound-chars [word cnstr]
  (->> (indexed word)
       (filter-bound-chars)
       (remove-indices))
  :where
  [bound-pos? (set (keys cnstr))
   filter-bound-chars #(filter (fn [[idx ch]] (not (bound-pos? idx))) %)
   remove-indices #(map (fn [[_ ch]] ch) %)])

(defnl dict-to-ch-freq [dict cnstr]
  (reverse (sort-by (fn [[k v]] v) merged))
  :where
  [unmerged (for [d dict]
              (zipmap (extract-unbound-chars (:word d) cnstr)
                      (repeat (:freq d))))
   merged (apply merge-with + unmerged)
   cmp #(- (compare (merged %1) (merged %2)))])

(defn main []
  (println "starting")
  (def lazy-dict (lazy-dict!))
  (println "building meta-dict")
  (def meta-dict$ (meta-dict lazy-dict 100000))
  (println "building meta-index")
  (def meta-index$ (build-meta-index meta-dict$))

  (do
    (println "Playing round.................")
    (def len 9)
    #_ conundrum
    (def index$ (meta-index$ len))
    (def dict$ (meta-dict$ len))
    #_ (println "uniquing")
    (def cnstr {0 \c
                1 \o
                2 \n
                5 \d
                4 \n
                })
    (def ba (index-unique index$ cnstr))
    #_ (println "expanding and mapping to words:")
    (def words$ (ba->words dict$ ba))
    #_ (println (map :word words$))
    #_ (println "calculating char frequencies:")
    (dorun (map println (take 3 (dict-to-ch-freq words$ cnstr))))
    )
  )

(defn -main []
  (time (main)))

#_ (def indices (build-meta-index))
#_ (def ba (index-unique indices 3 {0 \h 1 \e} ))
#_ (ba->words 3 ba)
#_ (def len 3)
#_ (def ba-1 ret)


#_ (let [letter \o
      dict (cd 3)
      is-bit-set? (for [w dict] (= (nth w pos) letter))
      pos 0
      w "is"])




