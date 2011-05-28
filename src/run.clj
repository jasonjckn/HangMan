(ns run
  (:use [core]
        [clojure.contrib
         [seq-utils :only [indexed]]]
        ))

(defn interactive2 []
  (println "starting")
  (def lazy-dict (lazy-dict!))
  (println "building meta-dict")
  (def meta-dict$ (meta-dict lazy-dict 35000))
  ;(def meta-dict$ (meta-dict lazy-dict 1000))
  (println "building meta-index")
  (def meta-index$ (build-meta-index meta-dict$))

  (do
    (println "Playing round.................")
    (print "What is the length of your word? (e.g. `4`): ")
    (flush)
    (def len (Integer/parseInt (read-line)))
    
    #_ conundrum
    (def index$ (meta-index$ len))
    (def dict$ (meta-dict$ len))

    (doseq [ foo (repeat 1)]
      (print "What is hang man state (e.g. `he_l_`): ")
      (flush)
      (def cnstr-str (read-line))
      #_ (def cnstr-str "h__l_")
      (def cnstr2 (into {}  (->> cnstr-str
                                (indexed)
                                (filter (fn [[_ ch]] (not (= ch \_))))
                                ;(vec)
                                )))
      (def ba (index-unique index$ cnstr2))
      #_ (println "expanding and mapping to words:")
      (def words$ (ba->words dict$ ba))
      #_ (println (map :word words$))
      (println "calculating char frequencies:")
      (dorun (map println (take 3 (dict-to-ch-freq words$ cnstr2)))))
    ))

(interactive2)

