(ns test.core
  (:use
   [core]
   [pallet.thread-expr]
   [clojure.test]))


(deftest test-array-and
  (is (= [1 1] (seq (and-arrays [(int-array [3 7])
                                 (int-array [1 1])]))))
  (is (= [1 3] (seq (and-arrays [(int-array [3 7])
                                 (int-array [1 3])
                                 (int-array [3 3])])))))

(deftest test-bits-to-ints
  (is (= (bits-to-int (repeat 31 true)) 2147483647)))

#_ (deftest test-cnst-dict
  (is (= (seq (cnst-dict agg 3 {0 \o})) (((agg 3) \o) 0))))






