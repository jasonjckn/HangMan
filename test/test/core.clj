(ns test.core
  (:use
   [core]
   [pallet.thread-expr]
   [clojure.test]))


(deftest test-array-and
  (is (= [1 1] (seq (and-arrays [(long-array [3 7])
                                 (long-array [1 1])]))))
  (is (= [0 0] (seq (and-arrays [(long-array [9 0])
                                 (long-array [0 1])]))))
  (is (= [0 1] (seq (and-arrays [(long-array [9 3])
                                 (long-array [6 5])]))))
  (is (= [1 3] (seq (and-arrays [(long-array [3 7])
                                 (long-array [1 3])
                                 (long-array [3 3])])))))

(deftest test-bits-to-longs
  (is (= (bits-to-long (repeat 31 true)) 2147483647))
  (is (= (bits-to-long [true true]) 3))
  (is (= (bits-to-long [true true true]) 7))
  (is (= (bits-to-long [false false false true]) 8)))






