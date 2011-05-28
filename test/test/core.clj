(ns test.core
  (:use
   [core]
   [clojure.test]))


(deftest array-and
  (is (= [0 0] (seq (and-arrays (int-array [0 0])
                                (int-array [0 0])))))
  (is (= [1 1] (seq (and-arrays (int-array [1 1])
                                (int-array [1 1])))))
  (is (= [3 3] (seq (and-arrays (int-array [3 3])
                                (int-array [3 3])))))
  (is (= [0 1] (seq (and-arrays (int-array [3 7])
                                (int-array [0 1])))))
  (is (= [1 1] (seq (and-arrays (int-array [3 7])
                                (int-array [1 1])))))
  (is (= [1 3] (seq (and-arrays (int-array [3 7])
                                (int-array [1 3])
                                (int-array [3 3]))))))

(deftest bits-to-ints
  (is (= (bits-to-int (repeat 31 true)) 2147483647)))

