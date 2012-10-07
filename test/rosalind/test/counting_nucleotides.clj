(ns rosalind.test.counting-nucleotides
  (:use [rosalind.counting-nucleotides])
  (:use [clojure.test]))

(deftest simple-test
  (is (= (count-neucl "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") [20 12 17 21])))