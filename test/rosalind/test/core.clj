(ns rosalind.test.core
  (:use [rosalind.core])
  (:use [clojure.test]))

(defn test_str [f in out]
	(= (apply str (f in)) out))
	
(deftest rna-test 
	(is (test_str rna_trans "GATGGAACTTGACTACGTAAATT" "GAUGGAACUUGACUACGUAAAUU" )))

(deftest reverse_complement 
	(is (test_str REVC "AAAACCCGGT" "ACCGGGTTTT")))

(deftest test-HAMM
	(is (= (HAMM "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT") 7)))

