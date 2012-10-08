(ns rosalind.test.core
  (:use [rosalind.core])
  (:use [clojure.test]))

(defn test_str [f in out]
	(= (apply str (f in)) out))

(deftest dna-test
  (is (= (DNA "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") [20 12 17 21])))

(deftest rna-test 
	(is (test_str RNA "GATGGAACTTGACTACGTAAATT" "GAUGGAACUUGACUACGUAAAUU" )))

(deftest revc-test
	(is (test_str REVC "AAAACCCGGT" "ACCGGGTTTT")))

(deftest hamm-test
	(is (= (HAMM "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT") 7)))

(deftest prot-test
	(is (test_str PROT "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA" "MAMAPRTEINSTRING")))

(deftest prot-test
	(is (= (SUBS "ACGTACGTACGTACGT" "GTA") [3 7 11])))
