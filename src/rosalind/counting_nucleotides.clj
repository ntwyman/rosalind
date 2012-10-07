(ns rosalind.counting-nucleotides)

(defn count_one [[as cs gs ts] nucleotide]
  (if (= nucleotide \A)
    [(+ 1 as) cs gs ts]
    (if (= nucleotide \C)
      [as (+ 1 cs) gs ts]
      (if (= nucleotide \G)
        [as cs (+ 1 gs) ts]
        [as cs gs (+ 1 ts)]))))

(defn count-neucl [data]
  (loop [counts [0 0 0 0], dna data]
    (if (empty? dna)
      counts
      (recur (count_one counts (first dna)) (rest dna)))))
