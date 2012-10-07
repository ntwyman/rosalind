(ns rosalind.core)
(use '[rosalind.counting-nucleotides :only (count-neucl)])

(defn rna-neucl [neucleotide]
  (if (= neucleotide \T)
    \U
    neucleotide))

(defn compl [neucleotide]
  (if (= neucleotide \A)
    \T
    (if (= neucleotide \C)
      \G
      (if (= neucleotide \G)
        \C
        \A))))

(defn counts [seq]
	(when seq
		(do
			(println (count-neucl (first seq)))
			(recur (next seq)))))

(defn rna_trans [seq]
	(if seq
		(cons (rna-neucl (first seq)) (rna_trans (next seq)))
		[]))

(defn REVC [xs]
	(loop [re xs acc {}]
		(if re
			(recur (next re) (cons (compl (first re)) acc))
			acc )))

(defn HAMM [seqa seqb]
	(loop [l seqa r seqb ham 0]
		(if (and l r)
			(let [ha (first l) hb (first r) xa (next l) xb (next r)]
				(if (= ha hb)
					(recur xa xb ham)
					(recur xa xb (+ 1 ham))))
			(if l
				(count l)
				(if r
					(count r)
					ham)))))

(defn do_test [in-name]
	(with-open [rdr (java.io.BufferedReader. (java.io.FileReader. in-name))]
	(let [lines (line-seq rdr)]
		(counts lines))))


(defn each_line [file_name func]
	(with-open [rdr (java.io.BufferedReader. (java.io.FileReader. file_name))]
		(loop [lines (line-seq rdr)]
			(when lines
				(func (first lines))
				(recur (next lines))))))

(defn each_line_println [file_name func]
	(each_line file_name (fn [s] (println (apply str (func s))))))

(defn line_pairs [file_name func]
	(with-open [rdr (java.io.BufferedReader. (java.io.FileReader. file_name))]
		(loop [lines (line-seq rdr)]
			(when lines
				(func (first lines) (second lines))
				(recur (next (next lines)))))))