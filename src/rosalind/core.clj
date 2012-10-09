(ns rosalind.core)

(def codon-table 
	{ "UUU" \F "CUU" \L "AUU" \I "GUU" \V
	"UUC" \F "CUC" \L "AUC" \I "GUC" \V
	"UUA" \L "CUA" \L "AUA" \I "GUA" \V
	"UUG" \L "CUG" \L "AUG" \M "GUG" \V
	"UCU" \S "CCU" \P "ACU" \T "GCU" \A
	"UCC" \S "CCC" \P "ACC" \T "GCC" \A
	"UCA" \S "CCA" \P "ACA" \T "GCA" \A
	"UCG" \S "CCG" \P "ACG" \T "GCG" \A
	"UAU" \Y "CAU" \H "AAU" \N "GAU" \D
	"UAC" \Y "CAC" \H "AAC" \N "GAC" \D
	"UAA" 0 "CAA" \Q "AAA" \K "GAA" \E
	"UAG" 0 "CAG" \Q "AAG" \K "GAG" \E
	"UGU" \C "CGU" \R "AGU" \S "GGU" \G
	"UGC" \C "CGC" \R "AGC" \S "GGC" \G
	"UGA" 0 "CGA" \R "AGA" \R "GGA" \G
	"UGG" \W "CGG" \R "AGG" \R "GGG" \G })

(defn rna-neucl [neucleotide]
  (if (= neucleotide \T)
    \U
    neucleotide))

(defn trans [neucleotide]
  (if (= neucleotide \A)
    \T
    (if (= neucleotide \C)
      \G
      (if (= neucleotide \G)
        \C
        \A))))

(defn count_one [[as cs gs ts] nucleotide]
  (if (= nucleotide \A)
    [(+ 1 as) cs gs ts]
    (if (= nucleotide \C)
      [as (+ 1 cs) gs ts]
      (if (= nucleotide \G)
        [as cs (+ 1 gs) ts]
	      (if (= nucleotide \T)
        	[as cs gs (+ 1 ts)]
        	[as cs gs ts])))))

(defn DNA [aseq]
	(let [counts (frequencies aseq)]
		[ (get counts \A ) (get counts \C) (get counts \G) (get counts \T)]))

(defn RNA [aseq]
	(loop [dna aseq trans (vector-of :char)]
		(if dna
			(recur (next dna) (conj trans (rna-neucl (first dna))))
			trans)))

(defn REVC [xs]
	(loop [re xs acc ()]
		(if re
			(recur (next re) (cons (trans (first re)) acc))
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

(defn PROT [rna]
	(loop [s rna prot (vector-of :char)]
		(if (>= (count s) 3)
			(let [codon (get codon-table (subs s 0 3) "")]
				(if (= codon 0)
					prot
					(recur (subs s 3) (conj prot codon))))			
			prot)))

(defn SUBS [s t]
	(loop [dna s motif t acc [] nextpos 0]
		(let [idx (+ 1 (.indexOf dna motif nextpos))]
			(if (> idx 0)
				(recur dna motif (conj acc idx) idx)
				acc))))

(defn cons_count_first [seq-vec]
	(loop [sv seq-vec counts [0 0 0 0] remainders []]
		(if sv 
			(let [sv1 (first sv)]			
				(recur (next sv) (count_one counts (first sv1))  (conj remainders (next sv1))))
			[counts remainders])))

(defn cons_count_all [seq-vec]
	(loop [sv seq-vec counts []]
		(if (first sv)
			(let [[cts remainders] (cons_count_first sv)]
				(recur remainders (conj counts cts)))
			counts)))

(defn cons-coll [cnts dna [as cs gs ts]]
	(if cnts
		(let [[a c g t] (first cnts) x (apply max (first cnts))]
			(recur (next cnts) (conj dna (get [\A \C \G \T] (.indexOf (first cnts) x))) [ (conj as a) (conj cs c) (conj gs g) (conj ts t)]))
		[(apply str dna) [as cs gs ts]]))

(defn show_cons_counts [counts c]
	(loop [cts counts acc [c \:]]
		(if cts
			(recur (next cts) (conj (conj acc \ ) (first cts)))
			(apply str acc))))

(defn CONS [sequences]
	(let [counts (cons_count_all sequences)] ; vector of [as cs gs ts] for each position in the sequence
		(let [[consensus [as cs gs ts]] (cons-coll counts [] [[][][][]])]
			[ consensus (show_cons_counts as \A) (show_cons_counts cs \C) (show_cons_counts gs \G) (show_cons_counts ts \T)])))	


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
				(println (func (first lines) (second lines)))
				(recur (next (next lines)))))))