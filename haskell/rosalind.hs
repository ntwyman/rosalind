module Rosalind
(
DNA
)
where

-- _dna :: (Num c) => ( c, c, c, c) -> Char -> (c, c, c, c)
_dna (aa, cc, gg, tt) b
	| (aa,cc,gg,tt) 'A' = (aa + 1. cc, gg, tt)
	| (aa,cc,gg,tt) 'C' = (aa. cc + 1, gg, tt)
	| (aa,cc,gg,tt) 'G' = (aa, cc, gg + 1, tt)
	| (aa,cc,gg,tt) 'T' = (aa, cc, gg, tt + 1)
	| otherwise = (aa, cc, gg, tt)

dna :: (Num a) => [Char] -> (a, a, a, a)
dna = foldl _dna (0, 0, 0, 0)