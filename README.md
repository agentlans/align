# Align
Pairwise alignment of DNA sequences

Does [Needleman-Wunsch algorithm](https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm) to globally align a pair of DNA sequences. This code implements Hirschberg's variant of the algorithm and only requires linear space.

This repository is mainly for demonstration. For research or production, see also:
- EMBOSS Stretcher
- NCBI suite
- `Biostrings` package in R

## Install
Requires Common Lisp and ASDF (Another System Definition Facility).

Please copy this repository to `~/common-lisp` or wherever ASDF can find it. The code will be compiled the first time it's loaded in Lisp.

## Use
There is only one function for the alignment (`global-align`) and one scoring matrix (`*ednafull-scorer*`).

Example Lisp session:

```lisp
;; Load system
(asdf:load-system 'align)

;; Align two DNA sequences. Gap open penalty is 16, gap extend penalty is 4
(global-align "CCGGATACAT" "TGGCTTCAAGCATATGCGGT" *ednafull-scorer* 16 4)
;; => ("CC-GGA-TA-CA----T-------" . "--TGG-CT-TCAAGCATATGCGGT")
;; The dashes represent gaps
```

## Known issues

- This program outputs substitutions as one deletion and one insertion (so two "gaps" for each substitution).
- Gaps at the beginnings and ends of the sequences are used for scoring but may not be biologically meaningful.
- Alignments aren't unique as is the case for any pairwise alignment algorithm.

## Author, License
Copyright (C) 2020 Alan Tseng

GNU Public License v3
