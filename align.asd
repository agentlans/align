;;;; align.asd

(asdf:defsystem #:align
  :description "Pairwise global alignment of two DNA sequences"
  :author "Alan Tseng"
  :license  "GNU Public License v3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "align")
	       (:file "row-operations" :depends-on ("package" "align"))
	       (:file "matrices" :depends-on ("row-operations"))
	       (:file "hirschberg" :depends-on ("row-operations"))
	       (:file "random" :depends-on ("package" "align"))))
