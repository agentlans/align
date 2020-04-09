;;;; align.asd

(asdf:defsystem #:align
  :description "Describe align here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "align")
	       (:file "row-operations" :depends-on ("package"))
	       (:file "matrices" :depends-on ("row-operations"))
	       (:file "hirschberg" :depends-on ("row-operations"))))
