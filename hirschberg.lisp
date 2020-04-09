
(defun argmax (lst)
  "Returns the index of the largest element in lst.
If largest element appears multiple times, only the first index
is returned."
  (let ((m (apply #'max lst)))
    (loop for x in lst
       for i from 0
       if (= x m) return i)))

(defun repeat-string (str times)
  "Repeats string a specified number of times and concatenates them.
Returns the result."
  (if (= times 0) ""
      (reduce (lambda (x y)
		(concatenate 'string x y))
	      (loop for i from 1 to times collect str))))
;; (repeat-string "1-" 5)

(defun concatenate-pairs (pair1 pair2)
  "Concatenates two pairs of strings."
  (cons (concatenate 'string (car pair1) (car pair2))
	(concatenate 'string (cdr pair1) (cdr pair2))))

;; last-row (str1 str2 s e d)

(defun hirschberg (a b scoring-func gap-open gap-extension)
  "Returns the alignment of strings a and b that maximizes
the alignment score using Hirschberg's variant
of Needleman-Wunsch algorithm.
This is algorithm C in Hirschberg's paper."
  (let ((m (length a))
	(n (length b)))
    (cond ((= m 0) ;; a is empty
	   (cons (repeat-string "-" n) b))
	  ((= n 0) ;; b is empty
	   (cons a (repeat-string "-" m)))
	  ((= m 1) ;; length(a) = 1 and b isn't empty
	   (let ((pos (position (character a) b)))
	     (cons (concatenate 'string
				(repeat-string "-" pos)
				a (repeat-string "-" (- n pos 1)))
		   b)))
	  ;; Split problem
	  (t (let* ((i (floor (/ m 2)))
		    ;; Find the scores for a[0:i] and reverse(a[i:m])
		    (l1 (last-row
			 (subseq a 0 i)
			 (subseq b 0 n)
			 scoring-func gap-open gap-extension))
		    (l2 (last-row
			 (reverse (subseq a i m))
			 (reverse (subseq b 0 n))
			 scoring-func gap-open gap-extension))
		    ;; Find cut point j in b that gives longest common substring
		    (k (argmax (loop for j from 0 to n collect
				    (+ (svref l1 j)
				       (svref l2 (- n j))))))
		    ;; Solve smaller subproblems
		    (c1 (hirschberg
			 (subseq a 0 i)
			 (subseq b 0 k)
			 scoring-func gap-open gap-extension))
		    (c2 (hirschberg
			 (subseq a i m)
			 (subseq b k n)
			 scoring-func gap-open gap-extension)))
	       (concatenate-pairs c1 c2))))))
