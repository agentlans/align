;;  This file is part of align: pairwise global DNA alignment
;;  Copyright (C) 2020  Alan Tseng

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defun scorer (mat letters)
  "Returns a function that scores nucleotides or amino acids.
mat is a 2D array whose rows and columns correspond to indices in letters."
  (flet ((get-index (x)
	   (position x letters)))
    (lambda (x1 x2)
      (aref mat
	    (get-index x1)
	    (get-index x2)))))
#|(defparameter *foo*
  (scorer (make-array
	   '(2 2) :initial-contents #((0 1) (2 3)))
	  '(#\a #\b)))
(funcall *foo* #\b #\b)|#

(defun max2 (lst)
  "Returns the maximum number in a list ignoring nils"
  (apply #'max
	 (remove-if-not #'identity lst)))
;; (max2 '(0 1 100 nil 200))

(defun -- (x y)
  "Subtraction with nils."
  (if (or (not x) (not y))
      nil
      (- x y)))

(defun as-vector (lst)
  (coerce lst 'vector))

;; These functions return a row of matrix
;; n is length of second string
;; e is gap open penalty
;; d is gap extension penalty

;; See http://www.cs.hunter.cuny.edu/~saad/courses/compbio/lectures/lecture6.pdf for explanation of a, b, c

;; a is optimal score that ends with no gaps
(defun a-row (i n)
  (funcall #'as-vector
	   (loop for j from 0 to n collect
		(cond ((and (= i 0) (= j 0)) 0)
		      (t nil)))))
;; (a-row 0 3)

;; b is optimal score that ends with gap aligned with x
(defun b-row (i n e d)
  (funcall #'as-vector
	   (loop for j from 0 to n collect
		(cond ((and (= i 0) (= j 0)) 0)
		      ((and (> i 0) (= j 0))
		       (- (- e) (* d (- i 1))))
		      (t nil)))))

;; c is optimal score that ends with gap aligned with y
(defun c-row (i n e d)
  (funcall #'as-vector
	   (loop for j from 0 to n collect
		(cond ((and (= i 0) (= j 0)) 0)
		      ((and (= i 0) (> j 0))
		       (- (- e) (* d (- j 1))))
		      (t nil)))))

(defun last-row (str1 str2 s e d)
  "Returns last row of matrix made during alignment of
str1 and str2. 
s is function that returns substitution score of two letters.
e is gap open penalty.
d is gap extension penalty."
  (declare (string str1 str2) (function s) (integer e d))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((m (length str1))
	 (n (length str2))
	 ;; a, b, c vectors
	 (av (funcall #'a-row 0 n))
	 (bv (funcall #'b-row 0 n e d))
	 (cv (funcall #'c-row 0 n e d))
	 ;; a, b, c vectors of the previous row
	 (avp nil)
	 (bvp nil)
	 (cvp nil))
    ;; Calculate each row from previous row
    (loop for i from 1 to m do
	 (progn
	   (setf avp av
		 bvp bv
		 cvp cv)
	   ;; Initialize new rows
	   (setf av (funcall #'a-row i n)
		 bv (funcall #'b-row i n e d)
		 cv (funcall #'c-row i n e d))
	   (loop for j from 1 to n do
		(progn
		  (setf (svref av j)
			(+ (funcall #'max2
				    (list
				     (svref avp (- j 1))
				     (svref bvp (- j 1))
				     (svref cvp (- j 1))))
			   (funcall s (schar str1 (- i 1))
				    (schar str2 (- j 1)))))
		  (setf (svref bv j)
			(funcall #'max2
				 (list
				  (funcall #'-- (svref avp j) e)
				  (funcall #'-- (svref bvp j) d)
				  (funcall #'-- (svref cvp j) e))))
		  (setf (svref cv j)
			(funcall #'max2
				 (list
				  (funcall #'-- (svref av (- j 1)) e)
				  (funcall #'-- (svref bv (- j 1)) e)
				  (funcall #'-- (svref cv (- j 1)) d))))))))
    ;; Return results from the last iteration
    (as-vector
     (loop for i from 0 to n collect
	  (funcall #'max2
		   (list (svref av i)
			 (svref bv i)
			 (svref cv i)))))))

