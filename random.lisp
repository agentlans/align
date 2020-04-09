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

(defun random-sample (lst)
  "Returns random element of lst."
  (elt lst (random (length lst))))

(defun random-dna (len)
  "Returns a random DNA sequence of specified length."
  (funcall (lambda (x) (concatenate 'string x))
	   (loop for i from 1 to len collect
		(random-sample '(#\A #\T #\G #\C)))))

