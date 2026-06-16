;;;; Maths helper functions
;;;;
;;;; Copyright (C) 2024--2026 Simon Dobson
;;;;
;;;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;;;
;;;; verilisp is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; verilisp is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/utils)


;;; ---------- Bit-level operations ----------

;;; These operations are needed because COERCE (in Common Lisp) can't
;;; coerce bit arrays to and from integers.

(defun bit-array-to-integer (a)
  "Convert A to an integer formed from the corresponding bits.

The bit at index 0 of the array becomes bit 0 (the lowest-order bit)
of the number."
  (let* ((w (array-dimension a 0))
	 (i (1- w))
	 (v 0))

    (dotimes (j w v)
      (setq v (+ (ash v 1) (aref a i)))
      (decf i))))


(defun bits-for-integer (n)
  "Return the number of bits needed to represent N.

This accounts for the sign bit needed if N is negative,
and will always return a minimum of 1 bit."
  (flet ((bfi (val)
	   (multiple-value-bind (b res)
	       (ceiling (log val 2))

	     (let ((bits (max (if (= res 0.0)
				  ;; add a bit if val is on a
				  ;; power-of-two boundary
				  (1+ b)
				  b)
			      1)))	; always need at least one bit
	       bits))))

    (cond ((= n 0)
	   1)

	  ((> n 0)
	   (bfi n))

	  (t
	   ;; add sign bit
	   (1+ (bfi (abs n)))))))


(defun integer-to-bit-array (n &optional bits)
  "Convert an integer N to a bit array.

The lowest-order bit of N becomes the bit at index 0 of the array.

The array is created to be long enough to hold all the non-zero
bits of N, unless BITS is specified, in which case it gets that
many bits. This will lose precision if not enough biots are
requested."
  (let* ((w (or bits (bits-for-integer n)))
	 (a (make-array (list w) :element-type 'bit :initial-element 0)))

    (dotimes (i w a)
      (setf (aref a i) (logand n #2r1))
      (setf n (ash n (- 1))))))
