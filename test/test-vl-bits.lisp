;; Tests of bitwise access
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/vl)


;; ---------- Single-bit access ----------

(test test-width-bit
  "Test we can extract a bit from a value."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
						  (vl::bref a 1))))
		    '(unsigned-byte 1))))


(test test-width-bits
  "Test we can extract bits from a value."
  ;; single-bit equivalents
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
						  (bref a 1 :end 1))))
		    '(unsigned-byte 1)))
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
						  (bref a 1 :width 1))))
		    '(unsigned-byte 1)))

  ;; multiple bits
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
						  (bref a 1 :width 2))))
		    '(unsigned-byte 2)))
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
						  (bref a 1 :end 0))))
		    '(unsigned-byte 2)))
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
						     (declare (width 8 a))
						     (bref a 7))))
		    '(unsigned-byte 8)))

  ;; matching and non-matching explicit widths
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
						  (bref a 4 :end 2 :width 3))))
		    '(unsigned-byte 3)))
  (signals (vl::type-mismatch)
    (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
				  (bref a 4 :end 2 :width 4))))))


(test test-positive-start-end-width
  "Test that we detect non-positive values."
  (signals (vl::value-mismatch)
    (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
				  (vl::bref a 4 :end -1)))))
  (signals (vl::value-mismatch)
    (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
				  (vl::bref a 4 :width -1)))))
  (signals (vl::value-mismatch)
    (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
				  (vl::bref a -2 :end 0)))))
  (signals (vl::value-mismatch)
    (vl::typecheck (vl::expand/vl '(let ((a #2r10110))
				  (vl::bref a -2))))))


;; ---------- Generalised place ----------

(test test-bit-dependencies
  "Test we can extract bref dependencies properly."
  (vl::with-new-frame
    (vl::declare-variable 'a '((type (unsigned-byte 8))
			       (initial-value 12)))
    (vl::declare-variable 'b '((type (unsigned-byte 8))
			       (initial-value 24)))
    (vl::declare-variable 'c '((type (unsigned-byte 8))
			       (initial-value 0)))
    (vl::declare-variable 'd '((type (unsigned-byte 8))
			       (initial-value 0)
			       (as constant)))

    (vl::expand/vl '(progn
		     (setq a (+ (vl::bref b d :end 0) 19))
		     (setq c a)))

    ;; not (b d) as d is a constant
    (is (set-equal (vl::variable-property 'a 'depends-on)
		   '(b)))

    (is (null (vl::variable-property 'b 'depends-on)))

    (is (set-equal (vl::variable-property 'c 'depends-on)
		   '(a)))

    ;; not (a b d), for the same reasons as above
    (is (set-equal (vl::traverse-dependencies 'c)
		   '(a b)))
    (is (null (vl::variable-property 'd 'depends-on)))))


(test test-bit-target
  "Test we can use bitfields as targets."
  (vl::with-new-frame
    (vl::declare-variable 'a '((type (unsigned-byte 8))
			       (initial-value 12)))
    (vl::declare-variable 'b '((type (unsigned-byte 8))
			       (initial-value 24)))
    (vl::declare-variable 'c '((type (unsigned-byte 8))
			       (initial-value 0)))

    (vl::expand/vl '(setf (bref a 2 :end 0) 0))
    (is (null (vl::variable-property 'a 'depends-on)))

    (vl::expand/vl '(setf (bref a 4 :end 2) (bref b 2 :end 0)))
    (is (set-equal (vl::variable-property 'a 'depends-on)
		   '(b)))


    (vl::expand/vl '(setf (bref c 4 :end 2) (bref c 2 :end 0)))
    (is (set-equal (vl::variable-property 'c 'depends-on)
		   '(c)))))
