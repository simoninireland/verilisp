;; Tests of helper macros
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


;; ---------- with-vl-errors-not-synthesisable ----------

(test test-helper-errors
  "Test we trap non-Verilisp errors."

  ;; traps and translates the divide-by-zero condition
  (signals (vl::not-synthesisable)
    (vl::with-vl-errors-not-synthesisable
      (let (a)
	(setq a (/ 12 0)))))

  ;; passes the Verilisp condition
  (signals (vl::unknown-variable)
    (vl:typecheck (vl::expand/vl '(let (a)
				   (setq b (+ 12 2)))))))


;; ---------- Unknown forms ----------

(test test-unknown-form
  "Test we detect unknown forms."
  (signals (dsl::unknown-form)
    (vl::typecheck (vl::expand/vl '(let (a)
				    (blig 34))))))


;; ---------- with-recover-on-error ----------

(test test-continue-on-error
  "Test we can continue with compilation after an error."
  (let ((errors 0)
	(result '()))

    (handler-bind
	((error (lambda (condition)
		  (declare (ignore condition))
		  ;; record the error
		  (incf errors)

		  ;; jump back in for the next element
		  (vl::recover))))

      (dolist (i (list 1 2 3 4))
	(vl::with-recover-on-error
	    ;; no recovery action needed
	    nil

	  (if (= i 2)
	      ;; 2 is bad...
	      (error "Bad thing happened")

	      ;; ...anyting else is fine
	      (appendf result (list i))))))

    (is (= errors 1))
    (is (equal result '(1 3 4)))))


(test test-continue-typecheck-on-error
  "Test that type-checking continues."
  (let ((errors 0))

    (handler-bind
	((vl::vl-error (lambda (condition)
			 ;; record the error
			 (incf errors)

			 ;; jump back in for the next element
			 (vl::recover))))

      (let ((ty (vl::typecheck (vl::expand/vl '(let ((a 45)
						   (b 0)
						   (c 2))
					      (setq d 123)
					      8)))))
	(is (vl::subtype-p ty '(unsigned-byte 4)))))

    (is (= errors 3)))) ;; one from dependencies, two from constraints
