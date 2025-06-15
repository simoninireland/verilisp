;; Tests of macro expansion pass
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


;; ---------- Macros imported from Common Lisp unchanged ----------

(test test-expand-cond
  "Test we can expand the COND macro."
  (let* ((f '(cond ((= a 12)
		    (+ a 1))
	      ((> a 34)
	       (+ a 67))
	      (t
	       0)))
	 (g (vl::expand-macros-in-environment f)))
    (is (equal g '(if (= a 12)
		   (+ a 1)
		   (if (> a 34)
		       (+ a 67)
		       (the t 0)))))))


(test test-no-expand-and
  "Test we don't expand AND, which is a macro in Common Lisp."
  (is (equal (vl::expand-macros-in-environment '(and a b))
	     '(and a b))))


;; ---------- Macros (re-)implemented ----------

(test test-expand-when
  "Test we can expand WHEN conditionals."
  (is (equal (vl::expand-macros-in-environment '(when (= a b)
				   (+ a 1)
				   (- b 1)))
	     '(if (= a b)
	       (progn
		 (+ a 1)
		 (- b 1))))))


(test test-expand-unless
  "Test we can expand UNLESS conditionals."
  (is (equal (vl::expand-macros-in-environment '(unless (= a b)
				   (+ a 1)
				   (- b 1)))
	     '(if (not (= a b))
	       (progn
		 (+ a 1)
		 (- b 1))))))


(test test-expand-incf
  "Test we canm expand INCF macros."
  (is (equal (vl::expand-macros-in-environment '(let ((a 1))
				   (incf a)))
	     '(let ((a 1))
	       (progn
		 (setf a (+ a 1))))))

  (is (equal (vl::expand-macros-in-environment '(let ((a 1))
				   (incf (bit a 5))))
	     '(let ((a 1))
	       (progn
		 (setf (bit a 5) (+ (bit a 5) 1)))))))


(test test-expand-decf
  "Test we canm expand DECF macros."
  (is (equal (vl::expand-macros-in-environment '(let ((a 1))
				   (decf a)))
	     '(let ((a 1))
	       (progn
		 (setf a (+ a 1))))))

  (is (equal (vl::expand-macros-in-environment '(let ((a 1))
				   (decf (bit a 5))))
	     '(let ((a 1))
	       (progn
		 (setf (bit a 5) (+ (bit a 5) 1)))))))


(test test-expand-maths
  "Test we can expand the maths operators."
  (is (equal (vl::expand-macros-in-environment '(1+ 45))
	     '(+ 45 1)))
  (is (equal (vl::expand-macros-in-environment '(1- 45))
	     '(- 45 1)))
  (is (equal (vl::expand-macros-in-environment '(vl::2* 45))
	     '(vl::<< 45 1))))


(test test-expand-tests
  "Test we can expand the maths tests."
  (is (equal (vl::expand-macros-in-environment '(vl:0= 45))
	     '(= 45 0)))
  (is (equal (vl::expand-macros-in-environment '(vl:0/= 45))
	     '(/= 45 0))))


(test test-expand-let-representations
  "Test we can expand the representation-specific LET variants."
  (is (equal (vl::expand-macros-in-environment '(vl:let-wires ((a 0 :width 10))
				   (setq a 12)))
	     '(let ((a 0 :as :wire :width 10))
	       (progn
		 (setq a 12)))))
  (is (equal (vl::expand-macros-in-environment '(vl:let-registers ((a 0 :width 10))
				   (setq a 12)))
	     '(let ((a 0 :as :register :width 10))
	       (progn
		 (setq a 12)))))
  (is (equal (vl::expand-macros-in-environment '(vl:let-constants ((a 0 :width 10))
				   (setq a 12)))
	     '(let ((a 0 :as :constant :width 10))
	       (progn
		 (setq a 12)))))

  ;; test failure
  (signals (vl:representation-mismatch)
     (vl::expand-macros-in-environment '(vl:let-wires ((a 10 :as :constant))
				   (setq a 12)))))


(test test-no-macro-synthesis
  "Test we can't synthesise if there are macros left enexpanded."

  ;; should catch an unknown form if we typecheck without expanding
  (let ((p (copy-tree '(let ((a 1))
			(incf a)))))
    (signals (vl:unknown-form)
      (vl:typecheck p)))

  ;; should fail if we try to synthesise
  (let ((p (copy-tree '(when 1 (+ 1 2)))))
    (signals (vl:not-synthesisable)
      (vl:synthesise p))))
