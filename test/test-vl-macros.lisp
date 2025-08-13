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
		       0))))))


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
	       (setf a (+ a 1)))))

  (is (equal (vl::expand-macros-in-environment '(let ((a 1))
						 (incf (bit a 5))))
	     '(let ((a 1))
	       (setf (bit a 5) (+ (bit a 5) 1))))))


(test test-expand-decf
  "Test we canm expand DECF macros."
  (is (equal (vl::expand-macros-in-environment '(let ((a 1))
						 (decf a)))
	     '(let ((a 1))
	       (setf a (- a 1)))))

  (is (equal (vl::expand-macros-in-environment '(let ((a 1))
						 (decf (bit a 5))))
	     '(let ((a 1))
	       (setf (bit a 5) (- (bit a 5) 1))))))


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
  (is (equal (vl::expand-macros-in-environment '(vl::0= 45))
	     '(= 45 0)))
  (is (equal (vl::expand-macros-in-environment '(vl::0/= 45))
	     '(/= 45 0))))


(test test-no-macro-synthesis
  "Test we can't synthesise if there are macros left unexpanded."

  ;; should catch an unknown form if we typecheck without expanding
  (let ((p (vl::add-frames (copy-tree '(let ((a 1))
					(incf a))))))
    (signals (vl::unknown-form)
      (vl::typecheck p)))

  ;; should fail if we try to synthesise
  (let ((p '(when 1 (+ 1 2))))
    (signals (vl::unknown-form)
      (vl::synthesise p))))


(test test-local-macro
  "Test we can declare local macros using MACROLET/VL."
  (vl::clear-global-environment)

  (vl::defmacro/vl test-locals (z &body body)
    (vl::macrolet/vl ((l1 (a)
			  `(+ ,a ,z))
		      (l2 (a &rest rs)
			  `(+ ,a ,@rs)))

      `(let (a b c)
	 ,@body)))

  ;; make sure all macros get expanded
  (let ((p (vl::expand/vl '(let (q w e)
			    (test-locals 23
			     (l1 q)
			     (l2 w e w))))))

    (let ((atoms (flatten p)))
      (is (not (member 'test-locals atoms)))
      (is (not (member 'l1 atoms)))
      (is (not (member 'l2 atoms)))))

  ;; make sure we can't use L1 or L2 as macros elsewhere
  ;; (they stay un-expanded out of context)
  (let ((p (vl::expand/vl '(let (q w e)
			    (l1 q)
			    (l2 e w e)))))

    (let ((atoms (flatten p)))
      (is (member 'l1 atoms))
      (is (member 'l2 atoms)))))


(test test-nested-macros
  "Test we can nest the same macro."
  (is (vl::expand/vl '(let (a b c) (when a (when b c))))))


(test test-nested-local-macro
  "Test we get the right versions of nested local macros."
  (vl::clear-global-environment)

  (vl::defmacro/vl test-locals (b &body body)
    (vl::macrolet/vl ((l1 (a)
			  `(+ ,a ,b)))

      `(progn
	 ,@body)))

  (let ((p (expand/vl '(let (x y z)
			(test-locals 12
			 (setq x 27)
			 (l1 y)
			 (test-locals 13
			  (setq x 25)
			  (l1 z))
			 (setq x (l1 z)))))))

    (is (contains-form-p '(+ y 12) p))
    (is (contains-form-p '(+ z 13) p))
    (is (contains-form-p '(+ z 12) p))))


(test test-do-no-variables
  "Test we can expand a DO that doesn't declare any loop variables."
  (let ((p (vl::expand/vl '(module test-do (clk)
			    (declare (type bit clk))
			    (@ (posedge clk)
			     (tagbody
			      ttt
				(setf clk 0)
				(go ttt)))))))
    (vl::typecheck p)
    (let ((q (cadr (vl::elaborate/vl p))))
      (is (vl::synthesise q)))))
