;;;; Tests of evaluation of static values
;;
;;;; Copyright (C) 2024--2026 Simon Dobson
;;
;;;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;;;; verilisp is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;
;;;; verilisp is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/vl)


;;; ---------- Environment closure ----------

(test test-make-environment
  "Test we shadow variables correctly."
  (declare (optimize debug))

  (let ((env1 (vl::add-frame (vl::empty-environment))))
    (mapc (lambda (decl)
	    (vl::declare-environment-variable (car decl) (cadr decl) env1))
	  '((a ((initial-value 4)))
	    (b ((initial-value 23) (g 34)))
	    (c ((initial-value 12)))))

    (let ((env2 (vl::add-frame env1)))
      (mapc (lambda (decl)
	      (vl::declare-environment-variable (car decl) (cadr decl) env2))
	    '((a ((initial-value 5)))
	      (d ((initial-value 234) (g 76)))))

      ;;; a should be shadowed and appear only once
      (let ((decls (vl::make-environment-alist env2)))
	(is (equal (mapcar #'car decls)
		   '(b c a d)))
	(is (= (cadr (assoc 'a decls)) 5))))))


;;; ---------- Evaluation ----------

(test test-eval-literal
  "Test literals are static constants."
  (is (= (vl::ensure-static 12) 12)))


(test test-eval-parameter
  "Test that module parameters are static constants."
  (vl::with-new-frame
    (vl::declare-variable 'a '((width 5)
			       (initial-value 12)
			       (as parameter)))

    (is (= (vl::ensure-static 'a)
	   12))))


(test test-eval-constant
  "Test that variables declared as constants are static constants."
  (vl::with-new-frame
    (vl::declare-variable 'a '((width 5)
			       (initial-value 12)
			       (as constant)))

    (is (= (vl::ensure-static 'a)
	   12))))


(test test-eval-expression
  "Test that expressions involving only constants are static constants."
  (vl::with-new-frame
    (vl::declare-variable 'a '((width 5)
			       (initial-value 12)
			       (as constant)))

    (is (= (vl::ensure-static '(+ a (+ a 12)))
	   36))))


(test test-eval-non-constant-variable
  "Test that non-constant variables are rejected."
  (vl::with-new-frame
    (mapc (lambda (decl)
	    (destructuring-bind (n props)
		decl
	      (vl::declare-variable n props)))
	  '((a ((width 5)
		(initial-value 12)
		(as constant)))
	    (b ((width 5)
		(initial-value 12)
		(as register)))))

    (signals (vl::not-static)
      (vl::ensure-static '(+ a b 12)))))


(test test-eval-static
  "Test we get the result from a static expression, and nil from one that's not."
  (vl::with-new-frame
    (mapc (lambda (decl)
	    (destructuring-bind (n props)
		decl
	      (vl::declare-variable n props)))
	  '((a ((width 5)
		(initial-value 12)
		(as constant)))
	    (b ((width 5)
		(initial-value 12)
		(as register)))))

     ;; static
     (is (= (vl::eval-if-static '(+ a (+ a 12)))
	    36))

     ;; not static
     (is (null (vl::eval-if-static '(+ a b 12))))))


(test test-shadowed-variables
  "Test we declare only the shallowest declaration of each variable."
  (vl::with-new-frame
    (vl::declare-variable 'a '((width 5)
			       (initial-value 12)
			       (as constant)))
    (vl::declare-variable 'b '((width 5)
			       (initial-value 15)
			       (as constant)))

    (vl::with-new-frame
      (vl::declare-variable 'b '((width 5)
				 (initial-value 30)
				 (as constant)))

      (is (= (vl::eval-in-static-environment '(+ a b)) 42)))))


(test test-eval-let-constants
  "Test we reduce statically determined constants."
  (let ((p (vl::expand/vl '(let* ((a 10)
				  (b (+ a 12))
				  (c (+ a b)))
			    (declare (as constant a b)
			     (type (unsigned-byte 8) a b c))
			    (incf c)))))
    (vl::typecheck p)

    (let ((f (cadr p)))
      (is (equal (vl::get-frame-property 'a 'initial-value f) 10))
      (is (equal (vl::get-frame-property 'b 'initial-value f) 22))
      (is (equal (vl::get-frame-property 'c 'initial-value f) 32))
      (is (eql (vl::get-frame-property 'c 'as f) 'register)))))
