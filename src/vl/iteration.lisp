;;;; Iteration
;;;;
;;;; Copyright (C) 2024--2025 Simon Dobson
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

;;;; Most macros to be re-implemented to ensure that they adhere to what
;;;; Verilisp needs, as the expansions done by built-in definitions are
;;;; not constrained and so tend to be implementation-dependent.

(in-package :verilisp/core)
(declaim (optimize debug))

;;; Verilisp has no iteration constructs built-in to the core: all
;;; iteration happens through state machines. We build iteration using
;;; macros, both the general DO form and some more structured and
;;; easily-readable forms for common patterns.
;;;
;;; The macros all end up in the core environment and so are always
;;; available.


;;; ---------- General DO loop iteration ----------

(defun generate-do-var (vds var)
  "Generate a declaration and stepper for VAR."
  (destructuring-bind (var-decls steppers)
      vds
    (if (listp var)
	;; full declaration
	(destructuring-bind (n &optional (init 0) step)
	    var

	  (list (append var-decls (list (list n init)))
		(if step
		    (append steppers (list n step))
		    steppers)))

	;; naked declaration, no stepper
	(list (append var-decls (list (list var 0)))
	      steppers))))


(defun generate-do-vars (vars)
  "Generate the varaible declarations and steppers for VARS."
  (foldr #'generate-do-var vars '(() ())))


(defcoremacro/vl do (vars test &body body)
  "Declare VARS for BODY.

TEST is a list consisting of a test for the end of
the loop and a form used to exit it.

BODY is implicitly a TAGBODY and so can contain tags and GO forms.
BODY is executed repeatedly as long as the end-test remains true, with
the increments to the variables being executed every time."
  (let ((end-test (car test))
	(end-body (cdr test)))

    (destructuring-bind (var-decls steppers)
	(generate-do-vars vars)

      (with-gensyms (loop-head loop-body loop-end)
	(let ((loop-body `(tagbody
			   ,loop-head
			     ;; run test to determine whether we exit
			     (if ,end-test
				 (progn
				   ;; test met, exit
				   ,@end-body
				   (go ,loop-end)))

			   ,loop-body
			     ;; run the loop body
			     ,@body

			     ;; run the stepper forms if there are any
			     ,(if steppers
				  `(psetq ,@steppers))

			     ;; return to head of the loop
			     (go ,loop-head)

			   ,loop-end)))

	  (if var-decls
	      ;; form introduces variables, declare them and declare
	      ;; them ignorable in the body
	      (let ((var-names (mapcar #'car var-decls)))
		`(let ,var-decls
		   (declare (ignorable ,@var-names)
			    (as register ,@var-names))

		   ,loop-body))

	      ;; no new variables
	      loop-body))))))


;;; ---------- Structured iteration ----------

(defcoremacro/vl while (condition &body body)
  "Run the BODY forms as long as CONDITION is true.

BODY is not run if CONDITION is already true."
  (with-gensyms (loop-head loop-end)
    `(tagbody
	,loop-head
	;; exit if condition isn't met
	(if (not ,condition)
	    (go ,loop-end))

	;; otherwise, execute the body and repeat
	,@body
	(go ,loop-head)

	,loop-end)))


(defcoremacro/vl until (condition &body body)
  "Run the BODY forms until CONDITION is true.

BODY is not run if CONDITION is already true."
  `(while (not ,condition)
     ,@body))


(defcoremacro/vl forever (&body body)
  "Run BODY forever."
  (with-gensyms (forever)
    `(tagbody
      ,forever
	,@body

	(go ,forever))))


(defcoremacro/vl dotimes ((var count) &body body)
  (with-gensyms (counter)
    `(let ((,counter ,count))
       (declare (as register ,counter))

       (do ((,var 0 (1+ ,var)))
	   ((>= ,var ,counter))

	 ;; optimisation to set the width of var if the bound
	 ;; is a constant -- otherwise may need to be done manually
	 ,(if (constant-p count)
	      (let* ((v (eval-in-static-environment count))
		     (bv (bits-for-integer v)))
		`(declare (type (unsigned-byte ,bv) ,var))))

	 ,@body))))
