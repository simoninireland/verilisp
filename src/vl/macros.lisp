;; (Re-)implementations of some macros
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

;; We re-implement some macros to avoid possible issues with the standard
;; versions generating non-synthesisable Lisp. The names of these macros
;; are suffixed /vl, and we alias them using ADD-MACRO in the loader.

(in-package :verilisp/core)
(declaim (optimize debug))


;; Most macros to be re-implemented to ensure that they adhere to what
;; Verilisp needs, as the expansions done by built-in definitions are
;; not constrained and so tend to be implementation-dependent.


;; ---------- Macro environment  ----------

(defvar *macro-environment* (empty-environment)
  "The frame containing all the macros available in Verilisp.

The frame is initially detatched. It should be attached to the global
environment before calling EXPAND-MACROS-IN-ENVIRONMENT (and can be
detached again afterwards).")


;; ---------- Macro definition macros ----------

(eval-when (:compile-toplevel :load-toplevel)

  (defmacro defmacro/vl (name args &body body)
    "Declare NAME with ARGS as a Verilisp macro."
    (with-gensyms (external-name)
      `(progn
	 (defmacro ,external-name ,args
	   ,@body)

	 (with-frame *macro-environment*
	   (declare-macro ',name ',external-name)))))


  (defmacro import-macro/vl (name)
    "Import Lisp macro NAME into Verilisp."
    `(with-frame *macro-environment*
       (declare-macro ',name))))


;; ---------- Imported macros ----------

(import-macro/vl cond)


;; ---------- Single-armed conditionals ----------

(defmacro/vl when (cond &body body)
  "Execute BODY when COND is true."
  `(if ,cond
       (progn
	 ,@body)))


(defmacro/vl unless (cond &body body)
  "Execute BODY unless COND is true."
  `(if (not,cond)
       (progn
	 ,@body)))


;; ---------- Increment and decrement ----------

;; These work because places in Verilisp can't be side-effecting.

(defmacro/vl incf (place &optional (value 1))
  "Increment PLACE by VALUE, which defaults to 1."
  `(setf ,place (+ ,place ,value)))


(defmacro/vl decf (place &optional (value 1))
  "Decrement PLACE by VALUE, which defaults to 1."
  `(setf ,place (- ,place ,value)))


;; ---------- Quick common tests ----------

(defmacro/vl 0= (arg)
  "Test whether ARG is equal to zero."
  `(= ,arg 0))


(defmacro/vl 0/= (arg)
  "Test whether ARG is not equal to zero."
  `(/= ,arg 0))


;; ---------- Quick common maths operations ----------

(defmacro/vl 1+ (arg)
  "Return ARG plus one."
  `(+ ,arg 1))


(defmacro/vl 1- (arg)
  "Return ARG minus one."
  `(- ,arg 1))


(defmacro/vl 2* (arg)
  "Return ARG times two."
  `(<< ,arg 1))


(defmacro/vl 2/ (arg)
  "Return ARG divided by two."
  `(>> ,arg 1))


;; ---------- Parallel SETQ ----------

(defmacro/vl psetq (&rest var-vals)
  "Update variables to values in parallel.

ALl the values of VAR-VALS are computed, and are only then
assigned to their respective variables. This ensures that all
updates use the old values of the variables, making their
ordeing irrelevant.

Note that this creates temporary variables to hold the
intermediate updates, one per update."
  (declare (optimize debug))

  (let* ((var-val-pairs (adjacent-pairs var-vals))
	 (vars (mapcar #'car var-val-pairs))
	 (vals (mapcar #'cadr var-val-pairs))
	 (tempvars (mapcar #'gensym (mapcar #'symbol-name vars))))

    (with-gensyms (temps reals)
      `(let ,tempvars
	 (tagbody
	  ,temps
	    ,@(mapcar (lambda (tempvar val)
			`(setq ,tempvar ,val))
		      tempvars vals)

	  ,reals
	    ,@(mapcar (lambda (var tempvar)
			`(setq ,var ,tempvar))
		      vars tempvars))))))


;; ---------- General iteration ----------

(defun generate-do-var (vds var)
  "Generate a declaration and stepper for VAR."
  (destructuring-bind (var-decls steppers)
      vds
    (if (listp var)
	;; full declaration
	(destructuring-bind (n &optional init step)
	    var

	  (list (append var-decls (list (list n init)))
		(if step
		    (append steppers (list n step))
		    steppers)))

	;; naked declaration, no stepper
	(list (append var-decls (list (list var 0)))
	      steppers))))


(defun generate-do-vars (vars)
  "Generate the varable declarations and steppers for VARS."
  (foldr #'generate-do-var vars '(() ())))


(defmacro/vl do (vars test &body body)
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

      (with-gensyms (loop-head loop-body)
	`(let ,var-decls

	   (tagbody
	    ,loop-head
	      ;; run test to determine whether we exit
	      (if ,end-test
		  (progn
		    ;; test met, exit
		    ,@end-body))

	    ,loop-body
	      ;; run the loop body
	      ,@body

	      ;; run the stepper forms
	      (psetq ,@steppers)

	      ;; return to head of the loop
	      (go ,loop-head)))))))


;; ---------- Structured iteration ----------

(defmacro/vl while (condition &body body)
  "Run the BODY forms as long as CONDITION is true.

BODY is not run if CONDITION is already true."
  `(do ()
       (,condition (return))
     ,@body))


(defmacro/vl until (condition &body body)
  "Run the BODY forms until CONDITION is true.

BODY is not run if CONDITION is already true."
  `(do ()
       ((not ,condition) (return))
     ,@body))
