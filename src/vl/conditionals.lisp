;; Synthesisable conditionals
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

(in-package :verilisp/core)
(declaim (optimize debug))


;; ---------- if ----------

(defmethod compute-type-sexp ((fun (eql 'if)) args)
  (destructuring-bind (condition then &rest else)
      args
    (let ((tycond (compute-type condition))
	  (tythen (compute-type then))
	  (tyelse (if else
		      (compute-type (with-implicit-progn else)))))

      ;; the type of the expression is the widest of the
      ;; types of the two arms
      (if else
	  `(or ,tythen ,tyelse)
	  tythen))))


(defmethod apply-type-constraints-sexp ((fun (eql 'if)) args)
  (destructuring-bind (condition then &rest else)
      args
    (let ((tycond (compute-type condition)))
      (ensure-boolean tycond))))


(defun synthesise-if-expression (form)
  "Synthesise FORM as a continued expansion of conditions."
  (declare (optimize debug))
  (if (listp form)
      (destructuring-bind (fun &rest args)
	  form
	(if (eql fun 'if)
	    (destructuring-bind (condition then &rest else)
		args
	      (as-literal "(")
	      (synthesise-if-expression condition)
	      (as-literal " ? ")
	      (synthesise-if-expression then)
	      (as-literal " : ")
	      (synthesise-if-expression (car else))
	      (as-literal ")"))

	    (synthesise-sexp fun args)))

      (synthesise form)))


(defmethod synthesise-sexp ((fun (eql 'if)) args)
  (declare (optimize debug))

  (destructuring-bind (condition then &rest else)
      args

    (if (in-expression-context-p)
	;; in expression, synthesise as a conditional expression
	(progn
	  (as-literal "((")
	  (synthesise condition)
	  (as-literal ") ? (")
	  (synthesise  then)
	  (as-literal ") : (")
	  (synthesise (car else))
	  (as-literal "))"))

	;; elsewhere, synthesise as a conditional statement
	(progn
	  ;; condition
	  (as-literal "if(")
	  (synthesise condition)
	  (as-literal ")")
	  (as-newline)

	  ;; then arm
	  (as-block (list then) :before "begin" :after "end"
				:always t)

	  ;; else arm
	  (when else
	    (if (and (listp else)
		     (listp (car else))
		     (eql (caar else) 'if))
		;; else arm is another if, don't indent
		(progn
		  (as-literal "else ")
		  (as-block else :indent nil
				 :always t))

		;; otherwise indent
		(progn
		  (as-literal "else" :newline t)
		  (as-block else :before "begin" :after "end"
				 :always t))))))))


;; ---------- case ----------

(defun compute-type-clause (clause)
  "Typecheck case CLAUSE.

Return the type of the clause body."
  (destructuring-bind (val &rest body)
      clause

    (compute-type (with-implicit-progn body))))


(defun compute-type-clauses (clauses)
  "Typecheck case CLAUSES.

The type is the lub of the clause types."
  (foldr (lambda (tyl clause)
	   (lub tyl (compute-type-clause clause)))
	 clauses nil))


(defmethod compute-type-sexp ((fun (eql 'case)) args)
  (destructuring-bind (condition &rest clauses)
      args
    (let ((ty (compute-type condition)))
      (compute-type-clauses clauses))))


(defun constrain-clause (ty clause)
  "Apply type constraints to a CLAUSE of a case.

Each test element must be testable against TY."
  (destructuring-bind (val &rest body)
      clause

    (if (not (eql val 't))
	(if (listp val)
	    ;; multiple test elements, make sure they're all appropriate
	    (dolist (v val)
	      (let ((tyval (compute-type v)))
		(ensure-subtype tyval ty)))

	    ;; single test element
	    (let ((tyval (compute-type val)))
	      (ensure-subtype tyval ty))))))


(defmethod apply-type-constraints-sexp ((fun (eql 'case)) args)
  (destructuring-bind (condition &rest clauses)
      args
    (let ((ty (compute-type condition)))

      (mapc (curry #'constrain-clause ty) clauses))))


(defun synthesise-clause (clause)
  "Synthesise case CLAUSE"
  (destructuring-bind (val &rest body)
      clause
    (if (eql val 't)
	(as-literal "default")
	(synthesise val))
    (as-literal ":" :newline t)
    (as-block body :before "begin"
		   :after "end")))


(defun synthesise-nested-if (condition clauses)
  "Synthesise a nested IF corresponding to CLAUSES applied to testing CONDITION."
  (let* ((clause (car clauses))
	 (val (car clause))
	 (body (cdr clause)))

    ;; can only operate with single-clause bodies
    (when (> (length body) 1)
      (error 'not-synthesisable :hint "CASE statements in assignments must have simple (one-form) bodies"))

    (if (> (length clauses) 1)
	`(if (= ,condition ,val)
	     ,(car body)

	     ,(synthesise-nested-if condition (cdr clauses)))

	;; if we're the last clause
	(if (eql val 't)
	    ;; unconditional result
	    (car body)

	    ;; otherwise add 0 as the final result
	    `(if (= ,condition ,val)
		 ,(car body)
		 0)))))


(defun synthesise-case-arm (clause)
  "Synthesise a single arm of a case statement."
  (destructuring-bind (match &rest body)
      clause

    ;; matching values
    (if (listp match)
	(as-list match :sep ", ")

	(if (eql match t)
	    (as-literal "otherwise")
	    (synthesise match)))
    (as-literal ": ")
    (as-newline)

    ;; body
    (as-block body :before "begin" :after "end" :always t)))


(defun synthesise-case (condition clauses)
  "Synthesise a case directly."
  (as-literal "case (")
  (synthesise condition)
  (as-literal ")")
  (as-newline)
  (as-block clauses :process #'synthesise-case-arm)
  (as-literal "endcase"))


(defmethod synthesise-sexp ((fun (eql 'case)) args)
  (declare (optimize debug))
  (destructuring-bind (condition &rest clauses)
      args
    (if (in-expression-context-p)
	;; within an expression, expand as nested conditional expressions
	(synthesise (synthesise-nested-if condition clauses))

	;; otherwise synthesise as a Verilog case
	(synthesise-case condition clauses))))


;; ---------- cond ----------

(defcoremacro/vl cond (&rest arms)
  "Compile each case in ARMS to a nested conditional.

Each case should be a list consisting of a test and a body executed if
that tests passes. The body executed will be the first for which the
test passes, reckoned from the top of the arms. If no test passes, no
body is executed,

A test T always passes. An UNREACHABLE-CODE warning will be signalled
if there are arms after one with a T test, since these can never be
executed."
  (labels ((arms-to-ifs (arms)
	     (if (null arms)
		 nil

		 (destructuring-bind (test &rest body)
		     (car arms)
		   (if (eql test t)
		       (progn
			 ;; catch-all arm, test for unrerachable code
			 (unless (null (cdr arms))
			   (warn 'unreachable-code :fragment (cadr arms)
						   :hint "Make sure all meaningful options in a COND form appear before the test against T"))

			 ;; return the body as the last nested IF
			 (single-or-long-body body))

		       ;; arm with test, construct a nested IF
		       (let ((rest (arms-to-ifs (cdr arms))))
			 (if rest
			     ;; arm with following arms
			     `(if ,test
				  ,(single-or-long-body body)

				  ,rest)

			     ;; last arm
			     `(if ,test
				  ,(single-or-long-body body))))))))

	   (single-or-long-body (body)
	     (if (> (length body) 1)
		 ;; long body, put into a PROGN
		 `(progn
		    ,@body)

		 ;; singleton body, return it
		 (car body))))

    (arms-to-ifs arms)))
