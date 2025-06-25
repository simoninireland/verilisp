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

(in-package :vl)
(declaim (optimize debug))


;; ---------- if ----------

(defmethod typecheck-sexp ((fun (eql 'if)) args)
  (destructuring-bind (condition then &rest else)
      args
    (let ((tycond (typecheck condition))
	  (tythen (typecheck then))
	  (tyelse (if else
		      (typecheck `(progn ,@else)))))
      (ensure-boolean tycond)

      ;; the type of the expression is the widest of the
      ;; types of the two arms
      (if else
	  (lub tythen tyelse)
	  tythen))))

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

(defun typecheck-clause (clause ty)
  "Typecheck case CLAUSE.

The value of the clause should have a type compatible with TY.
Return the type of the clause body."
  (destructuring-bind (val &rest body)
      clause
    (if (not (eql val 't))
	(let ((tyval (typecheck val)))
	  (ensure-subtype tyval ty)))
    (typecheck (cons 'progn body))))


(defun typecheck-clauses (clauses ty)
  "Typecheck case CLAUSES.

The clauses' test values should be compatible with TY.
The type is the lub of the clause types."
  (foldr (lambda (tyl clause)
	   (lub tyl (typecheck-clause clause ty)))
	 clauses nil))


(defmethod typecheck-sexp ((fun (eql 'case)) args)
  (destructuring-bind (condition &rest clauses)
      args
    (let ((ty (typecheck condition)))
      (typecheck-clauses clauses ty))))


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


(defmethod synthesise-sexp ((fun (eql 'case)) args)
  (declare (optimize debug))
  (destructuring-bind (condition &rest clauses)
      args
    (if (in-expression-context-p)
	;; within an expression, expand as nested conditional expressions
	(synthesise (synthesise-nested-if condition clauses))

	;; elsewhere, synthesise as case
	(progn
	  (as-literal"case (")
	  (synthesise condition)
	  (as-literal ")" :newline t)

	  (as-block clauses :process #'synthesise-clause)

	  (as-literal "endcase" :newline t)))))
