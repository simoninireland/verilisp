;; Synthesisable control flow
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


;; ---------- PROGN ----------

(defmethod compute-type-sexp ((fun (eql 'progn)) args)
  (declare (optimize debug))

  (labels ((compute-type-forms (forms)
	     (declare (optimize debug))

	     (let ((ty (with-recover-on-error
			   t
			 (with-current-form (car forms)
			   (compute-type (car forms))))))

	       (if (null (cdr forms))
		   ;; if we're the last form, return the type
		   ty

		   ;; otherwise proceed to the next forms
		   (compute-type-forms (cdr forms))))))

    (if (= (length args) 0)
	t

	(compute-type-forms args))))


(defmethod apply-type-constraints-sexp ((fun (eql 'progn)) args)
  (dolist (form args)
    (with-recover-on-error
	;; ignore any errors
	t
      (apply-type-constraints form))))


(defun simplify-progn-body (body)
  "Simplify the body of a PROGN or implied PROGN block."
  (foldr (lambda (l arg)
	   (if (and (listp arg)
		    (eql (car arg) 'progn))
	       (append l (cdr arg))
	       (append l (list arg))))
	 body
	 '()))


(defmethod simplify-progn-sexp ((fun (eql 'progn)) args)
  (destructuring-bind (&rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      (with-implicit-progn (simplify-progn-body newbody)))))


(defmethod synthesise-sexp ((fun (eql 'progn)) args)
  (as-block args :indent nil))


;; ---------- Sensitive blocks ----------

(defun combinatorial-trigger-p (form)
  "Test whether FORM is a combinatorial trigger.

Combinatorial triggers are sensitive to all the wires in the
block, and are represented by the symbol *."
  (and (listp form)
       (= (length form) 1)
       (eql (car form) '*)))


(defmethod compute-type-sexp ((fun (eql '@)) args)
  (destructuring-bind (sensitivities &rest body)
      args
    ;; We accept single variables or lists of variables as senasitivity,
    ;; but there's an ambiguity over posedge and negedge operators, so
    ;; we explicity check the car of any list to see whether it's
    ;; "really" an atom
    ;;
    ;; These checks actually need to be slightly different, to make
    ;; sure we identify something with wires and not just a value.
    ;; That's not quite "compute-type-ing" in the sense we use it.
    (if (listp sensitivities)
	(cond ((combinatorial-trigger-p sensitivities)
	       ;; sensitive to everything
	       nil)

	      ((edge-trigger-p sensitivities)
	       ;; a single instance of a trigger operator
	       (compute-type sensitivities))

	      (t
	       ;; a list of sensitivities
	       (dolist (s sensitivities)
		 (compute-type s))))

	;; an atom
	(compute-type sensitivities))

    ;; check the body in the outer environment
    (compute-type (with-implicit-progn body))))


(defun read-variables-sensitivities (sensitivities)
  "Return the variables read in the SENSITIVITIES.

This includes all the named variables, and excluses the * wildcard."
  (let ((rws (if (listp sensitivities)
		     (cond ((combinatorial-trigger-p sensitivities)
			    ;; sensitive to everything
			    '())

			   ((edge-trigger-p sensitivities)
			    ;; a single instance of a trigger operator
			    (read-variables sensitivities))

			   (t
			    ;; a list of sensitivities
			    (foldr #'union (mapcar #'read-variables sensitivities) '())))

		     ;; an atom
		     (read-variables sensitivities))))

    ;; discard any wildcard
    (set-difference rws (list '*))))


(defmethod read-variables-sexp ((fun (eql '@)) args)
  (declare (optimize debug))

  (destructuring-bind (sensitivities &rest body)
      args

    (let ((s-rws (read-variables-sensitivities sensitivities))
	  (v-rws (read-variables (with-implicit-tagbody body))))
      (union s-rws v-rws))))


(defmethod compute-dependencies-sexp ((fun (eql '@)) args)
  (dolist (n (read-variables-sensitivities args))
    (set-variable-property n 'read t))

  (compute-dependencies (with-implicit-progn args)))


(defmethod simplify-progn-sexp ((fun (eql '@)) args)
  (destructuring-bind ((&rest sensitivities) &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(@ ,sensitivities ,@(simplify-progn-body newbody)))))


(defmethod synthesise-sexp ((fun (eql '@)) args)
  (declare (optimize debug))
  (destructuring-bind (sensitivities &rest body)
      args
    (if (combinatorial-trigger-p sensitivities)
	;; luteral expansion for combinatoreial blocks
	(as-literal "always @(*)")

	;; expand specified wires
	(as-list (if (listp sensitivities)
		     (if (edge-trigger-p sensitivities)
			 ;; an edge trigger, synthesise as an operator
			 (list sensitivities)

			 ;; a list of triggers, synthesise as a list
			 sensitivities)

		     ;; a single trigger, synthesise as a list
		     (list sensitivities))
		 :before "always @(" :after ")"))
    (as-newline)

    (as-block body :before "begin" :after "end" :always t)
    (as-blank-line)))


;; ---------- Triggers ----------

(defun edge-trigger-p (form)
  "Test whether FORM is an edge trigger expression."
  (and (listp form)
       (member (car form) '(posedge negedge))))


(defmethod compute-type-sexp ((fun (eql 'posedge)) args)
  'bit)


(defmethod read-variables-sexp ((fun (eql 'posedge)) args)
  (read-variables (car args)))


(defmethod compute-dependencies-sexp ((fun (eql 'posedge)) args)
  (let ((n (car args)))
    (set-variable-property n 'read t)))


(defmethod synthesise-sexp ((fun (eql 'posedge)) args)
  (destructuring-bind (pin)
      args
    (as-literal"posedge(")
    (synthesise pin)
    (as-literal ")")))


(defmethod compute-type-sexp ((fun (eql 'negedge)) args)
  (destructuring-bind (pin)
      args
    (set-variable-property pin 'read t)
    'bit))


(defmethod read-variables-sexp ((fun (eql 'negedge)) args)
  (read-variables (car args)))


(defmethod compute-dependencies-sexp ((fun (eql 'negedge)) args)
  (let ((n (car args)))
    (set-variable-property n 'read t)))


(defmethod synthesise-sexp ((fun (eql 'negedge)) args)
  (destructuring-bind (pin)
      args
    (as-literal "negedge(")
    (synthesise pin)
    (as-literal")")))
