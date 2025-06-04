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

(in-package :vl)
(declaim (optimize debug))


;; ---------- PROGN ----------

(defmethod typecheck-sexp ((fun (eql 'progn)) args)
  (labels ((typecheck-forms (forms)
	     (let ((ty (with-recover-on-error
			   t
			 (typecheck (car forms)))))

	       (if (null (cdr forms))
		   ;; if we're the last form, return the type
		   ty

		   ;; otherwise proceed to the next forms
		   (typecheck-forms (cdr forms))))))

    (when (= (length args) 0)
      (error 'not-synthesisable :hint "Make sure body is not empty"))
    (typecheck-forms args)))


(defmethod dependencies-sexp ((fun (eql 'progn)) args)
  (flet ((cascade-changes (changed form)
	   (let ((chs (dependencies form)))
	     (mapc (lambda (n)
		     (let ((fvs (variable-property n :dependencies :default nil)))
		       (if (not (null (intersection chs fvs)))
			   ;; a variable n depends on has changed dependencies
			   (let ((deps (traverse-dependencies fvs)))
			     (set-variable-property n :dependencies deps)))))
		   changed)

	     (union changed chs))))

    (foldr #'cascade-changes args '())))

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
      `(progn ,@(simplify-progn-body newbody)))))


(defmethod synthesise-sexp ((fun (eql 'progn)) args)
  (as-block args :indent nil))


;; ---------- Triggered blocks ----------

(defun combinatorial-trigger-p (form)
  "Test whether FORM is a combinatorial trigger.

Combinatorial triggers are sensitive to all the wires in the
block, and are represented by the symbol *."
  (and (listp form)
       (= (length form) 1)
       (eql (car form) '*)))


(defmethod typecheck-sexp ((fun (eql '@)) args)
  (destructuring-bind (sensitivities &rest body)
      args
    ;; We accept single variables or lists of variables as senasitivity,
    ;; but there's an ambiguity over posedge and negedge operators, so
    ;; we explicity check the car of any list to see whether it's
    ;; "really" an atom
    ;;
    ;; These checks actually need to be slightly different, to make
    ;; sure we identify something with wires and not just a value.
    ;; That's not quite "typechecking" in the sense we use it.
    (if (listp sensitivities)
	(cond ((combinatorial-trigger-p sensitivities)
	       ;; sensitive to everything
	       nil)

	      ((edge-trigger-p sensitivities)
	       ;; a single instance of a trigger operator
	       (typecheck sensitivities))

	      (t
	       ;; a list of sensitivities
	       (dolist (s sensitivities)
		 (typecheck s))))

	;; an atom
	(typecheck sensitivities))

    ;; check the body in the outer environment
    (typecheck `(progn ,@body))))


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


(defmethod typecheck-sexp ((fun (eql 'posedge)) args)
  (destructuring-bind (pin)
      args
    (let ((ty (typecheck pin)))
      (ensure-subtype ty '(unsigned-byte 1))
      ty)))


(defmethod synthesise-sexp ((fun (eql 'posedge)) args)
  (destructuring-bind (pin)
      args
    (as-literal"posedge(")
    (synthesise pin)
    (as-literal ")")))


(defmethod typecheck-sexp ((fun (eql 'negedge)) args)
  (destructuring-bind (pin)
      args
    (let ((ty (typecheck pin)))
      (ensure-subtype ty '(unsigned-byte 1))
      ty)))


(defmethod synthesise-sexp ((fun (eql 'negedge)) args)
  (destructuring-bind (pin)
      args
    (as-literal "negedge(")
    (synthesise pin)
    (as-literal")")))
