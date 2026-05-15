;;;; Synthesisable control flow
;;;;
;;;; Copyright (C) 2024--2026 Simon Dobson
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

(in-package :verilisp/core)
(declaim (optimize debug))

;;; PROGN is the main sequencing construct, coupled with @ blocks
;;; (that do not appear in Common Lisp, of course) ans the
;;; corresponding POSEDGE and NEGEDGE operators

;;; TODO: We should make sure the edge triggers only appear in
;;; sensitivity lists.


;;; ---------- PROGN ----------

(defpassmethod compute-type (progn &rest body)
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

    (if (= (length body) 0)
	t

	(compute-type-forms body))))


(defpassmethod apply-type-constraints (progn &rest body)
  (dolist (form body)
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


(defpassmethod simplify-progn (progn &rest body)
  (let ((newbody (mapcar #'simplify-progn body)))
    (with-implicit-progn (simplify-progn-body newbody))))


(defpassmethod synthesise (progn &rest body)
  (as-block body :indent nil))


;;; ---------- Sensitive blocks ----------

(defun combinatorial-trigger-p (form)
  "Test whether FORM is a combinatorial trigger.

Combinatorial triggers are sensitive to all the wires in the
block, and are represented by the symbol *."
  (and (listp form)
       (= (length form) 1)
       (eql (car form) '*)))


(defpassmethod compute-type (@ sensitivities &rest body)
  ;; We accept single variables or lists of variables as sensitivity,
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
  (compute-type (with-implicit-progn body)))


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
			(union-all (mapcar #'read-variables sensitivities))))

		 ;; an atom
		 (read-variables sensitivities))))

    ;; discard any wildcard
    (set-difference rws (list '*))))


(defpassmethod read-variables (@ sensitivities &rest body)
  (declare (optimize debug))

  (let ((s-rws (read-variables-sensitivities sensitivities))
	(v-rws (read-variables (with-implicit-tagbody body))))
    (union s-rws v-rws)))


(defpassmethod compute-dependencies (@ sensitivities &rest body)
  (dolist (n (read-variables-sensitivities sensitivities))
    (set-variable-property n 'read t))

  (compute-dependencies (with-implicit-progn body)))


(defpassmethod simplify-progn (@ sensitivities &rest body)
  (let ((newbody (mapcar #'simplify-progn body)))
    `(@ ,sensitivities ,@(simplify-progn-body newbody))))


(defpassmethod synthesise (@ sensitivities &rest body)
  (declare (optimize debug))
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
  (as-blank-line))


;;; ---------- Triggers ----------

(defun edge-trigger-p (form)
  "Test whether FORM is an edge trigger expression."
  (and (listp form)
       (member (car form) '(posedge negedge))))


(defpassmethod compute-type (posedge v)
  'bit)


(defpassmethod read-variables (posedge v)
  (read-variables v))


(defpassmethod compute-dependencies (posedge n)
  (set-variable-property n 'read t))


(defpassmethod synthesise (posedge v)
  (as-literal"posedge(")
  (synthesise v)
  (as-literal ")"))


(defpassmethod compute-type (negedge v)
  (:same-as posedge))


(defpassmethod read-variables (negedge v)
  (:same-as posedge))


(defpassmethod compute-dependencies (negedge n)
  (:same-as posedge))


(defpassmethod synthesise (negedge v)
  (as-literal"negedge(")
  (synthesise v)
  (as-literal ")"))
