;; Assignments
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


(defun writeable-p (n)
  "Test whether N is writeable.

To be writeable a variable must be a register or wire, not a constant,
and not an input argument."
  (and (variable-declared-p n)
       (not (eql (get-representation n) 'constant))
       (not (eql (get-direction n) 'in))))


(defun ensure-writeable (n)
  "Ensure N is writeable.

Signals NOT-SYNTHESISABLE if an attempt is made to update a
constant or an input parameter, or UNKNOWN-VARIABLE if the variable
isn't declared."
  (unless (variable-declared-p n)
    (error 'unknown-variable :variable n
			     :hint "Make sure the variable is in scope"))
  (unless (writeable-p n)
    (error 'not-synthesisable :variable n
			      :hint "Ensure target is writeable")))


;; ---------- setq ----------

(defmethod compute-type-sexp ((fun (eql 'setq)) args)
  (destructuring-bind (n v &key sync)
      args

    (let ((ty (compute-type v)))
      ;; constraint the variable directly
      (add-type-constraint n ty)

      ty)))


(defmethod apply-type-constraints-sexp ((fun (eql 'setq)) args)
  (destructuring-bind (n v &key sync)
      args
    (ensure-writeable n)
    (apply-type-constraints `(setf ,n ,v :sync ,sync))))


(defmethod read-variables-sexp ((fun (eql 'setq)) args)
  (read-variables `(setf ,@args)))


(defmethod read-variables-setf ((selector symbol) val selectorargs)
  (union (list selector)
	 (read-variables val)))


(defmethod compute-dependencies-sexp ((fun (eql 'setq)) args)
  (destructuring-bind (n v &key sync)
      args

    ;; catch the common mistake of using SETQ when we need SETF
    (when (listp n)
      (error 'not-synthesisable :hint "Do you need SETF instead of SETQ?"))

    ;; catch assigning to a non-variable
    (unless (symbolp n)
      (error 'not-synthesisable :hint "Assignment target is not a variable"))

    (let ((read (read-variables v)))
      (add-dependencies n read)
      (set-variable-property n 'written t))))


(defmethod synthesise-sexp ((fun (eql 'setq)) args)
  (synthesise `(setf ,@args)))


;; ---------- Parallel SETQ ----------

(defcoremacro/vl psetq (&rest var-vals)
  "Update variables to values in parallel.

VAR-VALS is a list of alternating variables and values. In performing
the update, all the values are computed, and are only then assigned to
their respective variables. This ensures that all updates use the
same (old) values of the variables, making their ordering irrelevant.

Note that this creates temporary variables to hold the intermediate
updates. Also note that it only works for variables, not for
generalised places."
  (let* ((var-val-pairs (adjacent-pairs var-vals))
	 (vars (mapcar #'car var-val-pairs))
	 (vals (mapcar #'cadr var-val-pairs)))

    (if (= (length vars) 1)
	;; single assignment, replace with SETQ
	`(setq ,(car vars) ,(car vals))

	;; multiple assignments, expand
	(progn
	  ;; warn about the temporary variables
	  (warn 'resources-created
		:description (format nil "PSETQ form created ~a new variables"
				     (length vars)))

	  (let ((tempvars (mapcar #'gensym (mapcar #'symbol-name vars))))
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
			      vars tempvars)))))))))


;; ---------- setf (generalised places) ----------

(defun ensure-generalised-place (form)
  "Ensure FORM is a generalised place."
  (unless (generalised-place-p form)
    (error 'not-synthesisable :hint "Make sure the target of the assignment is a generalised, SETF-able, place")))


(defmethod read-variables-sexp ((fun (eql 'setf)) args)
  (declare (optimize debug))
  (destructuring-bind (place val &key sync)
      args
    (if (listp place)
	(destructuring-bind (selector &rest selectorargs)
	    place

	  (union (read-variables val)
		 (read-variables-setf selector val selectorargs)))

	;; a SETF to a simple variable is a SETQ
	(read-variables val))))


(defmethod compute-dependencies-sexp ((fun (eql 'setf)) args)
  (declare (optimize debug))

  (destructuring-bind (place val &key sync)
      args

    (if (listp place)
	(destructuring-bind (selector &rest selectorargs)
	    place

	  (let ((written (written-variables-setf selector val selectorargs))
		(read (read-variables-setf selector val selectorargs)))

	    (dolist (n written)
	      (add-dependencies n read)
	      (set-variable-property n 'written t))))

	;; a SETF applied to a variable is just a SETQ
	(compute-dependencies `(setq ,place ,val :sync ,sync)))))


(defmethod compute-type-sexp ((fun (eql 'setf)) args)
  (destructuring-bind (place val &key sync)
      args
    (compute-type place)
    (compute-type val)))


(defmethod apply-type-constraints-sexp ((fun (eql 'setf)) args)
  (destructuring-bind (place val &key sync)
      args

    ;; ensure we can do the assignment
    (ensure-generalised-place place)

    ;; ensure the types match
    (let* ((tyvar (compute-type place))
	   (tyval (compute-type val)))
      (ensure-subtype tyval tyvar))

    (apply-type-constraints place)
    (apply-type-constraints val)))


(defmethod synthesise-sexp ((fun (eql 'setf)) args)
  (destructuring-bind (var val &key (sync nil))
      args
    (if (in-module-context-p)
	;; outermost in a module
	(progn
	  (as-literal "assign ")
	  (synthesise var)
	  (as-literal " = ")
	  (synthesise val))

	;; elsewhere (in a block)
	(progn
	  (synthesise var)
	  (if sync
	      (as-literal " = ")
	      (as-literal " <= "))
	  (synthesise val)))

    (as-literal ";")))
