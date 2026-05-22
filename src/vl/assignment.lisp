;;;; Assignments
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

;;; Assignments using SETQ and SETF. The complicated part of SETF is the
;;; use of generalised places, which are handled alongside those places,
;;; not here.


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
    (error 'access-mismatch :variable n
			    :hint "Ensure variable is writeable")))


;;; ---------- setq ----------

(defpassmethod compute-type (setq n v)
  (let ((ty (compute-type v)))
    ;; constraint the variable directly
    (add-type-constraint n ty)

    ty))


(defpassmethod apply-type-constraints (setq n v)
  (ensure-writeable n)
  (apply-type-constraints `(setf ,n ,v)))


(defpassmethod read-variables (setq &rest args)
  (:same-as setf))


(defpassmethod read-variables-setf (setq n v)
  (:same-as setf))


(defpassmethod compute-dependencies (setq n v)
  (declare (optimize debug))

  ;; catch the common mistake of using SETQ when we need SETF
  (when (listp n)
    (error 'not-synthesisable :hint "Did you mean SETF instead of SETQ?"))

  ;; catch assigning to a non-variable
  (unless (symbolp n)
    (error 'not-synthesisable :hint "Assignment target is not a variable"))

  (let ((read (read-variables v)))
    (add-dependencies n read)
    (set-variable-property n 'written t)))


(defpassmethod synthesise (setq &rest args)
  (:same-as setf))


;;; ---------- Parallel SETQ ----------

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


;;; ---------- setf (generalised places) ----------

(defpassmethod read-variables (setf place val)
  (declare (optimize debug))

  (if (listp place)
      (union (read-variables val)
	     (read-variables-setf place))

      ;; a SETF to a simple variable is a SETQ
      (read-variables val)))


(defpassmethod read-variables-setf (setf place val)
  (union (read-variables place
	 (read-variables val))))


(defpassmethod compute-dependencies (setf place val)
  (declare (optimize debug))

  (if (listp place)
      (let ((written (written-variables-setf place))
	    (read (union (read-variables-setf place)
			 (read-variables val))))
	(dolist (n written)
	  (add-dependencies n read)
	  (set-variable-property n 'written t)))

      ;; a SETF applied to a variable is just a SETQ
      (compute-dependencies `(setq ,place ,val))))


(defpassmethod compute-type (setf place val)
  (declare (optimize debug))

  (compute-type place)
  (break)
  (compute-type val))


(defpassmethod apply-type-constraints (setf place val)
  ;; ensure we can do the assignment
  (ensure-generalised-place place)

  ;; ensure the types match
  (let* ((tyvar (compute-type place))
	 (tyval (compute-type val)))
    (ensure-subtype tyval tyvar))

  (apply-type-constraints place)
  (apply-type-constraints val))


(defpassmethod synthesise (setf var val)
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
	(if (in-synchronous-block-context-p)
	    ;; use non-blocking assignment
	    (as-literal " <= ")

	    ;; use blocking assignment
	    (as-literal " = "))
	(synthesise val)))

  (as-literal ";"))
