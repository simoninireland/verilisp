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

(defmethod typecheck-sexp ((fun (eql 'setq)) args)
  (destructuring-bind (n v &key sync)
      args
    ;; catch the common mistake of using SETQ when we need SETF
    (unless (symbolp n)
      (error 'not-synthesisable :hint "Do you need SETF instead of SETQ?"))

    (let ((tyvar (typecheck n))
	  (tyval (typecheck v)))
      ;;(ensure-subtype tyval tyvar)
      (ensure-writeable n)
      ;;(add-type-constraint n tyval)
      tyval)))


(defmethod read-written-variables-sexp ((fun (eql 'setq)) args)
  (read-written-variables `(setf ,@args)))


(defmethod dependencies-sexp ((fun (eql 'setq)) args)
  (dependencies `(setf ,@args)))


(defmethod synthesise-sexp ((fun (eql 'setq)) args)
  (synthesise `(setf ,@args)))


;; ---------- Parallel SETQ ----------

(defmacro/vl psetq (&rest var-vals)
  "Update variables to values in parallel.

VAR-VALS is a list of alternating variables and values. In performing
the update, all the values are computed, and are only then assigned to
their respective variables. This ensures that all updates use the
same (old) values of the variables, making their ordering irrelevant.

Note that this creates temporary variables to hold the intermediate
updates. Also note that it only works for variables, not for
generalised places."
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


;; ---------- setf (generalised places) ----------

(defun ensure-generalised-place (form)
  "Ensure FORM is a generalised place."
  (unless (generalised-place-p form)
    (error 'not-synthesisable :hint "Make sure the target of the assignment is a generalised, SETF-able, place")))


(defmethod typecheck-sexp ((fun (eql 'setf)) args)
  (destructuring-bind (var val &key sync)
      args
    (if (listp var)
	(destructuring-bind (selector &rest selectorargs)
	    var
	  (typecheck-sexp-setf selector val selectorargs :sync sync))

	;; a SETF to a simple variable is a SETQ
	(typecheck `(setq ,var ,val :sync ,sync)))))


(defmethod typecheck-sexp-setf ((selector symbol) val selectorargs &key sync)
  (let* ((place `(,selector ,@selectorargs))
	 (tyvar (typecheck place))
	 (tyval (typecheck val)))
    (ensure-subtype tyval tyvar)
    (ensure-generalised-place place)

    tyvar))


(defmethod read-written-variables-sexp ((fun (eql 'setf)) args)
  (declare (optimize debug))

  (destructuring-bind (place v &key &allow-other-keys)
      args
    (let ((place-rws (if (symbolp place)
			 ;; place targets a variable directly
			 (list '() (list place))

			 ;; place is complex, recurse into it
			 (read-written-variables place)))
	  (v-rws (merge-all-variables-as-read (read-written-variables v))))
      (union2 place-rws v-rws))))


(defmethod dependencies-sexp ((fun (eql 'setf)) args)
  (declare (optimize debug))

  (with-recover-on-error
      ;; leave dependencies alone
      nil

    (destructuring-bind (place v &key &allow-other-keys)
	args

      (let* ((rws (if (symbolp place)
		      ;; shortcut for a symbol
		      (list () (list place))

		      (read-written-variables place)))
	     (fvs (union (remove-if #'static-constant-p (free-variables v))
			 (car rws))))

	;; set the dependencies for all written variables
	(mapc (lambda (n)
		(let ((depends-on (variable-property n 'depends-on :default nil)))
		  (set-variable-property n 'depends-on (union depends-on fvs))))
	      (cadr rws))))))


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
