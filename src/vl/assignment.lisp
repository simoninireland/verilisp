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

(in-package :vl)
(declaim (optimize debug))


(defun writeable-p (n)
  "Test whether N is writeable.

To be writeable a variable must be a register or wire, not a constant,
and not an input argument."
  (and (variable-declared-p n)
       (not (get-constant n))
       (not (eql (get-representation n) :constant))
       (not (eql (get-direction n) :in))))


(defun ensure-writeable (n)
  "Ensure N is writeable.

Signals NOT-SYNTHESISABLE if an attempt is made to update a
constant or an input parameter, or UNKNOWN-VARIABLE if the variable
isn't declared."
  (unless (variable-declared-p n)
    (error 'unknown-variable :variable n
			     :hint "Make sure the variable is in scope"))
  (unless (writeable-p n)
    (error 'not-synthesisable :hint "Ensure target is writeable")))


;; ---------- setq ----------

(defmethod typecheck-sexp ((fun (eql 'setq)) args)
  (destructuring-bind (n v &key sync)
      args
    ;; catch the common mistake of using SETQ when we need SETF
    (unless (symbolp n)
      (error 'not-synthesisable :hint "Do you need SETF instead of SETQ?"))

    (let ((tyvar (typecheck n))
	  (tyval (typecheck v)))
      (ensure-subtype tyval tyvar)
      (ensure-writeable n)
      (add-type-constraint n tyval)

      tyval)))


(defmethod dependencies-sexp ((fun (eql 'setq)) args)
  (dependencies `(setf ,@args)))


(defmethod synthesise-sexp ((fun (eql 'setq)) args)
  (synthesise `(setf ,@args)))


;; ---------- setf (generalised places) ----------

(defgeneric generalised-place-p (form)
  (:documentation "Test whether FORM is a generalised place.

Generalised places can appear as the target of SETF forms. (In other
languages they are sometimes referred to as /lvalues/.) This is
separate, but related to, their type: a generalised place has a type,
but is also SETF-able.

By default forms are /not/ generalised places.")
  (:method ((form integer))
    nil)
  (:method ((form symbol))
    (not (get-constant form)))
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (generalised-place-sexp-p fun args))))


;; No need to dive into the args in most cases, just the selector

;; There might be a better approach to this using reference types
;; to differentiate between l- and r-value instances

(defgeneric generalised-place-sexp-p (fun args)
  (:documentation "Test whether FUN applied to ARGS identifies a generalised place.

The default is for a form /not/ to be a generalised place.")
  (:method (fun args)
    nil))


(defun ensure-generalised-place (form)
  "Ensure FORM is a generalised place."
  (unless (generalised-place-p form)
    (error 'not-synthesisable :hint "Make sure the code identifies a generalised, SETF-able, place")))


(defmethod typecheck-sexp ((fun (eql 'setf)) args)
  (destructuring-bind (var val &key sync)
      args
    (if (listp var)
	(typecheck-sexp-setf (car var) val (cdr var) :sync sync)

	;; a SETF to a simple variable is a SETQ
	(typecheck `(setq ,var ,val :sync ,sync)))))


(defmethod typecheck-sexp-setf ((selector symbol) val selectorargs &key sync)
  (let* ((place `(,selector ,@selectorargs))
	 (tyvar (typecheck place))
	 (tyval (typecheck val)))
    (ensure-subtype tyval tyvar)
    (ensure-generalised-place place)

    tyvar))


(defmethod dependencies-sexp ((fun (eql 'setf)) args)
  (declare (optimize debug))

  (destructuring-bind (n v &key &allow-other-keys)
      args

    (let* ((fvs (free-variables v))
	   (all-fvs (traverse-dependencies fvs))
	   (deps (variable-property n :dependencies :default nil)))
      (set-variable-property n :dependencies (union deps all-fvs))

      (list n))))


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
