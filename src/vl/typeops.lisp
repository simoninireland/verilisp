;;;; Type operations
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

;;; These operations declare the type algebra for Verilisp. The operations
;;; all evaluate the parameters to types in a static environment, ensure
;;; that all types are determined at compile-time.
;;;
;;; To avoid getting caught up in recursions we use adopt the convention
;;; of always defining LUB with the more complex type on the left.


;;; ---------- Type algebra ----------

;;; Union types

(defsubtype ((or &rest tys) (&type ty))
  ;; A or B < C if A < C and B < C
  (every (rcurry #'subtype-p ty) tys))

(defsubtype ((&type ty) (or &rest tys))
  ;; C < A or B  if C < A or C < B
  (some (curry #'subtype-p ty) tys))

(deflub ((or &rest tys) (&type ty))
  (lub (foldr #'lub (cdr tys) (car tys)) ty))
(deflub ((&type ty) (or &rest tys))
  (lub `(or ,@tys) ty))


;;; There are no general intersection types (yet), but
;;; there are some defined for fixed-width types.

(deflub ((&type ty) (and &rest tys))
  (lub `(and ,@tys) ty))


;;; type-of types

;;; Types containing references to the types of other variables
;;; mean that we may need to solve type constraints in the course
;;; of funding LUBs. Since there's a danger of circularity, we
;;; define a queue of variables having their types solved and
;;; escape if we encounter a cycle. (This is possibly too strong,
;;; but I don't think there are fixpoints to be found in the
;;; Verilisp type system.)

(defvar *variables-being-constrained* nil
  "List of variables names a frames that are having their types solved..

This is used only in computing LUBs involving TYPE-OF types.")


(defsubtype ((type-of a f) (&type ty))
  (if-let ((ty (get-frame-property a 'type f :default nil)))
    ;; either we've constrained the type fully or it's been declared
    ;; explicitly (and won't be inferred to be wider)
    ty

    ;; we don't know the full type of a variable until it's been fully
    ;; constrained, but we can work out an upper bound and leave it to
    ;; be further refined later
    ;; TODO: Not sure this is the right approach
    (if-let ((constraints (get-frame-property a 'type-constraints f :default nil)))
      (subtype-p (solve-type-constraints a f constraints) ty)

      ;; otherwise we have to stay unconstrained
      t)))
(defsubtype ((&type ty) (type-of a f))
  (subtype-p `(type-of ,a ,f) ty))


(deflub ((type-of a f) (&type ty2))
  (if-let ((ty1 (get-frame-property a 'type f :default nil)))
    ;; type has been declared or solved
    (lub ty1 ty2)

    ;; type hasn't been solved, solve it
    (if (assoc a *variables-being-constrained*)
	;; circular dependencies
	(let ((constraints (get-frame-property a 'type-constraints f :default nil)))
	  (error 'circular-type-dependencies :variable a
					     :involved constraints
					     :hint "Declare types for one or more of the variables involved"))

	;; solve the constraints
	(let ((*variables-being-constrained* (cons (list a f)
						   *variables-being-constrained*)))
	  (lub (solve-type-constraints-and-declare a f) ty2)))))

(deflub ((&type ty1) (type-of a f))
  (lub `(type-of ,a ,f) ty1))


;;; ---------- Type constraints ----------

(defun add-frame-type-constraint (n ty env)
  "Constrain variable N to have type TY in ENV.

This constraint will be used when inferring the finla type of N."
  (let ((constraints (get-frame-property n 'type-constraints env)))
    (set-frame-property n 'type-constraints (cons ty constraints) env)))


(defun add-type-constraint (n ty)
  "Constrain N to have type TY.

This constraint will be used when inferring the finla type of N."
  (let ((constraints (variable-property n 'type-constraints)))
    (set-variable-property n 'type-constraints (cons ty constraints))))


(defun get-type-constraints (n)
  "Return the type constraints that apply to N."
  (variable-property n 'type-constraints))


;;; ---------- Type constraint solving ----------

;;; Verilisp uses a constraint-based type-checker. These functions solve
;;; a set of type constraints to determine the smallest type that can
;;; represent the values required.

(defun solve-type-constraints (n f constraints)
  "Determine the smallest representable type under CONSTRAINTS.

Return the inferred type."
  (declare (optimize debug))

  (let ((*variables-being-constrained* (cons (list n f)
					     *variables-being-constrained*)))
    (let ((cty (if (= (length constraints) 1)
		   (lub (car constraints)) ; simplify
		   (foldr #'lub (cdr constraints) (car constraints)))))

      cty)))


(defun solve-type-constraints-and-declare (n f)
  "Determine the smallest representable type for N in frame F.

The type inferred is used to declare N's type in F. If N already
has a declared type nit is retained, but a warning is signalled if
the inferred type is not compatible with the declared type.

It is safe to call this method repeatedly for the same variable, as
once it's been called once (and has resolved the constraints) they are
deleted to prevent re-solving."
  (declare (optimize debug))

  (if-let ((ty (get-frame-property n 'type f :default nil)))
    ;; variable has a declared type
    (if-let ((constraints (get-frame-property n 'type-constraints f :default nil)))
      ;; variable has constraints, solve them
      (let ((cty (solve-type-constraints n f constraints)))
	;; check the declared and inferred types are compatible
	(unless (subtype-p cty ty)
	    (warn 'type-mismatch :expected ty
				 :got cty
				 :hint "Check the declared type is appropriate"))

	;; remove the constraints so we don't re-solve them
	(set-frame-property n 'type-constraints nil f)

	;; return the declared type
	ty)

      ;; variable has no constraints
      ty)

    ;; variable doesn't have a declared type
    (if-let ((constraints (get-frame-property n 'type-constraints f :default nil)))
      ;; variable has constraints, solve them
      (let ((cty (solve-type-constraints n f constraints)))
	;; remove the constraints so we don't re-solve them
	(set-frame-property n 'type-constraints nil f)

	;; declare the type
	(set-frame-property n 'type cty f)
	cty)

      ;; variable has no constraints or type, probably an unused variable
      (progn
	(warn 'unused-variable :variable n :hint "Can't infer a type, and none declared")
	nil))))


;;; ---------- Type checking ----------

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2 in the current enironment.

Signals a TYPE-MISMATCH warning if the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (when (not (subtype-p ty1 ty2))
    (warn 'type-mismatch :expected ty2 :got ty1)))


;;; ---------- Bit widths ----------

;;; TODO: Change this into a proper structure

(defun bitwidth (ty)
  "Return the number of bits needed to represent type TY."
  (destructuring-bind (tytag tyargs)
      (deconstruct-type ty)
    (bitwidth/form tytag tyargs)))


(defgeneric bitwidth/form (tytag tyargs)
  (:documentation "Return the number of bits required to represent (TYTAG . TYARGS).")

  (:method (tytag tyargs)
    (error 'not-representable :type (construct-type tytag tyargs)))

  ;; union types
  (:method ((tytag (eql 'or)) tyargs)
    (apply #'max (mapcar #'bitwidth tyargs)))

  ;; intersection types
  (:method ((tytag (eql 'and)) tyargs)
    (declare (optimize debug))

    (apply #'+ (mapcar #'bitwidth tyargs)))

  ;; types of variables
  (:method ((tytag (eql 'type-of)) tyargs)
    (destructuring-bind (n &optional (f (current-frame)))
	tyargs
      (let ((ty (solve-type-constraints-and-declare n f)))
	(bitwidth ty)))))


;;; ---------- Representability ----------

;;; A type is representable iff it has a known bitwidth

(defun representable-type-p (ty)
  "Test whether TY is representable."
  (handler-case
      (bitwidth ty)
    (not-representable ()
      nil)))


(defun ensure-representable-type (ty)
  "Ensure that TY is representable."
  ;; this is just the calculation itself, which fails if
  ;; the type has no representation
  (bitwidth ty))
