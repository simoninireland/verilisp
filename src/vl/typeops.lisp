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
  ;; A or B < C iff A < C and B < C
  (every (rcurry #'subtype-p ty) tys))

(defsubtype ((&type ty) (or &rest tys))
  ;; C < A or B iff C < A or C < B
  (some (curry #'subtype-p ty) tys))

(deflub ((or &rest tys) (&type ty))
  (lub (foldr #'lub (cdr tys) (car tys)) ty))
(deflub ((&type ty) (or &rest tys))
  (lub `(or ,@tys) ty))


;;; Intersection types

(defsubtype ((&type ty) (and &rest tys))
  ;; C < A and B iff C < A and C < B
  (every (curry #'subtype-p ty) tys))

(deflub ((&type ty) (and &rest tys))
  (lub `(and ,@tys) ty))


;;; type-of types

;;; Types containing references to the types of other variables
;;; mean that we may need to solve type constraints in the course
;;; of finding LUBs. Since there's a danger of circularity, we
;;; define a queue of variables having their types solved and
;;; escape if we encounter a cycle. (This is possibly too strong,
;;; but I don't think there are fixpoints to be found in the
;;; Verilisp type system.)

(defvar *variables-being-constrained* nil
  "List of variables names a frames that are having their types solved.

This is used only in computing LUBs involving TYPE-OF types.")


(defsubtype ((type-of a f) (&type ty2))
  (if-let ((ty1 (get-frame-property a 'type f :default nil)))
    ;; either we've constrained the type fully or it's been declared
    ;; explicitly (and won't be inferred to be wider)
    (subtype-p ty1 ty2)

    ;; we don't know the full type of a variable until it's been fully
    ;; constrained, but we can work out an upper bound and leave it to
    ;; be further refined later
    ;; TODO: Not sure this is the right approach
    (if-let ((constraints (get-frame-property a 'type-constraints f :default nil)))
      (subtype-p (solve-type-constraints a f constraints) ty2)

      ;; otherwise we have to stay unconstrained
      t)))


(defsubtype ((&type ty1) (type-of a f))
  (if-let ((ty2 (get-frame-property a 'type f :default nil)))
    (subtype-p ty1 ty2)

    (if-let ((constraints (get-frame-property a 'type-constraints f :default nil)))
      (subtype-p ty1 (solve-type-constraints a f constraints))

      t)))


(deflub ((type-of a f) (&type ty2))
  (if-let ((ty1 (get-frame-property a 'type f :default nil)))
    ;; type has been declared or solved
    (lub ty1 ty2)

    ;; type hasn't been solved, solve it
    (progn
      (when (assoc a *variables-being-constrained*)
	;; circular dependencies
	(let ((constraints (get-frame-property a 'type-constraints f :default nil)))
	  (error 'circular-type-dependencies :variable a
					     :involved constraints
					     :hint "Declare types for one or more of the variables involved")))

      ;; solve the constraints
      (let ((*variables-being-constrained* (cons (list a f)
						 *variables-being-constrained*)))
	(lub (solve-type-constraints-and-declare a f) ty2)))))

(deflub ((&type ty1) (type-of a f))
  (lub `(type-of ,a ,f) ty1))


;;; ---------- Closure types ----------

(defsubtype ((closure ty1 env) (&type ty2))
  (in-environment env
    (subtype-p ty1 ty2)))

(defsubtype ((&type ty1) (closure ty2 env))
  (in-environment env
    (subtype-p ty1 ty2)))


(deflub ((closure ty1 env) (&type ty2))
  (in-environment env
    (lub ty1 ty2)))

(deflub ((&type ty1) (closure ty2 env))
  (in-environment env
    (lub ty1 ty2)))


;;; ---------- Type constraints ----------

(defun add-frame-type-constraint (n ty env)
  "Constrain variable N to have type TY in ENV.

This constraint will be used when inferring the finla type of N."
  (let ((constraints (get-frame-property n 'type-constraints env)))
    (set-frame-property n 'type-constraints (cons ty constraints) env)))

(defun add-type-constraint (n ty)
  "Constrain N to have type TY in the current environment.

This constraint will be used when inferring the final type of N."
  (declare (optimize debug))

  (let ((env (get-frame-declaring n (current-environment))))
    (when (null env)
      (error 'unknown-variable :variable n))
    (add-frame-type-constraint n ty env)))


(defun get-type-constraints (n)
  "Return the type constraints that apply to N."
  (variable-property n 'type-constraints))


;;; ---------- Type constraint solving ----------

;;; Verilisp uses a constraint-based type-checker. These functions solve
;;; a set of type constraints to determine the smallest type that can
;;; represent the values required.

(defun solve-type-constraints (n f constraints)
  "Determine the smallest representable type for N in F under CONSTRAINTS.

Return the inferred type."
  (declare (optimize debug))

  (when (member n *variables-being-constrained* :key #'car)
    (error 'circular-type-dependencies :variable n :involved constraints))

  (let ((*variables-being-constrained* (cons (list n f)
					     *variables-being-constrained*)))

    (apply #'lub constraints)))


(defun solve-type-constraints-and-declare (n f)
  "Determine the smallest representable type for N in frame F.

The type inferred is used to declare N's type in F. If N already
has a declared type it is retained, but a warning is signalled if
the inferred type is not compatible with the declared type.

It is safe to call this method repeatedly for the same variable, as
once it's been called once (and has resolved the constraints) they are
deleted to prevent re-solving."
  (declare (optimize debug))

  (let* ((cs (get-frame-property n 'type-constraints f :default nil))
	 (ety (get-frame-property n 'type f :default nil))
	 (ty (if (null cs)
	       ;; no constraints (and therefore no initial value), is there an explicit type?
	       (if ety
		   ;; explicit type provided, use it
		   ety

		   ;; no explicit type either, use the compiler's default type
		   ;; if it has one
		   (if-let ((dty (get-compiler-flag 'default-variable-type)))
		     dty

		     ;; no default type either, fail
		     ;; TODO: This needs to be a different condition
		     (error 'compiler-error :hint (format nil "No explicit type, constraints, or default type for variable ~s" n))))

	       ;; we have constraints, solve them
	       (handler-case
		   (let ((sty (solve-type-constraints n f cs)))
		     ;; remove the constraints so we don't re-solve them
		     (set-frame-property n 'type-constraints nil f)

		     (if ety
			 ;; we have an explicit type, check inferred type for compatibility
			 (progn
			   (unless (subtype-p sty ety)
			     (warn 'type-mismatch :expected ety
						  :got sty
						  :hint "Check the declared type is appropriate"))

			   ;; retain the explicit type, /not/ the inferred one
			   ety)

			 ;; no explicit type to compare against, use the inferred type
			 sty))

		 (circular-type-dependencies ()
		   ;; see if we have an explicit type we can use
		   (if ety
		       (progn
			 ;; yes, use that (with a warning)
			 (warn 'type-defaulted :variable n :defaulted-to ety)

			 ;; use the explcit type
			 ety)

		       ;; no explicit type, try the compiler default
		       (if-let ((dty (get-compiler-flag 'default-variable-type)))
			 (progn
			   ;; we have a compiler default, use that (with a warning)
			   (warn 'type-defaulted :variable n :defaulted-to dty)

			   ;; use the default type
			   dty)

			 ;; no default type either, fail
			 (error 'circular-type-dependencies :variable n :involved cs
							    :hint "Add explicit types for some or all variables"))))))))

    ;; record the inferred type
    (set-frame-property n 'type ty f)

    ty))


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
  "Return the number of bits needed to represent type TY.

This may be a value or more generally a static constant that will
This need to be evaluated (using EVAL-IN-STATIC-ENVIRONMENT) if
This used internally."
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
    (apply #'+ (mapcar #'bitwidth tyargs)))

  ;; types of variables
  (:method ((tytag (eql 'type-of)) tyargs)
    (declare (optimize debug))

    (destructuring-bind (n &optional (f (current-frame)))
	tyargs
      (let ((ty (solve-type-constraints-and-declare n f)))
	(bitwidth ty))))

  ;; closure types
  (:method ((tytag (eql 'closure)) tyargs)
    (destructuring-bind (ty env)
	tyargs
      (in-environment env
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
