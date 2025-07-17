;; Type operations
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


;; ---------- Type manipulation ----------

(defun deconstruct-type (ty)
  "Deconstruct the type specifier TY into tag and arguments.

If TY is a naked symbol then the arguments part is nil; otherwise
it contains the arguments."
  (if (listp ty)
      (if (null ty)
	  (list '() '())
	  (list (car ty) (cdr ty)))
      (list ty '())))


(defun construct-type (tytag tyargs)
  "Construct a type specifier from TYTAG and TYARGS.

This is simply TYTAG is TYARGS is nil, or a list specifier consisting
of TYTAG and TYARGS."
  (if (null tyargs)
      tytag
      (cons tytag tyargs)))


;; ---------- Type algebra ----------

;; The builtin SUBTYPEP is sometimes either too aggressive or too
;; demanding in what it requires. So we provide a generic version
;; that's minimally intrusive.

(defgeneric subtype-type (ty1tag ty1args ty2tag ty2args)
  (:documentation "Test whether one type is a sub-type of another.

Methods on this function should specialise on the type tags and
be as generally admitting as possible, for example by not examining
the arguments when not needed.

By default there is no relationship between a pair of types.")
  (:method (ty1tag ty1args ty2tag ty2args)
    nil)

  ;; union types
  (:method ((ty1tag (eql 'or)) ty1args ty2tag ty2args)
    ;; A or B < C if A < C and B < C
    (every (rcurry #'subtype-p (construct-type ty2tag ty2args)) ty1args))

  (:method (ty1tag ty1args (ty2tag (eql 'or)) ty2args)
    (not (subtype-p (construct-type ty2tag ty2args)
		    (construct-type ty1tag ty1args))))

  ;; types of variables
  (:method ((ty1tag (eql 'type-of)) ty1args ty2tag ty2args)
    ;; type-of A < C if A < C
    (subtype-p (get-type (car ty1args)) (construct-type ty2tag ty2args)))

  (:method (ty1tag ty1args (ty2tag (eql 'type-of)) ty2args)
    (not (subtype-p (construct-type ty2tag ty2args)
		    (construct-type ty1tag ty1args))))

  ;; widened types
  (:method ((ty1tag (eql 'widen)) ty1args ty2tag ty2args)
    ;; widen A by n < C if widened A < C
    (subtype-p (widen (lub (car ty1args)) (cadr ty1args))
	       (construct-type ty2tag ty2args)))

  (:method (ty1tag ty1args (ty2tag (eql 'widen)) ty2args)
    (not (subtype-p (construct-type ty2tag ty2args)
		    (construct-type ty1tag ty1args)))))


(defun subtype-p (ty1 ty2)
  "Determine whether TY1 is a sub-type of TY2.

Some complex type specifiers are supported, currently OR and AND
types. The 'lattice' types of top (T) and bottom (NIL) are also
supported. The TYPE-OF type retrieves the type of a variable from the
current enviroment. Other relationships are defined by SUBTYPE-TYPE.

This function should be used in preference to the built-in SUBTYPEP
when comparing Verilisp types."
  (declare (optimize debug))

  (cond ((null ty1)
	 ;; the nil (bottom) type is a sub-type of everything
	 t)

	((eql ty1 t)
	 ;; the t (top) type is only a super-type of top
	 (eql ty2 t))

	((null ty2)
	 ;; the nil (bottom) type has no sub-types
	 nil)

	((eql ty2 t)
	 ;; the t (top) type has every type as subtypes
	 t)

	(t
	 (destructuring-bind (ty1tag ty1args)
	     (deconstruct-type ty1)
	   (destructuring-bind (ty2tag ty2args)
	       (deconstruct-type ty2)
	     (subtype-type ty1tag ty1args ty2tag ty2args))))))


;; ---------- Type checking ----------

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2 in the current enironment.

Signals a TYPE-MISMATCH warning if the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (when (not (subtype-p ty1 ty2))
    (warn 'type-mismatch :expected ty2 :got ty1)))


;; ---------- Bit widths ----------

(defgeneric bitwidth-type (tytag tyargs)
  (:documentation "Return the width need for values of a type.

The type is tagged TYTAG with arguments TYARGS.")
  (:method (tytag tyargs)
    nil)

  ;; union and intersection types
  (:method ((tytag (eql 'or)) tyargs)
    (max (mapcar #'bitwidth tyargs)))
  (:method ((tytag (eql 'and)) tyargs)
    (max (mapcar #'bitwidth tyargs)))

  ;; types of variables
  (:method ((tytag (eql 'type-of)) tyargs)
    (bitwidth (get-type (car tyargs))))

  ;; widened types
  (:method ((tytag (eql 'widen)) tyargs)
    (+ (bitwidth (car tyargs)) (cadr tyargs))))


(defun bitwidth (ty)
  "Return the bits required to represent type TY."
  (apply #'bitwidth-type (deconstruct-type ty)))


(defgeneric widen-type (tytag tyargs n)
  (:documentation "Widen a type by N bits.

Methods on this function should widen the type accordingly. The default
is for types not to be wideneable, which signals a TYPE-MISMATCH warning.")
  (:method (tytag tyargs n)
    (let ((ty (construct-type tytag tyargs)))
      (error 'type-mismatch :expected '(unsigned-byte signed-byte bit)
			    :got ty
			    :hint "Can only widen fixed-width types"))

    ;; return the type as-is
    ty))


(defun widen (ty n)
  "Widen type TY by N bits."
  (apply #'widen-type (append (deconstruct-type ty) (list n))))


;; ---------- Least upper-bounds ----------

(defgeneric lub-type (ty1tag ty1args ty2tag ty2args)
  (:documentation "Return the least upper-bound of two types in the current environment.

The default LUB of two types is their union type.")
  (:method (ty1tag ty1args ty2tag ty2args)
    (cond ((null ty2tag)
	   (construct-type ty1tag ty1args))

	  ((eql ty2tag nil)
	   t)

	  (t
	   `(or ,(construct-type ty1tag ty1args)
		,(construct-type ty2tag ty2args)))))

  (:method ((ty1tag (eql t)) ty1args ty2tag ty2args)
    t)

  (:method ((ty1tag (eql 'nil)) ty1args ty2tag ty2args)
    (construct-type ty2tag ty2args))

  (:method ((ty1tag (eql 'or)) ty1args ty2tag ty2args)
    (if (= (length ty1args) 1)
	(lub (car ty1args)
	     (construct-type ty2tag ty2args))

	(lub (apply #'lub ty1args)
	     (construct-type ty2tag ty2args))))

  (:method ((ty1tag (eql 'type-of)) ty1args ty2tag ty2args)
    (lub (get-type (car ty1args))
	 (construct-type ty2tag ty2args)))

  (:method ((ty1tag (eql 'widen)) ty1args ty2tag ty2args)
    (lub (widen (lub (car ty1args)) (cadr ty1args))
	 (construct-type ty2tag ty2args))))


(defun lub (ty &rest tys)
  "Return the least upper-bound of types TY and any further types in TYS.

The actual type calculations are performed by LUB-TYPE.

Calling LUB with a single type is a quick way to simplify TY."
  (declare (optimize debug))

  (flet ((lubtype (ty1 ty2)
	   (destructuring-bind (ty1tag ty1args)
	       (deconstruct-type ty1)
	     (destructuring-bind (ty2tag ty2args)
		 (deconstruct-type ty2)
	       (lub-type ty1tag ty1args ty2tag ty2args)))))

    (if (null tys)
	;; only one type, make sure we try to reduce it
	;; (otherwise FOLDR short-cuts and returns TY)
	(lub ty nil)

	;; otherwise, fold across the types
	(foldr #'lubtype tys ty))))


(defun representable-type-p (ty)
  "Tests whether TY is representable on hardware."
  (destructuring-bind (tytag tyargs)
      (deconstruct-type ty)

    (not (or (null tytag)
	     (eql tytag t)
	     (and (member tytag '(unsigned-byte signed-byte))
		  (null tyargs))))))


(defun lurb (ty &rest tys)
  "Return the least upper representable bound of TY1 and any further TYS.

Representable types are those identified by REPRESENTABLE-TYPE-P. A
NOT-REPRESENTABLE error is signalled if there is no representable
upper bound."
  (let ((lurbtype (apply #'lub (cons ty tys))))
    (if (representable-type-p lurbtype)
	lurbtype

	(error 'not-representable :type lurbtype
				  :hint "Make sure types are representable"))))


;; ---------- Type constraints ----------

(defun add-type-constraint (n ty)
  "Constrain N to have type TY.

This constraint will be used when inferring the finla type of N."
  (let ((constraints (variable-property n 'type-constraints)))
    (set-variable-property n 'type-constraints (cons ty constraints))))


(defun get-type-constraints (n)
  "Return the type constraints that apply to N."
  (variable-property n 'type-constraints))
