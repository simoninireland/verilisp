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

(in-package :vl)


;; ---------- Sub-type checking ----------

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

This is simply TYTAG is TYARGS is nil, or a list specifier."
  (if (null tyargs)
      tytag
      (cons tytag tyargs)))


(defun representable-type-p (ty)
  "Tests whether TY is representable.

Cut-off un-representable types in the type lattice. This includes
unbounded UNSIGNED-BYTE and SIGNED-BYTE types, and the lattice types T
and NIL."
  (destructuring-bind (tytag tyargs)
      (deconstruct-type ty)

    (not (or (null tytag)
	     (eql tytag t)
	     (and (member tytag '(unsigned-byte signed-byte))
		  (null tyargs))))))


;; The builtin SUBTYPEP is sometimes either too aggressive or too
;; demanding in what it requires. So we provide a generic version
;; that's minimally intrusive.

(defgeneric subtype-type (ty1tag ty1args ty2tag ty2args)
  (:documentation "Test whether one type is a sub-type of another.

Methods on this function should specialise on the type tags and
be a generally admitting as possible, for example by not examining
tyer arguments when not needed.")

  (:method (ty1tag ty1args (ty2tag (eql 'or)) ty2args)
    ;; union type, one of the types matches
    (some (curry #'subtype-p (construct-type ty1tag ty1args)) ty2args))

  (:method (ty1tag ty1args (ty2tag (eql 'and)) ty2args)
    ;; intersection type, all of the types matches
    (every (curry #'subtype-p (construct-type ty1tag ty1args)) ty2args))

  (:method (ty1tag ty1args (ty2tag (eql 'not)) ty2args)
    ;; negation type, must not match
    (not (subtype-p (construct-type ty1tag ty1args) (car ty2args))))

					;TODO: Extend to handle complex type specifiers on the left as well
  ;; as on the right
  )


(defun subtype-p (ty1 ty2)
  "Determine whether TY1 is a sub-type of TY2.

Some complex type specifiers are supported for TY2, currently OR, AND,
and NOT types. The 'lattice' types of top (T) and bottom (NIL) are
also supported

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

	     (handler-case
		 (subtype-type ty1tag ty1args ty2tag ty2args)

	       ;; if there is no comparison provided, the test fails
	       (error nil)))))))


;; ---------- Type checking ----------

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2 in the current enironment.

Signals TYPE-MISMATCH is the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (let ((ety1 (expand-type-parameters ty1))
	(ety2 (expand-type-parameters ty2)))
    (when (not (subtype-p ety1 ety2))
      (warn 'type-mismatch :expected ty2 :got ty1))))


;; ---------- Bit widths ----------

(defgeneric bitwidth (v)
  (:documentation "Return the bits required to represent V.

A list V is assumed to be a type specifier, which is passed to
BITWIDTH-TYPE.")
  ;; TODO: I'm not entirely sure about this as a design
  (:method ((v list))
    (let ((tys (safe-car-cdr v)))
      (bitwidth-type (car tys) (cadr tys)))))


(defgeneric bitwidth-type (tytag tyargs)
  (:documentation "Return the width need for values of a type.

The type is tagged TYTAG with arguments TYARGS."))


;; ---------- Least upper-bounds ----------

(defgeneric lub-type (ty1tag ty1args ty2tag ty2args)
  (:documentation "Return the least upper-bound of two types in the current environment.

The types are tagged TY1TAG and TY2TAG, with specialising arguments
TY1ARGS and TY2ARGS respectively (both of which can be nil).

The default LUB of two types is T, the top type.")
  (:method (ty1tag ty1args ty2tag ty2args)
    t)

  ;TODO: Extend to handle complex type specifiers
  )


(defun lub (ty &rest tys)
  "Return the least upper-bound of types TY and any further types in TYS.

By default the upper bound is T, and if either is null the
upper bound is the other. For other combinations the type tag
is extracted and used in a call to LUB-TYPE.

Type parameters are not expanded by default."
  (flet ((lubtype (ty1 ty2)
	   (cond ((null ty1)
		  ty2)
		 ((null ty2)
		  ty1)
		 (t
		  (destructuring-bind (ty1tag ty1args)
		      (deconstruct-type ty1)
		    (destructuring-bind (ty2tag ty2args)
			(deconstruct-type ty2)

		      (lub-type ty1tag ty1args ty2tag ty2args)))))))

    (foldr #'lubtype tys ty)))


(defun lurb (ty &rest tys)
  "Return the least upper representable bound of TY1 and any further TYS.

Representable types are those identified by REPRESENTABLE-TYPE-P. A
NOT-REPRESENTABLE error is signalled if there is no representable
upper bound."
  (flet ((lurbtype (ty1 ty2)
	   (if (representable-type-p ty1)
	       (if (representable-type-p ty2)
		   ;; both types are representable, return their LUB
		   (lub ty1 ty2)

		   ;; TY2 is un-representable, return TY1
		   ty1)

	       ;; TY1 is un-representable, return TY2
	       ty2)))

    (let ((lurb (foldr #'lurbtype tys ty)))
      (if (representable-type-p lurb)
	  lurb

	  (error 'not-representable :type lurb
				    :hint "Make sure types are representable")))))


;; ---------- Type parameter expansion ----------

(defgeneric expand-type-parameters (ty)
  (:documentation "Expand any parameters in the type TY in the current environment.

This expands constants that appear within type declarations,
which always need to be statically constant.")
  (:method (ty)
    ty)
  (:method ((ty list))
    (expand-type-parameters-type (car ty) (cdr ty))))


(defgeneric expand-type-parameters-type (ty args)
  (:documentation "Expand any parameters in the type tag TY applied to ARGS."))


;; ---------- Type constraints in environments ----------

(defun add-type-constraint (n ty)
  "Constraint N to have type TY.

N must be in scope, but need not be in the shallowest frame of ENV."
  (let ((constraints (variable-property n :type-constraints :default nil)))
    (set-variable-property n :type-constraints (cons ty constraints))))
