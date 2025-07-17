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
  (:method (ty1tag ty1args (ty2tag (eql 'or)) ty2args)
    ;; C < A or B if C < A or C < B
    (some (curry #'subtype-p (construct-type ty1tag ty1args)) ty2args))

  (:method ((ty1tag (eql 'or)) ty1args ty2tag ty2args)
    ;; A or B < C if A < C and B < C
    (every (rcurry #'subtype-p (construct-type ty2tag ty2args)) ty1args))

  ;; intersection types
  (:method (ty1tag ty1args (ty2tag (eql 'and)) ty2args)
    ;; C < A and B if C < A and C < B
    (every (curry #'subtype-p (construct-type ty1tag ty1args)) ty2args))

  (:method ((ty1tag (eql 'and)) ty1args ty2tag ty2args)
    ;; A and B < C if A < C or B < C
    (some (rcurry #'subtype-p (construct-type ty2tag ty2args)) ty1args))

  ;; types of variables
  (:method (ty1tag ty1args (ty2tag (eql 'type-of)) ty2args)
    (subtype-p (construct-type ty1tag ty1args) (get-type (car ty2args))))

  (:method ((ty1tag (eql 'type-of)) ty1args ty2tag ty2args)
    (subtype-p (get-type (car ty1args)) (construct-type ty2tag ty2args)))

  ;; widened types
  (:method (ty1tag ty1args (ty2tag (eql 'widen)) ty2args)
    (subtype-p (construct-type ty1tag ty1args)
	       (apply #'widen ty2args)))

  (:method ((ty1tag (eql 'widen)) ty1args ty2tag ty2args)
    (subtype-p (apply #'widen ty1args)
	       (construct-type ty2tag ty2args))))


(defun disjointtype-p (ty1 ty2)
  "Test whether TY1 and TY2 are disjoint types."
  (and (not (subtype-p ty1 ty2))
       (not (subtype-p ty2 ty1))))


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
    (bitwidth (get-type (car tyargs)))))


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
    `(or ,(construct-type ty1tag ty1args) ,(construct-type ty2tag ty2args)))

  ;; union types
  (:method  (ty1tag ty1args (ty2tag (eql 'or)) ty2args)
    (apply #'lub (cons (construct-type ty1tag ty1args) ty2args)))

  (:method ((ty1tag (eql 'or)) ty1args ty2tag ty2args)
    (lub (construct-type ty2tag ty2args) (construct-type ty1tag ty1args)))

  ;; intersection types
  (:method (ty1tag ty1args (ty2tag (eql 'and)) ty2args)
    (lub (construct-type ty1tag ty1args) `(or ,ty2args)))

  (:method ((ty1tag (eql 'and)) ty1args ty2tag ty2args)
    (lub (construct-type ty2tag ty2args) (construct-type ty1tag ty1args)))

  ;; types of variables
  (:method (ty1tag ty1args (ty2tag (eql 'type-of)) ty2args)
    (lub (construct-type ty1tag ty1args) (get-type (car ty2args))))

  (:method (ty1tag ty1args (ty2tag (eql 'type-of)) ty2args)
    (lub (construct-type ty2tag ty2args) (construct-type ty1tag ty1args)))

  ;; widened types
  (:method (ty1tag ty1args (ty2tag (eql 'widen)) ty2args)
    (lub (construct-type ty1tag ty1args) (apply #'widen ty2args)))

  (:method ((ty1tag (eql 'widen)) ty1args ty2tag ty2args)
    (lub (construct-type ty2tag ty2args) (construct-type ty1tag ty1args))))


(defun lub (ty &rest tys)
  "Return the least upper-bound of types TY and any further types in TYS.

By default the upper bound is T, and if either is null the upper bound
is the other. All other combinations make a call to LUB-TYPE."
  (flet ((lubtype (ty1 ty2)
	   (cond ((null ty1)
		  ty2)

		 ((null ty2)
		  ty1)

		 ((or (eql ty1 t)
		      (eql ty2 t))
		  t)

		 (t
		  (destructuring-bind (ty1tag ty1args)
		      (deconstruct-type ty1)
		    (destructuring-bind (ty2tag ty2args)
			(deconstruct-type ty2)
		      (lub-type ty1tag ty1args ty2tag ty2args)))))))

    (foldr #'lubtype tys ty)))


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
