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
    ;; C < A or B  if C < A or C < B
    (some (curry #'subtype-p (construct-type ty1tag ty1args)) ty2args))

  ;; intersection types
  (:method ((ty1tag (eql 'and)) ty1args ty2tag ty2args)
    ;; A and B < C if A < C or B < C
    (every (rcurry #'subtype-p (construct-type ty2tag ty2args)) ty1args)
    )
  (:method (ty1tag ty1args (ty2tag (eql 'and)) ty2args)
    ;; C < A and B if C < A or C < B
    (some (rcurry #'subtype-p (construct-type ty2tag ty2args)) ty1args))

  ;; types of variables
  (:method ((ty1tag (eql 'type-of)) ty1args ty2tag ty2args)
    ;; type-of A < C if referent of A < C
    (destructuring-bind (n &optional (f (currrent-frame)))
	ty1args
      (subtype-p (get-frame-property n 'type f)
		 (construct-type ty2tag ty2args))))
  (:method (ty1tag ty1args (ty2tag (eql 'type-of)) ty2args)
    (destructuring-bind (n &optional (f (currrent-frame)))
	ty2args
      (subtype-p (construct-type ty1tag ty1args)
		 (get-frame-property n 'type f)))))


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
	     (deconstruct-type (eval-type ty1))
	   (destructuring-bind (ty2tag ty2args)
	       (deconstruct-type (eval-type ty2))
	     (subtype-type ty1tag ty1args ty2tag ty2args))))))


;; ---------- Evaluate type parameters ----------

(defgeneric eval-type-type (tytag tyargs)
  (:documentation "Evaluate any parameters in a type.

Methods on this function should reduce a type containing a value
expression to a type containing a literal, signalling a NOT-STATIC
error if this can't be done.

The default is for there to be no parameters to expand.")
  (:method (tytag tyargs)
    (construct-type tytag tyargs))

  (:method ((tytag (eql 'or)) tyargs)
    `(or ,@(mapcar #'eval-type tyargs)))

  (:method ((tytag (eql 'and)) tyargs)
    `(and ,@(mapcar #'eval-type tyargs)))

  (:method ((tytag (eql 'type-of)) tyargs)
    (destructuring-bind (n &optional (f (current-frame)))
	tyargs
      (eval-type (get-frame-property n 'type f)))))


(defun eval-type (ty)
  "Evaluate any parameter values in TY."
  (apply #'eval-type-type (deconstruct-type ty)))


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

The type is tagged TYTAG with arguments TYARGS.

The default width of a type is zero, meaning it won;t be representable.")
  (:method (tytag tyargs)
    0)

  ;; union types
  (:method ((tytag (eql 'or)) tyargs)
    (apply #'max (mapcar #'bitwidth tyargs)))

  ;; intersection types
  (:method ((tytag (eql 'and)) tyargs)
    (apply #'+ (mapcar #'bitwidth tyargs)))

  ;; types of variables
  (:method ((tytag (eql 'type-of)) tyargs)
    (destructuring-bind (n &optional (f (current-frame)))
	tyargs
      (bitwidth (get-frame-property n 'type f)))))


(defun bitwidth (ty)
  "Return the bits required to represent type TY."
  (apply #'bitwidth-type (deconstruct-type ty)))


;; ---------- Representability ----------

(defun representable-type-p (ty)
  "Tes that type TY can be represented."
  (apply #'representable-type-sexp-p (deconstruct-type ty)))


(defgeneric representable-type-sexp-p (tytag tyargs)
  (:documentation "Test that a type can be represented.

Methods on this function should test that there is a representation
for the type, which generally means that it can be represented by a
fixed number of bits.

The default is that types are not representable.")
  (:method (tytag tyargs)
    nil)

  (:method ((tytag (eql 'type-of)) tyargs)
    (destructuring-bind (n &optional (f (current-frame)))
	tyargs
      (representable-type-p (get-frame-property n 'type f)))))


;; ---------- Least upper-bounds ----------

(defgeneric lub-type (ty1tag ty1args ty2tag ty2args)
  (:documentation "Return the least upper-bound of two types in the current environment.

The default LUB of two types is their union type.")
  (:method (ty1tag ty1args ty2tag ty2args)
    `(or ,(construct-type ty1tag ty1args)
	 ,(construct-type ty2tag ty2args)))

  (:method ((ty1tag (eql t)) ty1args ty2tag ty2args)
    t)
  (:method (ty1tag ty1args (ty2tag (eql t)) ty2args)
    t)

  (:method ((ty1tag (eql 'nil)) ty1args ty2tag ty2args)
    (construct-type ty2tag ty2args))
  (:method (ty1tag ty1args (ty2tag (eql nil)) ty2args)
    (construct-type ty1tag ty1args))

  ;; union types
  (:method ((ty1tag (eql 'or)) ty1args ty2tag ty2args)
    (if (= (length ty1args) 1)
	(lub (car ty1args)
	     (construct-type ty2tag ty2args))

	(lub (apply #'lub ty1args)
	     (construct-type ty2tag ty2args))))
  (:method (ty1tag ty1args (ty2tag (eql 'or)) ty2args)
    (lub `(or ,@ty2args) (construct-type ty1tag ty1args)))

  ;; there are as yet no general intersection types

  ;; type-of types
  (:method ((ty1tag (eql 'type-of)) ty1args ty2tag ty2args)
    (destructuring-bind (n &optional (f (current-frame)))
	ty1args
      (lub (get-frame-property n 'type f)
	   (construct-type ty2tag ty2args))))
  (:method (ty1tag ty1args (ty2tag (eql 'type-of)) ty2args)
    (lub `(type-of ,(car ty2args)) (construct-type ty1tag ty1args))))


(defun lub (ty &rest tys)
  "Return the least upper-bound of types TY and any further types in TYS.

The actual type calculations are performed by LUB-TYPE.

The upper bound may not be representable. To ensure representability,
use LURB.

Calling LUB with a single type is a quick way to simplify TY."
  (declare (optimize debug))

  (flet ((lubtype (ty1 ty2)
	   (destructuring-bind (ty1tag ty1args)
	       (deconstruct-type (eval-type ty1))
	     (destructuring-bind (ty2tag ty2args)
		 (deconstruct-type (eval-type ty2))
	       (lub-type ty1tag ty1args ty2tag ty2args)))))

    (if (null tys)
	;; only one type, evaluate it against NIL
	;; (otherwise FOLDR short-cuts and returns TY)
	(lubtype ty nil)

	(foldr #'lubtype tys ty))))


(defun lurb (ty &rest tys)
  "Compute the least upper representable bound of TY and TYS.

The representable bound includes only the representable types in determining the
type to be used. Any unrepresentable types in the list are used to make sure
that the computed type is an appropriate sub-type. The advantage of this is
that an operation can require, for example, a SIGNED-BYTE argument without
committing to a particular width, and have that width be inferred from other
context.

For example,

(LUB '(UNSIGNED-BYTE 8) 'UNSIGNED-BYTE)

is UNSIGNED-BYTE, the upper bound of the two types, while

(LURB '(UNSIGNED-BYTE 8) 'UNSIGNED-BYTE)

is (UNSIGNED-BYTE 8), which is a sub-type of UNSIGNED-BYTE and is the
largest representable type that can be formed."
  (declare (optimize debug))

  (flet ((lurbtype (ty1 ty2)
	   (let ((lubtype (lub ty1 ty2)))
	     (if (representable-type-p lubtype)
		 ;; LUB is representable, return it
		 lubtype

		 ;; otherwise, check what's stopping it
		 (let* ((ety1 (eval-type ty1))
			(ety2 (eval-type ty2))
			(rep1 (representable-type-p ty1))
			(rep2 (representable-type-p ty2)))

		   (cond (rep1
			  (ensure-subtype ety1 ety2)
			  ety1)
			 (rep2
			  (ensure-subtype ety2 ety1)
			  ety2)

			 ;; neither type is representable
			 (t
			  nil)))))))

    (if (null tys)
	;; only one type, evaluate it against NIL
	;; (otherwise FOLDR short-cuts and returns TY)
	(lurbtype ty nil)

	(foldr #'lurbtype tys ty))))


;; ---------- Type constraints ----------

(defun add-type-constraint (n ty)
  "Constrain N to have type TY.

This constraint will be used when inferring the finla type of N."
  (let ((constraints (variable-property n 'type-constraints)))
    (set-variable-property n 'type-constraints (cons ty constraints))))


(defun get-type-constraints (n)
  "Return the type constraints that apply to N."
  (variable-property n 'type-constraints))
