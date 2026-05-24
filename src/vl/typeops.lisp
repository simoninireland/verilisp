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


;;; ---------- Type algebra ----------

;;; Fixed-width types

(defsubtype ((unsigned-byte a) (unsigned-byte b))
  (if (eql b '*)
      (or (null a)
	  (eql a '*))

      (<= a b)))

(defsubtype ((signed-byte a) (signed-byte b))
  (if (eql b '*)
      (or (null a)
	  (eql a '*))

      (<= a b)))

(defsubtype (unsigned-byte signed-byte)
  t)

(defsubtype ((unsigned-byte a) (signed-byte b))
  (<= a (1+ b)))

(deflub (unsigned-byte unsigned-byte)
	'unsigned-byte)


;;; Abbreviations for fixed-width types

(defsubtype (bit (&type ty))
  (subtype-p '(unsigned-byte 1) ty))

(defsubtype ((&type ty) bit)
  (subtype-p ty '(unsigned-byte 1)))


;;; Arrays

;;; TODO: Should we check dimensions, since we can do so statically?

(defsubtype ((array lty ldim) (array rty rdim))
  "Arrays are covariant in their element type."
  (subtype-p lty rty))


;;; Union types

(defsubtype ((or &rest tys) (&type ty))
  ;; A or B < C if A < C and B < C
  (every (rcurry #'subtype-p ty) tys))

(defsubtype ((&type ty) (or &rest tys))
  ;; C < A or B  if C < A or C < B
  (some (curry #'subtype-p ty) tys))

(deflub ((or &rest tys) (&type ty))
  (lub (foldr #'lub tys nil) ty))


;; type-of types

(defsubtype ((type-of a f) (&type ty))
  (let ((ty1 (get-type a f)))
    (subtype-p ty1 ty)))

(defsubtype ((&type ty) (type-of a f))
    (let ((ty2 (get-type a f)))
      (subtype-p ty ty2)))


;;; ---------- Type checking ----------

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2 in the current enironment.

Signals a TYPE-MISMATCH warning if the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (when (not (subtype-p ty1 ty2))
    (warn 'type-mismatch :expected ty2 :got ty1)))


;;; ---------- Bit widths ----------

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


;; ;;; ---------- Representability ----------

;; (defun representable-type-p (ty)
;;   "Tes that type TY can be represented."
;;   (apply #'representable-type-sexp-p (deconstruct-type ty)))


;; (defgeneric representable-type-sexp-p (tytag tyargs)
;;   (:documentation "Test that a type can be represented.

;; Methods on this function should test that there is a representation
;; for the type, which generally means that it can be represented by a
;; fixed number of bits.

;; The default is that types are not representable.")
;;   (:method (tytag tyargs)
;;     nil)

;;   (:method ((tytag (eql 'type-of)) tyargs)
;;     (destructuring-bind (n &optional (f (current-frame)))
;;	tyargs
;;       (representable-type-p (get-frame-property n 'type f)))))


;; ;;; ---------- Least upper-bounds ----------

;; (defun lurb (ty &rest tys)
;;   "Compute the least upper representable bound of TY and TYS.

;; The representable bound includes only the representable types in determining the
;; type to be used. Any unrepresentable types in the list are used to make sure
;; that the computed type is an appropriate sub-type. The advantage of this is
;; that an operation can require, for example, a SIGNED-BYTE argument without
;; committing to a particular width, and have that width be inferred from other
;; context.

;; For example,

;; (LUB '(UNSIGNED-BYTE 8) 'UNSIGNED-BYTE)

;; is UNSIGNED-BYTE, the upper bound of the two types, while

;; (LURB '(UNSIGNED-BYTE 8) 'UNSIGNED-BYTE)

;; is (UNSIGNED-BYTE 8), which is a sub-type of UNSIGNED-BYTE and is the
;; largest representable type that can be formed."
;;   (declare (optimize debug))

;;   (flet ((lurbtype (ty1 ty2)
;;	   (let ((lubtype (lub ty1 ty2)))
;;	     (if (representable-type-p lubtype)
;;		 ;; LUB is representable, return it
;;		 lubtype

;;		 ;; otherwise, check what's stopping it
;;		 (let* ((ety1 (eval-type ty1))
;;			(ety2 (eval-type ty2))
;;			(rep1 (representable-type-p ty1))
;;			(rep2 (representable-type-p ty2)))

;;		   (cond (rep1
;;			  (ensure-subtype ety1 ety2)
;;			  ety1)
;;			 (rep2
;;			  (ensure-subtype ety2 ety1)
;;			  ety2)

;;			 ;; neither type is representable
;;			 (t
;;			  nil)))))))

;;     (if (null tys)
;;	;; only one type, evaluate it against NIL
;;	;; (otherwise FOLDR short-cuts and returns TY)
;;	(lurbtype ty nil)

;;	(foldr #'lurbtype tys ty))))


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
