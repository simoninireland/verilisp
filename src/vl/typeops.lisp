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

;;; Union types

(defsubtype ((or &rest tys) (&type ty))
  ;; A or B < C if A < C and B < C
  (every (rcurry #'subtype-p ty) tys))

(defsubtype ((&type ty) (or &rest tys))
  ;; C < A or B  if C < A or C < B
  (some (curry #'subtype-p ty) tys))

(deflub ((or &rest tys) (&type ty))
  (lub (foldr #'lub tys nil) ty))


;;; There are no general intersection types (yet)


;; type-of types

(defsubtype ((type-of a f) (&type ty2))
  (let ((ty1 (get-frame-property a 'type f)))
    (subtype-p ty1 ty2)))

(defsubtype ((&type ty1) (type-of a f))
    (let ((ty2 (get-frame-property a 'type f)))
      (subtype-p ty1 ty2)))

(deflub ((type-of a f) (&type ty2))
   (let ((ty1 (get-frame-property a 'type f)))
     (lub ty1 ty2)))

(deflub ((&type ty1) (type-of a f))
   (let ((ty2 (get-frame-property a 'type f)))
     (lub ty1 ty2)))


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
    (apply #'+ (mapcar #'bitwidth tyargs)))

  ;; types of variables
  (:method ((tytag (eql 'type-of)) tyargs)
    (destructuring-bind (n &optional (f (current-frame)))
	tyargs
      (bitwidth (get-frame-property n 'type f)))))


;;; ---------- Rep[resentability ----------

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
