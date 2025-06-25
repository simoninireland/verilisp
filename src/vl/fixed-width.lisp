;; Fixed-width types
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

;; ---------- Fixed-width integers ----------

(defun fixed-width-type-bound (tyargs)
  "Returns any bound in TYARGS, or NIL if the type is unbounded."
  (if (or (null tyargs)
	  (eql (car tyargs) '*))
      nil
      (car tyargs)))


(defun fixed-width-type-bounded-p (ty)
  "Test whether TY is a fixed-width type with bounds."
  (and (fixed-width-p ty)
       (destructuring-bind (tytag tyargs)
	   (deconstruct-type ty)
	 (null (fixed-width-type-bound tyargs)))))


(defmethod subtype-type ((ty1tag (eql 'unsigned-byte)) ty1args
			 (ty2tag (eql 'unsigned-byte)) ty2args)
  (if-let ((ty1width (fixed-width-type-bound ty1args)))
    ;; first type is bounded, second must be unbounded or have
    ;; a larger bound
    (if-let ((ty2width (fixed-width-type-bound ty2args)))
      (<= ty1width ty2width)

      t)

    ;; first type is unbounded, second one must be too
    (null (fixed-width-type-bound ty2args))))


(defmethod subtype-type ((ty1tag (eql 'signed-byte)) ty1args
			 (ty2tag (eql 'signed-byte)) ty2args)
  (if-let ((ty1width (fixed-width-type-bound ty1args)))
    ;; first type is bounded, second must be unbounded or have
    ;; a larger bound
    (if-let ((ty2width (fixed-width-type-bound ty2args)))
      (<= ty1width ty2width)

      t)

    ;; first type is unbounded, second one must be too
    (null (fixed-width-type-bound ty2args))))


(defmethod subtype-type ((ty1tag (eql 'unsigned-byte)) ty1args
			 (ty2tag (eql 'signed-byte)) ty2args)
 (if-let ((ty1width (fixed-width-type-bound ty1args)))
    ;; first type is bounded, second must be unbounded or have
    ;; a strictly larger bound
    (if-let ((ty2width (fixed-width-type-bound ty2args)))
      (< ty1width ty2width)

      t)

    ;; first type is unbounded, second one must be too
    (null (fixed-width-type-bound ty2args))))

;; there is no method for SIGNED-BYTE compared to UNSIGNED-BYTE


(defmethod subtype-type ((ty1tag (eql 'bit)) ty1args
			 ty2tag ty2args)
  (subtype-p '(unsigned-byte 1) (construct-type ty2tag ty2args)))


(defun fixed-width-p (ty)
  "Test whether TY is a fixed-width type."
  (subtype-p ty '(or unsigned-byte signed-byte)))


(defun signed-byte-p (ty)
  "Test whether TY is a signed fixed-width type."
  (subtype-p ty '(and signed-byte (not unsigned-byte))))


(defun unsigned-byte-p (ty)
  "Test whether TY is an unsigned fixed-width type."
  (subtype-p ty '(and 'unsigned-byte (not signed-byte))))


(defun ensure-fixed-width (ty)
  "Ensure that TY is a fixed-width integer."
  (unless (fixed-width-p ty)
    (warn 'type-mismatch :expected '(unsigned-byte
				     signed-byte)
			 :got ty)))


(defmethod expand-type-parameters-type ((ty (eql 'unsigned-byte)) args)
  (if (null args)
      ty
      (let ((bounds (car args)))
	(if (eql bounds '*)
	    '(unsigned-byte *)
	    `(unsigned-byte ,(eval-in-static-environment bounds))))))


(defmethod expand-type-parameters-type ((ty (eql 'signed-byte)) args)
  (if (null args)
      ty
      (let ((bounds (car args)))
	(if (eql bounds '*)
	    '(unsigned-byte *)
	    `(signed-byte ,(eval-in-static-environment bounds))))))


;; ---------- Least upper-bound ----------

(defmethod lub-type ((ty1tag (eql 'unsigned-byte)) ty1args
		     (ty2tag (eql 'unsigned-byte)) ty2args)
  (if-let ((ty1width (fixed-width-type-bound ty1args))
	   (ty2width (fixed-width-type-bound ty2args)))
    ;; both types have bounds, take the max
    `(unsigned-byte ,(max ty1width ty2width))

    ;; one or both types are unbound, return the unbounded type
    'unsigned-byte))


(defmethod lub-type ((ty1tag (eql 'signed-byte)) ty1args
		     (ty2tag (eql 'signed-byte)) ty2args)
  (if-let ((ty1width (fixed-width-type-bound ty1args))
	   (ty2width (fixed-width-type-bound ty2args)))
    ;; both types have bounds, take the max
    `(signed-byte ,(max ty1width ty2width))

    ;; one or both types are unbound, return the unbounded type
    'signed-byte))


(defmethod lub-type ((ty1tag (eql 'unsigned-byte)) ty1args
		     (ty2tag (eql 'signed-byte)) ty2args)
  (if-let ((ty1width (fixed-width-type-bound ty1args))
	   (ty2width (fixed-width-type-bound ty2args)))
    ;; both types have bounds, take the max accounting for sign
    `(signed-byte ,(max (1+ ty1width) ty2width))

    ;; one or both types are unbound, return the unbounded type
    'signed-byte))


(defmethod lub-type ((ty1tag (eql 'signed-byte)) ty1args
		     (ty2tag (eql 'unsigned-byte)) ty2args)
  (lub (construct-type ty2tag ty2args) (construct-type ty1tag ty1args)))


(defmethod lub-type ((ty1tag (eql 'bit)) ty1args
		     ty2tag ty2args)
  (lub '(unsigned-byte 1) (construct-type ty2tag ty2args)))


(defmethod lub-type (ty1tag ty1args
		     (tytag2 (eql 'bit)) ty2args)
  (lub (construct-type ty1tag ty1args) '(unsigned-byte 1)))


;; ---------- Widths ----------

(defun bits-for-integer (val)
  "Return the number of bits needed to represent VAL."
  (flet ((bfi (val)
	   (multiple-value-bind (b res)
	       (ceiling (log val 2))
	     (let ((bits (max (if (= res 0.0)
				  ;; add a bit if val is on a
				  ;; power-of-two boundary
				  (1+ b)
				  b)
			      1)))	; always need at least one bit
	       bits))))
    (cond ((= val 0)
	   1)

	  ((> val 0)
	   (bfi val))

	  (t
	   (1+ (bfi (abs val)))))))


(defmethod bitwidth-type ((tytag (eql 'unsigned-byte)) tyargs)
  (car tyargs))


(defmethod bitwidth-type ((tytag (eql 'signed-byte)) tyargs)
  (car tyargs))
