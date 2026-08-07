;;;; Fixed-width types
;;;;
;;;; Copyright (C) 2024--2025 Simon Dobson
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

;;; We re-use the Common Lisp fixed-width types like UNSIGNED-BYTE, but
;;; we need more flexible checking and manipulation.


;;; ---------- Type algebra ----------

(defsubtype ((unsigned-byte a) (unsigned-byte b))
  (cond ((or (null b)
	     (eql b '*))
	 ;; unbounded RHS
	 t)
	((or (null a)
	     (eql a '*))
	 ;; unbounded LHS but not unbounded RHS
	 nil)

	(t
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   (<= wa wb)))))


(defsubtype ((signed-byte a) (signed-byte b))
  (if (eql b '*)
      (or (null a)
	  (eql a '*))

      (let ((wa (eval-in-static-environment a))
	    (wb (eval-in-static-environment b)))
	(<= wa wb))))


(defsubtype (unsigned-byte signed-byte)
  t)


(defsubtype ((unsigned-byte a) (signed-byte b))
  (cond ((or (null b)
	     (eql b '*))
	 ;; unbounded RHS
	 t)
	((or (null a)
	     (eql a '*))
	 ;; unbounded LHS but not unbounded RHS
	 nil)

	(t
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   (< wa wb)))))


(deflub (unsigned-byte unsigned-byte)
  'unsigned-byte)
(deflub ((unsigned-byte a) unsigned-byte)
  'unsigned-byte)
(deflub (unsigned-byte (unsigned-byte b))
  'unsigned-byte)


(deflub ((unsigned-byte a) (unsigned-byte b))
  (cond ((or (null b)
	     (eql b '*))
	 ;; unbounded RHS
	 'unsigned-byte)
	((or (null a)
	     (eql a '*))
	 ;; unbounded LHS but not unbounded RHS
	 'unsigned-byte)

	(t
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   `(unsigned-byte ,(max wa wb))))))


(deflub (signed-byte signed-byte)
  'signed-byte)
(deflub ((signed-byte a) signed-byte)
  'signed-byte)
(deflub (signed-byte (signed-byte b))
  'signed-byte)


(deflub ((signed-byte a) (signed-byte b))
  (cond ((or (null b)
	     (eql b '*))
	 ;; unbounded RHS
	 'signed-byte)
	((or (null a)
	     (eql a '*))
	 ;; unbounded LHS but not unbounded RHS
	 'signed-byte)

	(t
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   `(signed-byte ,(max wa wb))))))


(deflub (unsigned-byte signed-byte)
  'signed-byte)
(deflub (signed-byte unsigned-byte)
  'signed-byte)


(deflub ((unsigned-byte a) (signed-byte b))
  (cond ((or (null b)
	     (eql b '*))
	 ;; unbounded RHS
	 t)
	((or (null a)
	     (eql a '*))
	 ;; unbounded LHS but not unbounded RHS
	 t)

	(t
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   (if (< wa wb)
	       ;; unsigned fits into signed
	       `(signed-byte ,wb)

	       ;; expand signed to accommodate unsigned
	       `(signed-byte ,(1+ wa)))))))
(deflub ((signed-byte a) (unsigned-byte b))
  (lub `(unsigned-byte ,b) `(signed-byte ,a)))


;; Simplification rules evaluate the width
(deflub ((unsigned-byte a) nil)
  (let ((w (eval-in-static-environment a)))
    `(unsigned-byte ,w)))
(deflub ((signed-byte a) nil)
  (let ((w (eval-in-static-environment a)))
    `(signed-byte ,w)))


(defmethod bitwidth/form ((tytag (eql 'unsigned-byte)) tyargs)
  (declare (optimize debug))

  (when (or (null tyargs)
	    (eql (car tyargs) '*))
    (error 'not-representable :type (construct-type tytag tyargs)))

  (eval-in-static-environment (car tyargs)))


(defmethod bitwidth/form ((tytag (eql 'signed-byte)) tyargs)
  (when (or (null tyargs)
	    (eql (car tyargs) '*))
    (error 'not-representable :type (construct-type tytag tyargs)))

  (eval-in-static-environment (car tyargs)))


;;; Abbreviations for fixed-width types

(defsubtype (bit (&type ty))
  (subtype-p '(unsigned-byte 1) ty))
(defsubtype ((&type ty) bit)
  (subtype-p ty '(unsigned-byte 1)))
(deflub (bit (&type ty))
  (lub '(unsigned-byte 1) ty))
(deflub ((&type ty) bit)
  (lub ty '(unsigned-byte 1)))

(defmethod bitwidth/form ((tytag (eql 'bit)) tyargs)
  (bitwidth '(unsigned-byte 1)))


;;; Intersections of fixed-width types

(defun width-of-and-of-fixed-width (tys)
  "Return the total bit width of the AND of TYS.

All elements of TYS must reduce to a bounded fixed-width type.
Otherwise, return NIL."
  (declare (optimize debug))

  (let ((ltys (mapcar #'lub tys)))
    (if (every #'fixed-width-p ltys)
	(let ((ws (mapcar #'bitwidth ltys)))
	  (if (every #'identity ws)
	      (apply #'+ ws))))))


(defsubtype ((and &rest tys) (unsigned-byte a))
  (declare (optimize debug))

  (or (not (bounded-fixed-width-p `(unsigned-byte ,a)))
      (<= (width-of-and-of-fixed-width tys) a)))


(defsubtype ((and &rest ltys) (and &rest rtys))
  (<= (width-of-and-of-fixed-width ltys)
      (width-of-and-of-fixed-width rtys)))


(defsubtype ((unsigned-byte a) (and &rest tys))
  (declare (optimize debug))

  (and (bounded-fixed-width-p `(unsigned-byte ,a))
       (<= a (width-of-and-of-fixed-width tys))))


(defsubtype ((and &rest tys) (signed-byte a))
  (declare (optimize debug))

  (or (not (bounded-fixed-width-p `(signed-byte ,a)))
      (<= (width-of-and-of-fixed-width tys) a)))


(defsubtype ((signed-byte a) (and &rest tys))
  (declare (optimize debug))

  (and (bounded-fixed-width-p `(signed-byte ,a))
       (<= a (width-of-and-of-fixed-width tys))))


(deflub ((and &rest tys) (&type ty))
  (declare (optimize debug))

  (let ((ltys (mapcar #'lub tys)))
    (if (every #'fixed-width-p ltys)
	(let ((w (apply #'+ (mapcar #'bitwidth ltys))))
	  (lub `(unsigned-byte ,w) ty))

	(call-next-method))))


(deflub ((&type ty) (and &rest tys))
  (lub `(and ,@tys) ty))


;;; ---------- Tests ----------

(defun fixed-width-p (ty)
  "Test whether TY is a fixed-width type."
  (subtype-p ty '(or unsigned-byte signed-byte)))


(defun bounded-fixed-width-p (ty)
  "Test whether TY is a fixed-width type with a concrete bound."
  (and (fixed-width-p ty)
       (not (or (atom ty)
		(eql (cadr ty) '*)))))


(defun signed-byte-p (ty)
  "Test whether TY is a signed fixed-width type."
  (and (subtype-p ty 'signed-byte)
       (not (unsigned-byte-p ty))))


(defun unsigned-byte-p (ty)
  "Test whether TY is an unsigned fixed-width type."
  (subtype-p ty '(or unsigned-byte bit)))


(defun ensure-fixed-width (ty)
  "Ensure that TY is a fixed-width integer."
  (unless (fixed-width-p ty)
    (warn 'type-mismatch :expected '(unsigned-byte
				     signed-byte
				     bit)
			 :got ty)))


;;; ---------- Widths ----------

(defmethod bitwidth/form ((tytag (eql 'unsigned-byte)) tyargs)
  (if (not (or (null tyargs)
	       (eql (car tyargs) '*)))
      (car tyargs)))


(defmethod bitwidth/form ((tytag (eql 'signed-byte)) tyargs)
  (if (not (or (null tyargs)
	       (eql (car tyargs) '*)))
      (car tyargs)))


(defmethod bitwidth/form ((tytag (eql 'bit)) tyargs)
  1)
