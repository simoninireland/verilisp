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

;;; We re-use the Common Lisp fixed-width types like UNSIGNED-BYTE,
;;; but we need more flexible checking and manipulation. Specifically,
;;; we need to assign a type (UNSIGNED-BYTE x) to constants needing x
;;; bits for their representation. (Normal SUBTYPEP will typically
;;; assign a type of (INTEGER 0 *) to a value 2, rather than
;;; (UNSIGNED-BYTE 2) -- although the behaviour isn't actually
;;; constrained to be this.)


;;; ---------- Type algebra ----------

;;; Subtypes take account of the width constraints of types when specified.
;;; These can be expressions that are computed statically.

(defun unbounded-fixed-width-type (ty)
  "Extract the unbounded type from TY.

This essentially strips any bound and returns the unbounded bare type tag."
  (destructuring-bind (tag bounds)
      (deconstruct-type ty)
    (declare (ignore bounds))
    tag))


(defun make-fixed-width-bounded-type (ty bound)
  "Set BOUND as the bound on TY.

If TY has a bound, it is repalced; if it is unbounded, a
bouded version is created."
  (let ((tag (unbounded-fixed-width-type ty)))
    `(,tag ,bound)))


(defun unbounded-bound-p (a)
  "Test whether A is an unbounded bound.

Unbounded bounds are those found in types like UNSIGNED-BYTE
or (SIGNED-BYTE *)."
  (or (null a)
      (eql (safe-car a) '*)))


;; unsigned < unsigned

(defsubtype ((unsigned-byte a) (unsigned-byte b))
  (cond ((unbounded-bound-p b)
	 ;; unbounded RHS, always matches
	 t)
	((unbounded-bound-p a)
	 ;; unbounded LHS but not unbounded RHS, never matches
	 nil)

	(t
	 ;; RHS bound must be at least the LHS bound
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   (<= wa wb)))))


(defsubtype ((unsigned-byte a) (signed-byte b))
  (cond ((unbounded-bound-p b)
	 ;; unbounded RHS, always matches
	 t)
	((unbounded-bound-p a)
	 ;; unbounded LHS but not unbounded RHS, never matches
	 nil)

	(t
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   ;; RHS bound must be strictly larger than the LHS bound
	   (< wa wb)))))


;; signed < signed

(defsubtype ((signed-byte a) (signed-byte b))
  (cond ((unbounded-bound-p b)
	 ;; unbounded RHS, always matches
	 t)
	((unbounded-bound-p a)
	 ;; unbounded LHS but not unbounded RHS, never matches
	 nil)

	(t
	 ;; RHS bound must be at least the LHS bound
	 (let ((wa (eval-in-static-environment a))
	       (wb (eval-in-static-environment b)))
	   (<= wa wb)))))


;; unsigned < signed

(defsubtype (unsigned-byte signed-byte)
  t)


;; signed < unsigned never happens

;; (LUB unsigned unsigned)

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


;; (LUB signed signed)

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


;; (LUB unsigned signed)

(deflub (unsigned-byte signed-byte)
  'signed-byte)
(deflub (unsigned-byte (signed-byte b))
  'signed-byte)
(deflub ((unsigned-byte a) signed-byte)
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


;; (LUB signed unsigned)

(deflub (signed-byte unsigned-byte)
  'signed-byte)
(deflub ((signed-byte a) unsigned-byte)
  'signed-byte)
(deflub (signed-byte (unsigned-byte b))
  'signed-byte)

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

  (when (unbounded-bound-p tyargs)
    (error 'not-representable :type (construct-type tytag tyargs)))

  (eval-in-static-environment (car tyargs)))


(defmethod bitwidth/form ((tytag (eql 'signed-byte)) tyargs)
  (when (unbounded-bound-p tyargs)
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


;; Intersections of fixed-width types
;; TODO: Not sure we want (or need) these

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


;;; ---------- + and * types ----------

;;; These represent the calculations needed to compute the widths of
;;; numbers generated by arithmetic operators, which can't be computed
;;; upfront as they need to have their component types resolved first.
;;;
;;; A * type represents the addition of a sequence of numbers; a * type
;;; represents their multiplication.
;;;
;;; At the moment we only need to deal with integers.

(defun compute-addition-type (tys)
  "Return the type arising from adding values of the types TYS together."
  (declare (optimize debug))

  (let ((n (length tys))
	(lty (apply #'lub tys)))
    (ensure-fixed-width lty)

    (let* ((w (bitwidth lty))
	   (tag (safe-car lty)))

      `(,tag ,(+ w (1- n))))))


(defsubtype ((+ &rest tys) (&type ty))
  (let ((wty (compute-addition-type tys)))
    (subtype-p wty ty)))


(defsubtype ((&type ty) (+ &rest tys))
  (let ((wty (compute-addition-type tys)))
    (subtype-p ty wty)))


(deflub ((+ &rest tys) (&type ty))
  (let ((wty (compute-addition-type tys)))
    (lub wty ty)))


(deflub ((&type ty) (+ &rest tys))
  (lub `(+ ,@tys) ty))


(defun compute-multiplication-type (tys)
  "Return the type arising from multiplying values of the types TYS together."
  (declare (optimize debug))

  (make-fixed-width-bounded-type (lub (safe-car tys))
				 (foldr (lambda (l r)
					  (+ l (bitwidth r)))
					tys 0)))


(defsubtype ((* &rest tys) (&type ty))
  (let ((wty (compute-multiplication-type tys)))
    (subtype-p wty ty)))


(defsubtype ((&type ty) (* &rest tys))
  (let ((wty (compute-multiplication-type tys)))
    (subtype-p ty wty)))


(deflub ((* &rest tys) (&type ty))
  (declare (optimize debug))

  (let ((wty (compute-multiplication-type tys)))
    (break)
    (lub wty ty)))


(deflub ((&type ty) (* &rest tys))
  (lub `(* ,@tys) ty))


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
    (warn 'type-mismatch :expected "fixed-width type"
			 :got ty)))


(defun bounded-fixed-width-p (ty)
  "Test whether TY is a fixed-width type with a concrete bound.

Bounded types are representable."
  (and (fixed-width-p ty)
       (destructuring-bind (tag bound)
	   (deconstruct-type ty)
	 (declare (ignore tag))
	 (not (unbounded-bound-p bound)))))


(defun unbounded-fixed-width-p (ty)
  "Test whether TY is an unbounded fixed-width type.

Unbounded types aren't representable."
  (and (fixed-width-p ty)
       (destructuring-bind (tag bound)
	   (deconstruct-type ty)
	 (declare (ignore tag))
	 (unbounded-bound-p bound))))


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
