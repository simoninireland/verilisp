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

(in-package :verilisp/core)


;; ---------- Fixed-width integers ----------

(defmethod subtype-type ((ty1tag (eql 'unsigned-byte)) ty1args
			 (ty2tag (eql 'unsigned-byte)) ty2args)
  (let ((ty1w (bitwidth (construct-type ty1tag ty1args)))
	(ty2w (bitwidth (construct-type ty2tag ty2args))))
    (cond  ((null ty1w)
	    (null ty2w))
	   ((null ty2w)
	    t)
	   (t
	    (<= ty1w ty2w)))))


(defmethod subtype-type ((ty1tag (eql 'signed-byte)) ty1args
			 (ty2tag (eql 'signed-byte)) ty2args)
  (let ((ty1w (bitwidth (construct-type ty1tag ty1args)))
	(ty2w (bitwidth (construct-type ty2tag ty2args))))
    (cond  ((null ty1w)
	    (null ty2w))
	   ((null ty2w)
	    t)
	   (t
	    (<= ty1w ty2w)))))


(defmethod subtype-type ((ty1tag (eql 'unsigned-byte)) ty1args
			 (ty2tag (eql 'signed-byte)) ty2args)
  (let ((ty1w (bitwidth (construct-type ty1tag ty1args)))
	(ty2w (bitwidth (construct-type ty2tag ty2args))))
    (cond  ((null ty1w)
	    (null ty2w))
	   ((null ty2w)
	    t)
	   (t
	    (> ty2w ty1w)))))


(defmethod subtype-type ((ty1tag (eql 'bit)) ty1args
			 ty2tag ty2args)
  (subtype-p '(unsigned-byte 1) (construct-type ty2tag ty2args)))


(defmethod subtype-type (ty1tag ty1args
			 (ty2tag (eql 'bit)) ty2args)
  (subtype-p (construct-type ty1tag ty1args) '(unsigned-byte 1)))


(defun fixed-width-p (ty)
  "Test whether TY is a fixed-width type."
  (subtype-p ty '(or unsigned-byte signed-byte)))


(defun signed-byte-p (ty)
  "Test whether TY is a signed fixed-width type."
  (subtype-p ty 'signed-byte))


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


;; ---------- Representability ----------

;; Fixed-width types are representable if they have positive widths

(defmethod representable-type-sexp-p ((tytag (eql 'unsigned-byte)) tyargs)
  (let ((w (eval-in-static-environment (car tyargs))))
    (> w 0)))


(defmethod representable-type-sexp-p ((tytag (eql 'bit)) tyargs)
  t)


(defmethod representable-type-sexp-p ((tytag (eql 'signed-byte)) tyargs)
  (let ((w (eval-in-static-environment (car tyargs))))
    (> w 0)))


;; ---------- Least upper-bound ----------

(defmethod lub-type ((ty1tag (eql 'unsigned-byte)) ty1args
		     (ty2tag (eql 'unsigned-byte)) ty2args)
  (let ((ty1w (bitwidth (construct-type ty1tag ty1args)))
	(ty2w (bitwidth (construct-type ty2tag ty2args))))
    (if  (or (null ty1w)
	     (null ty2w))
	 'unsigned-byte

	 `(unsigned-byte ,(max ty1w ty2w)))))


(defmethod lub-type ((ty1tag (eql 'signed-byte)) ty1args
		     (ty2tag (eql 'signed-byte)) ty2args)
  (let ((ty1w (bitwidth (construct-type ty1tag ty1args)))
	(ty2w (bitwidth (construct-type ty2tag ty2args))))
    (if  (or (null ty1w)
	     (null ty2w))
	 'signed-byte

	 `(signed-byte ,(max ty1w ty2w)))))


(defmethod lub-type ((ty1tag (eql 'unsigned-byte)) ty1args
		     (ty2tag (eql 'signed-byte)) ty2args)
  (let ((ty1w (bitwidth (construct-type ty1tag ty1args)))
	(ty2w (bitwidth (construct-type ty2tag ty2args))))
    (cond ((and (null ty1w)
		(null ty2w))
	   'signed-byte)

	  ((null ty1w)
	   t)

	  ((null ty2w)
	   'signed-byte)

	  (t
	   (if (< ty1w ty2w)
	       ;; unsigned fits into signed
	       `(signed-byte ,ty2w)

	       ;; expand signed to accommodate unsigned
	       `(signed-byte ,(1+ ty1w)))))))
(defmethod lub-type ((ty1tag (eql 'signed-byte)) ty1args
		     (ty2tag (eql 'unsigned-byte)) ty2args)
  (lub (construct-type ty2tag ty2args)
       (construct-type ty1tag ty1args)))


(defmethod lub-type ((ty1tag (eql 'bit)) ty1args
		     ty2tag ty2args)
  (lub '(unsigned-byte 1) (construct-type ty2tag ty2args)))
(defmethod lub-type (ty1tag ty1args
		     (tytag2 (eql 'bit)) ty2args)
  (lub (construct-type ty1tag ty1args) '(unsigned-byte 1)))


(defmethod lub-type((ty1tag (eql 'or)) ty1args ty2tag ty2args)
  (if (every #'fixed-width-p ty1args)
      ;; the union of fixed-width types is the maximum of their widths
      (let* ((w (apply #'max (mapcar (compose #'bitwidth #'eval-type) ty1args)))
	     (tyu `(,(if (every (rcurry #'subtype-p 'unsigned-byte) ty1args)
		      'unsigned-byte
		      'signed-byte)
		    ,w)))
	(lub tyu (construct-type ty2tag ty2args)))

      ;; otherwise fall through
      (call-next-method)))


(defmethod lub-type((ty1tag (eql 'and)) ty1args ty2tag ty2args)
  (if (every #'fixed-width-p ty1args)
      ;; the intersection of fixed-width types is the sum of their widths
      (let* ((w (apply #'+ (mapcar (compose #'bitwidth #'eval-type) ty1args)))
	     (tyu `(,(if (every (rcurry #'subtype-p 'unsigned-byte) ty1args)
			 'unsigned-byte
			 'signed-byte)
		    ,w)))
	(lub tyu (construct-type ty2tag ty2args)))

      ;; otherwise fall through
      (call-next-method)))


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
  (eval-in-static-environment (car tyargs)))


(defmethod bitwidth-type ((tytag (eql 'signed-byte)) tyargs)
  (eval-in-static-environment (car tyargs)))


(defmethod bitwidth-type ((tytag (eql 'bit)) tyargs)
  1)


(defmethod eval-type-type ((tytag (eql 'unsigned-byte)) tyargs)
  (let ((w (bitwidth (construct-type tytag tyargs))))
    (if (null w)
	'unsigned-byte
	`(unsigned-byte ,w))))


(defmethod eval-type-type ((tytag (eql 'signed-byte)) tyargs)
  (let ((w (bitwidth (construct-type tytag tyargs))))
    (if (null w)
	'signed-byte
	`(signed-byte ,w))))
