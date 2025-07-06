;; Type casting and coercion
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
(declaim (optimize debug))


;; ---------- Type casts ----------

(defmethod typecheck-sexp ((fun (eql 'the)) args)
  (destructuring-bind (ty val)
      args
    (unquote ty)

    (let ((tyval (typecheck val)))
      (ensure-subtype tyval ty)

      ty)))


(defmethod synthesise-sexp ((fun (eql 'the)) args)
  (destructuring-bind (ty val)
      args
    (synthesise val)))


;; ---------- Type coercions ----------

(defmethod typecheck-sexp ((fun (eql 'coerce)) args)
  (destructuring-bind (val ty)
      args
    (unquote ty)

    (let ((vty (typecheck val)))
      (if (and (fixed-width-p ty)
	       (fixed-width-p vty))
	  ;; can coerce fixed-width types
	  (let ((tyw (bitwidth-type (car ty) (cdr ty)))
		(vtyw (bitwidth-type (car vty) (cdr vty))))
	    ty)

	  ;; can't coerce anything else for now
	  (error 'coercion-mismatch :expected ty :got vty
				    :hint "Make sure the two types are coercible.")))))


(defmethod synthesise-sexp ((fun (eql 'coerce)) args)
  (declare (optimize debug))

  (destructuring-bind (val ty)
      args
    (unquote ty)

    (let* ((vty (typecheck val))
	   (tyw (bitwidth-type (car ty) (cdr ty)))
	   (vtyw (bitwidth-type (car vty) (cdr vty))))

      (cond
	;; type are both unsigned
	((and (unsigned-byte-p vty)
	      (unsigned-byte-p ty))
	 (cond ((= vtyw tyw)
		;; types have equal width, synthesise unchanged
		(synthesise val))

	       ((> vtyw tyw)
		;; value is wider, shrink it
		(synthesise `(bref ,val ,(- tyw 1) :end 0)))

	       (t
		;; value is narrower, zero-extend
		(let ((zeros (- tyw vtyw)))
		  (synthesise `(make-bitfields (extend-bits 0 ,zeros)
					       ,val))))))

	;; type are both signed
	((and (signed-byte-p vty)
	      (signed-byte-p ty))
	 (cond ((= vtyw tyw)
		;; types have equal width, leave unchanged
		(synthesise val))

	       ((> vtyw tyw)
		;; value is wider, shrink it by using the same
		;; sign bit and the low-order bits
		(let* ((signbit (1- vtyw))
		       (startbit (- tyw 2)))
		  (synthesise `(make-bitfields (bref ,val ,signbit)
					       (bref ,val ,startbit :end 0)))))

	       (t
		;; value is narrower, sign-extend
		(let ((signbit (1- vtyw))
		      (signs (- tyw vtyw)))
		  (synthesise `(make-bitfields (extend-bits (bref ,val ,signbit) ,signs)
					       ,val))))))

	;; value is unsigned, needed as signed
	((and (unsigned-byte-p vty)
	      (signed-byte-p ty))
	 (cond  ((= vtyw tyw)
		 ;; types have equal width, reduce value and zero-extend
		 (let ((reduced (- tyw 2)))
		   (synthesise `(make-bitfields 0
						(bref ,val ,reduced :end 0)))))

		((> vtyw tyw)
		 ;; value is wider, shrink it
		 (let* ((startbit (- tyw 2)))
		   (synthesise `(make-bitfields 0
						(bref ,val ,startbit :end 0)))))

		(t
		 ;; value is narrower, zero-extend
		 (let ((signs (- tyw vtyw)))
		   (synthesise `(make-bitfields (extend-bits 0 ,signs)
						,val))))))

	;; value is signed, needed as unsigned
	((and (signed-byte-p vty)
	      (unsigned-byte-p ty))
	 (cond ((= vtyw tyw)
		;; types have equal width, reduce value and zero-extend
		(let ((reduced (- tyw 2)))
		  (synthesise `(make-bitfields 0
					       (bref (if (< ,val 0)
							 (- ,val)
							 ,val)
						     ,reduced :end 0)))))

	       ((> vtyw tyw)
		;; value is wider, shrink it
		(let ((reduced (- tyw 2)))
		  (synthesise `(make-bitfields 0
					       (bref (if (< ,val 0)
							 (- ,val)
							 ,val)
						     ,reduced :end 0)))))

	       (t
		;; value is narrower, zero-extend
		(let ((signs (1+ (- tyw vtyw)))
		      (reduced (- vtyw 2)))
		  (synthesise `(make-bitfields (extend-bits 0 ,signs)
					       (bref (if (< ,val 0)
							 (- ,val)
							 ,val)
						     ,reduced :end 0)))))))))))
