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

(defmethod compute-type-sexp ((fun (eql 'the)) args)
  (destructuring-bind (ty val)
      args
    (unquote ty)

    (compute-type val)

    ;; the type we assume is the type we're asserting
    ty))


(defmethod apply-type-constraints-sexp ((fun (eql 'the)) args)
  (destructuring-bind (ty val)
      args
    (unquote ty)

    (let ((tyval (compute-type val)))
      (cond ((eql ty t)
	     ;; casting to top does nothing
	     tyval)

	    ((null ty)
	     ;; casting to nil can't possibly succeed
	     (error 'type-mismatch :expected "a type"
				   :got nil
				   :hint "Casting to the empty type can't succeed"))

	    (t
	     ;; check the cast makes sense
	     (ensure-subtype tyval ty)

	     ty)))))


(defmethod read-variables-sexp ((fun (eql 'the)) args)
  (destructuring-bind (ty val)
      args
    (read-variables val)))


(defmethod synthesise-sexp ((fun (eql 'the)) args)
  (destructuring-bind (ty val)
      args
    (unquote ty)

    ;; (if (and (signed-byte-p ty)
    ;;	     (not (unsigned-byte-p ty)))
    ;;	(progn
    ;;	  (as-literal "$signed(")
    ;;	  (synthesise val)
    ;;	  (as-literal ")"))
    ;;	(synthesise val))
    (synthesise val)
    ))


;; ---------- Type coercions ----------

(defmethod compute-type-sexp ((fun (eql 'coerce)) args)
  (destructuring-bind (val ty)
      args
    (unquote ty)

    (compute-type val)

    ;; the type of the coercion is the type we're coercing to
    ty))


(defmethod apply-type-constraints-sexp ((fun (eql 'coerce)) args)
  (destructuring-bind (val ty)
      args
    (unquote ty)

    ;; check we can do the coercion
    (let ((vty (compute-type val)))
      (if (not (and (fixed-width-p ty)
		    (fixed-width-p vty)))
	  ;; can't coerce anything else for now
	  (error 'coercion-mismatch :expected ty :got vty
				    :hint "Make sure the two types are coercible.")))))


(defmethod read-variables-sexp ((fun (eql 'coerce)) args)
  (destructuring-bind (val ty)
      args
    (read-variables val)))


;; We can make use of some type constraints here. Either:
;;
;; - the start bit is a variable and the result has width 1; or
;; - a width (or end) is specified and is constant, and so is
;;   the start bit
;;
;; These constraints let us expand nested constructs somewhat

(defmethod synthesise-sexp ((fun (eql 'coerce)) args)
  (declare (optimize debug))

  (destructuring-bind (val ty)
      args
    (unquote ty)

    (let* ((vty (compute-type val))
	   (tyw (bitwidth (lub ty)))
	   (vtyw (bitwidth (lub vty))))

      (cond
	;; type are both unsigned
	((and (unsigned-byte-p vty)
	      (unsigned-byte-p ty))
	 (cond ((or (null tyw)
		    (= vtyw tyw))
		;; types have equal width, synthesise unchanged
		(synthesise val))

	       ((> vtyw tyw)
		;; value is wider, shrink it
		(if (> vtyw 1)
		    (synthesise `(bref ,val ,(- tyw 1) :end 0))
		    (synthesise val)))

	       (t
		;; value is narrower, zero-extend
		(if (> vtyw 1)
		    (synthesise `(make-bitfields (extend-bits 0 ,(- tyw vtyw))
						 (bref ,val ,(- vtyw 1) :end 0)))
		    (synthesise `(make-bitfields (extend-bits 0 ,(- tyw vtyw))
						 ,val))))))

	;; type are both signed
	((and (signed-byte-p vty)
	      (signed-byte-p ty))
	 (cond ((or (null tyw)
		    (= vtyw tyw))
		;; types have equal width, leave unchanged
		(synthesise val))

	       ((> vtyw tyw)
		;; value is wider, shrink it by using the same
		;; sign bit and the low-order bits
		(synthesise `(make-bitfields (bref ,val ,(1- vtyw))
					     (bref ,val , (- tyw 2) :end 0))))

	       (t
		;; value is narrower, sign-extend
		(synthesise `(make-bitfields (extend-bits (bref ,val ,(1- vtyw)) ,(- tyw vtyw))
					     (bref ,val ,(- vtyw 1) :end 0))))))

	;; value is unsigned, needed as signed
	((and (unsigned-byte-p vty)
	      (signed-byte-p ty))
	 (cond ((or (null tyw)
		    (= vtyw tyw))
		;; types have equal width, reduce value and zero-extend
		(synthesise `(make-bitfields 0
					     (bref ,val ,(- tyw 1) :end 0))))

	       ((> vtyw tyw)
		;; value is wider, shrink it
		(synthesise `(make-bitfields 0
					     (bref ,val ,(- tyw 2) :end 0))))

	       (t
		;; value is narrower, zero-extend
		(synthesise `(make-bitfields (extend-bits 0 , (- tyw vtyw))
					     (bref ,val ,(- vtyw 1) :end 0))))))

	;; value is signed, needed as unsigned
	((and (signed-byte-p vty)
	      (unsigned-byte-p ty))
	 (cond ((= vtyw tyw)
		;; types have equal width, reduce value and zero-extend
		(synthesise `(make-bitfields 0
					     (if (< ,val 0)
						 (+ (lognot (bref a ,(- tyw 2) :end 0)) 1)
						 (bref a ,(- tyw 2) :end 0)))))

	       ((> vtyw tyw)
		;; value is wider, shrink it
		(synthesise `(make-bitfields 0
					     (if (< ,val 0)
						 (+ (lognot (bref a ,(- tyw 2) :end 0)) 1)
						 (bref a ,(- tyw 2) :end 0)))))

	       (t
		;; value is narrower, zero-extend
		(synthesise `(make-bitfields (extend-bits 0 ,(- tyw vtyw))
					     (if (< ,val 0)
						 (+ (lognot (bref a ,(- vtyw 2) :end 0)) 1)
						 (bref a ,(- vtyw 2) :end 0)))))))))))
