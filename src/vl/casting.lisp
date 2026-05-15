;;;; Type casting and coercion
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
(declaim (optimize debug))

;;; Type management is rather more important in Verilisp than it it in
;;; Common Lisp, because we need to synthesise explisit widths for
;;; variables. This makes COERCE in particular more important, because
;;; we use the signed-ness of a variable to drive the way things are
;;; synthesised.


;;; ---------- Type casts ----------

(defpassmethod compute-type (the ty val)
  (unquote ty)

  (compute-type val)

  ;; the type we assume is the type we're asserting
  ty)


(defpassmethod apply-type-constraints (the ty val)
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

	   ty))))


(defpassmethod read-variables (the ty val)
  (read-variables val))


(defpassmethod simple-expression-form-p (the &rest args)
  t)


(defpassmethod synthesise (the ty val)
  (unquote ty)

  ;; (if (and (signed-byte-p ty)
  ;;	     (not (unsigned-byte-p ty)))
  ;;	(progn
  ;;	  (as-literal "$signed(")
  ;;	  (synthesise val)
  ;;	  (as-literal ")"))
  ;;	(synthesise val))
  (synthesise val)
  )


;;; ---------- Type coercions ----------

(defpassmethod compute-type (coerce val ty)
  (unquote ty)

  (compute-type val)

  ;; the type of the coercion is the type we're coercing to
  ty)


(defpassmethod apply-type-constraints (coerce val ty)
  (unquote ty)

  ;; check we can do the coercion
  (let ((vty (compute-type val)))
    (if (not (and (fixed-width-p ty)
		  (fixed-width-p vty)))
	;; can't coerce anything else for now
	(error 'coercion-mismatch :expected ty :got vty
				  :hint "Make sure the two types are coercible."))))


(defpassmethod read-variables (coerce val ty)
  (read-variables val))


(defpassmethod simple-expression-form-p (coerce &rest args)
  t)


;;; We transform COERCE forms away before synthesis.
;;; This will need to change if and when we broaden the scope of
;;; what can be coerced.

(defpass transform-coerce-to-bitfields (form)
  (:documentation "Transform a COERCE form into bitfields.

The main use of COERCE is change the widths and/or signs of variables.
We do this directly using MAKE-BITFIELDS, which works becase all the
elements need to be statically constant.")
  (:queue transforming)
  (:schema into-arguments)

  (:method (form)
    form))


;;; We can make use of some type constraints here. Either:
;;;
;;; - the start bit is a variable and the result has width 1; or
;;; - a width (or end) is specified and is constant, and so is
;;;   the start bit
;;;
;;; These constraints let us expand nested constructs somewhat

(defpassmethod transform-coerce-to-bitfields (coerce val ty)
  (declare (optimize debug))

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
	      ;; types have equal width, leave unchanged
	      val)

	     ((> vtyw tyw)
	      ;; value is wider, shrink it
	      (if (> vtyw 1)
		  `(bref ,val ,(- tyw 1) :end 0)
		  val))

	     (t
	      ;; value is narrower, zero-extend
	      (if (> vtyw 1)
		  `(make-bitfields (extend-bits 0 ,(- tyw vtyw))
				   (bref ,val ,(- vtyw 1) :end 0))
		  `(make-bitfields (extend-bits 0 ,(- tyw vtyw))
				   ,val)))))

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
	      `(make-bitfields (bref ,val ,(1- vtyw))
			       (bref ,val , (- tyw 2) :end 0)))

	     (t
	      ;; value is narrower, sign-extend
	      `(make-bitfields (extend-bits (bref ,val ,(1- vtyw)) ,(- tyw vtyw))
			       (bref ,val ,(- vtyw 1) :end 0)))))

      ;; value is unsigned, needed as signed
      ((and (unsigned-byte-p vty)
	    (signed-byte-p ty))
       (cond ((or (null tyw)
		  (= vtyw tyw))
	      ;; types have equal width, reduce value and zero-extend
	      `(make-bitfields 0
			       (bref ,val ,(- tyw 1) :end 0)))

	     ((> vtyw tyw)
	      ;; value is wider, shrink it
	      `(make-bitfields 0
			       (bref ,val ,(- tyw 2) :end 0)))

	     (t
	      ;; value is narrower, zero-extend
	      `(make-bitfields (extend-bits 0 , (- tyw vtyw))
			       (bref ,val ,(- vtyw 1) :end 0)))))

      ;; value is signed, needed as unsigned
      ((and (signed-byte-p vty)
	    (unsigned-byte-p ty))
       (cond ((= vtyw tyw)
	      ;; types have equal width, reduce value and zero-extend
	      `(make-bitfields 0
			       (if (< ,val 0)
				   (+ (lognot (bref a ,(- tyw 2) :end 0)) 1)
				   (bref a ,(- tyw 2) :end 0))))

	     ((> vtyw tyw)
	      ;; value is wider, shrink it
	      `(make-bitfields 0
			       (if (< ,val 0)
				   (+ (lognot (bref a ,(- tyw 2) :end 0)) 1)
				   (bref a ,(- tyw 2) :end 0))))

	     (t
	      ;; value is narrower, zero-extend
	      `(make-bitfields (extend-bits 0 ,(- tyw vtyw))
			       (if (< ,val 0)
				   (+ (lognot (bref a ,(- vtyw 2) :end 0)) 1)
				   (bref a ,(- vtyw 2) :end 0))))))

      (t
       (error 'not-synthesisable :hint (format nil "Something is weird with types ~s and ~s" ty vty))))))


;;; Don't enter the special forms
;;; TODO: There should be a way of making this generic across passes?

(defun transform-coerce-let (fun args)
  "Transform COERCE forms in LET or LET*."
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    ;; grab the decls ahead of being rebound
    (with-local-frame decls
      ;; rewrite body and initial values (in decls and env)
      (let ((tdecls (mapcar (lambda (decl)
			      (if (or (atom decl)
				      (null (cadr decl)))
				  ;; naked value remains the same
				  decl

				  ;; initial value is recursed into
				  (destructuring-bind (n v)
				      decl
				    (let ((tv (transform-coerce-to-bitfields v)))
				      ;; change the initial value in the environment
				      (set-variable-property n 'initial-value tv)

				      ;; return the new decl
				      (list n tv)))))
			    decls))
	    (tbody (mapcar #'transform-coerce-to-bitfields body)))

	;; re-write to the use the new decls, frane, and body
	(appendf decls (list (list 'local-frame (current-frame))))
	`(,fun ,decls
	       ,@tbody)))))


(defpassmethod transform-coerce-to-bitfields (let &rest args)
  (transform-coerce-let 'let args))


(defpassmethod transform-coerce-to-bitfields (let* &rest args)
  (transform-coerce-let 'let* args))


(defpassmethod transform-coerce-to-bitfields (module modname decls &rest body)
  (let ((tbody (mapcar #'transform-coerce-to-bitfields body)))
    `(module ,modname ,decls ,@tbody)))
