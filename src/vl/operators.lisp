;; Synthesisable operators
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


;; ---------- Helper ----------

(defun ensure-number-of-arguments (fun args n)
  "Ensure that ARGS has exactly N arguments.

A NOT-SYNTHESISABLE error is raised if the arguments are wrong."
   (if (/= (length args) n)
      (error 'not-synthesisable :hint (format nil "Operator needs exactly ~a arguments" n))))


;; ---------- Maths ----------

(defun compute-type-addition (args)
  "Compute the type of an addition or subtraction of ARGS."
  (let ((tys (mapcar #'compute-type args))
	(n (1- (length args))))

    ;; type is the LUB of the arguments plus the
    ;; extra bits required for carries between the additions
    `(and (or ,@tys) (unsigned-byte ,n))))


(defmethod apply-type-constraints-addition (args)
  (let ((tys (mapcar #'compute-type args)))
    (dolist (ty tys)
      (ensure-fixed-width ty))))


(defun fold-constant-expressions-addition (fun args)
  "Fold expressions in addition operator FUN applied to ARGS."
  (labels ((fold-constants (total l)
	     (if (null l)
		 (list total '())
		 (let ((v (car l)))
		   (if (integerp v)
		       ;; fold-in the constant, reduce the rest
		       (let ((s (apply fun (list v total))))
			 (fold-constants s (cdr l)))

		       ;; reduce the rest with the non-constant on the front
		       (destructuring-bind (ct cl)
			   (fold-constants total (cdr l))
			 (list ct (cons v cl))))))))

    (destructuring-bind (total remaining)
	(fold-constants 0 args)
      (if (null remaining)
	  total
	  (if (= total 0)
	      `(,fun ,@remaining)
	      `(,fun ,total ,@remaining ))))))


(defmethod compute-type-sexp ((fun (eql '+)) args)
  (compute-type-addition args))


(defmethod apply-type-constraints-sexp ((fun (eql '+)) args)
  (apply-type-constraints-addition args))


(defmethod fold-constant-expressions-sexp ((fun (eql '+)) args)
  (fold-constant-expressions-addition '+ args))


(defmethod synthesise-sexp ((fun (eql '+)) args)
  (as-infix '+ args))


(defmethod lispify-sexp ((fun (eql '+)) args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(+ ,@vals)))


(defmethod compute-type-sexp ((fun (eql '-)) args)
  (if (= (length args) 1)
      ;; unary negation
      (let ((ty (compute-type (car args))))
	`(signed-byte (1+ (bitwidth ',ty))))

      ;; general substraction
      ;; we force subtractions to be signed
      (let ((ty (compute-type-addition args)))
	`(signed-byte (bitwidth ',ty)))))


(defmethod apply-type-constraints-sexp ((fun (eql '-)) args)
  (apply-type-constraints-addition args))


(defmethod fold-constant-expressions-sexp ((fun (eql '-)) args)
  (fold-constant-expressions-addition '- args))


(defmethod synthesise-sexp ((fun (eql '-)) args)
  (if (= (length args) 1)
      ;; unary minus
      (progn
	(as-literal "(- ")
	(synthesise (car args))
	(as-literal ")"))

      ;; application
      (as-infix '- args)))


(defmethod synthesise-sexp ((fun (eql '*)) args)
  (as-infix '* args))


(defmethod lispify-sexp ((fun (eql '-)) args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(- ,@vals)))


;; ---------- Shifts ----------

;; Verilog provides left and right shift operators; Common Lisp uses ash
;; and switches depending on the sign of the shift (negative for right).
;; That behaviour seems impossible to synthesise without using an extra
;; register, so we provide two different operators instead. (This will
;; change if I can figure out a way to synthesise ash.)
;;
;; The destructuring is to ensure there are only two arguments.
;;
;; The result is always unsigned.

(defmethod compute-type-sexp ((fun (eql '<<)) args)
  (ensure-number-of-arguments fun args 2)

  (destructuring-bind (val offset)
      args
    (let ((tyval (compute-type val))
	  (tyoffset (compute-type offset)))
      (ensure-fixed-width tyval)
      (ensure-fixed-width tyoffset)

      ;; the width is the width of the value plus the
      ;; maximum number that can be in the offset
      `(unsigned-byte ,(+ (bitwidth tyval)
			  (1- (ash 1 (bitwidth tyoffset))))))))


(defmethod fold-constant-expressions-sexp ((fun (eql '<<)) args)
  (destructuring-bind (val offset)
      args
    (if (and (integerp val)
	     (integerp offset))
	(ash val offset)
	`('<< ,val ,offset))))


(defmethod synthesise-sexp ((fun (eql '<<)) args)
  (as-infix '<< args))


(defmethod lispify-sexp ((fun (eql '<<)) args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(ash ,@vals)))


;; We should probably do sign extension here, like Lisp does

(defmethod compute-type-sexp ((fun (eql '>>)) args)
  (ensure-number-of-arguments fun args 2)

  (destructuring-bind (val offset)
      args
    (let ((tyval (compute-type val))
	  (tyoffset (compute-type offset)))
      (ensure-fixed-width tyval)
      (ensure-fixed-width tyoffset)

      ;; use the width of the value as the maximum width, since
      ;; shifting right can only make it smaller
      `(unsigned-byte ,(bitwidth tyval)))))


(defmethod fold-constant-expressions-sexp ((fun (eql '>>)) args)
  (destructuring-bind (val offset)
      args
    (if (and (integerp val)
	     (integerp offset))
	(ash val (- offset))
	`('<< ,val ,offset))))


(defmethod synthesise-sexp ((fun (eql '>>)) args)
  (as-infix '>> args))


(defmethod lispify-sexp ((fun (eql '>>)) args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(ash ,(car vals) (- ,(cadr vals)))))


;; ---------- Bitwise operators ----------

(defun typecheck-bitwise-operator (args)
  "Typecheck the arguments ARGS to a logical operator."
  (let ((ty (foldr (lambda (ty1 arg)
		     (lub ty1 (compute-type arg)))
		   args nil)))
    (ensure-subtype ty 'unsigned-byte)

    ty))


(defmethod compute-type-sexp ((fun (eql 'logand)) args)
  (typecheck-bitwise-operator args))


(defmethod fold-constant-expressions-sexp ((fun (eql 'logand)) args)
  (fold-constant-expressions-addition 'logand args))


(defmethod synthesise-sexp ((fun (eql 'logand)) args)
  (as-infix '& args))


(defmethod compute-type-sexp ((fun (eql 'logior)) args)
  (typecheck-bitwise-operator args))


(defmethod fold-constant-expressions-sexp ((fun (eql 'logior)) args)
  (fold-constant-expressions-addition 'logior args))


(defmethod synthesise-sexp ((fun (eql 'logior)) args)
  (as-infix '|\|| args))


(defmethod compute-type-sexp ((fun (eql 'logxor)) args)
  (typecheck-bitwise-operator args))


(defmethod fold-constant-expressions-sexp ((fun (eql 'logxor)) args)
  (fold-constant-expressions-addition 'logxor args))


(defmethod synthesise-sexp ((fun (eql 'logxor)) args)
  (as-infix '^ args))


;; ---------- Logical ----------

(defun typecheck-logical-operator (args)
  "Typecheck the arguments ARGS to a logical operator.

All arguments must be booleans."
  (mapc (lambda (arg)
	  (let ((ty (compute-type arg)))
	    (ensure-boolean ty)))
	args)
  '(unsigned-byte 1))


(defmethod compute-type-sexp ((fun (eql 'and)) args)
  (typecheck-logical-operator args))


(defmethod synthesise-sexp ((fun (eql 'and)) args)
  (as-infix '&& args))


(defmethod compute-type-sexp ((fun (eql 'or)) args)
  (typecheck-logical-operator args))


(defmethod synthesise-sexp ((fun (eql 'or)) args)
  (as-infix '|\|\|| args))


(defmethod compute-type-sexp ((fun (eql 'not)) args)
  (ensure-number-of-arguments 'not args 1)
  (ensure-boolean (car args))
  '(unsigned-byte 1))


(defmethod synthesise-sexp ((fun (eql 'not)) args)
  (as-literal "(")
  (as-literal "!")
  (synthesise (car args))
  (as-literal ")"))
