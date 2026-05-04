;; Synthesisable operators
;;
;; Copyright (C) 2024--2026 Simon Dobson
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


;; ---------- Helpers ----------

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


(defmethod compute-type-sexp ((fun (eql '+)) args)
  (compute-type-addition args))


(defmethod apply-type-constraints-sexp ((fun (eql '+)) args)
  (apply-type-constraints-addition args))


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

      ;; general subtraction
      ;; we force subtractions to be signed
      (let ((ty (compute-type-addition args)))
	`(signed-byte (bitwidth ',ty)))))


(defmethod apply-type-constraints-sexp ((fun (eql '-)) args)
  (apply-type-constraints-addition args))


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
;; The right shift (>>) operator behaves like ash in that it does
;; sign extension automat6ically based on the type of the value. This
;; means that Verilog's >>> (arithmetic shoft right) is generated implicitly
;; by type, rather than being provided explicitly.

(defmethod compute-type-sexp ((fun (eql '<<)) args)
  (declare (optimize debug))

  (ensure-number-of-arguments fun args 2)

  (destructuring-bind (val offset)
      args
    (let ((tyval (compute-type val))
	  (tyoffset (compute-type offset)))

      ;; the width is the width of the value plus the
      ;; maximum number that can be in the offset
      `(and ,tyval (unsigned-byte (bitwidth ',tyoffset))))))


(defmethod synthesise-sexp ((fun (eql '<<)) args)
  (as-infix '<< args))


(defmethod lispify-sexp ((fun (eql '<<)) args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(ash ,@vals)))


(defmethod compute-type-sexp ((fun (eql '>>)) args)
  (ensure-number-of-arguments fun args 2)

  (destructuring-bind (val offset)
      args
    (let ((tyval (compute-type val))
	  (tyoffset (compute-type offset)))

      ;; type is the same as the value, since
      ;; shifting right can only make it smaller
      tyval)))


(defmethod synthesise-sexp ((fun (eql '>>)) args)
  (destructuring-bind (val offset)
      args

    (let ((ty (compute-type val)))
      (if (signed-byte-p ty)
	  ;; value is signed, do arithmetic shift
	  (as-infix '>>> args)

	  ;; value is unsigned, do logical shift
	  (as-infix '>> args)))))


(defmethod lispify-sexp ((fun (eql '>>)) args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(ash ,(car vals) (- ,(cadr vals)))))


;; ---------- Bitwise operators ----------

(defmacro define-fixed-width-binary-bitwise-operator (symbol &optional verilog-operator)
  "Declare the necessary functions for SYMBOL.

Use VERILOG-OPERATOR if provided for synthesis; otherwise use SYMBOL."
  (unless verilog-operator
    (setq verilog-operator symbol))

  `(progn
     (defmethod compute-type-sexp ((fun (eql ',symbol)) args)
       '(unsigned-byte 1))


     (defmethod apply-type-constraints-sexp ((fun (eql ',symbol)) args)
       (destructuring-bind (l r)
	   args
	 (ensure-fixed-width (compute-type l))
	 (ensure-fixed-width (compute-type r))))


     (defmethod synthesise-sexp ((fun (eql ',symbol)) args)
       (destructuring-bind (l r)
	   args
	 (as-literal "(")
	 (synthesise l)
	 (as-literal ,(format nil " ~a " verilog-operator))
	 (synthesise r)
	 (as-literal ")")))))

(define-fixed-width-binary-bitwise-operator logand "&")
(define-fixed-width-binary-bitwise-operator logior "|")
(define-fixed-width-binary-bitwise-operator logxor "^")


(defmacro define-fixed-width-unary-bitwise-operator (symbol &optional verilog-operator)
  "Declare the necessary functions for SYMBOL.

Use VERILOG-OPERATOR if provided for synthesis; otherwise use SYMBOL."
  (unless verilog-operator
    (setq verilog-operator symbol))

  `(progn
     (defmethod compute-type-sexp ((fun (eql ',symbol)) args)
       '(unsigned-byte 1))


     (defmethod apply-type-constraints-sexp ((fun (eql ',symbol)) args)
       (destructuring-bind (l)
	   args
	 (ensure-fixed-width (compute-type l))))


      (defmethod synthesise-sexp ((fun (eql ',symbol)) args)
	(destructuring-bind (l)
	    args
	  (as-literal "(")
	  (as-literal ,(format nil "~a " verilog-operator))
	  (synthesise l)
	  (as-literal ")")))))

(define-fixed-width-unary-bitwise-operator lognot "~")


;; ---------- Logical ----------

(defmacro define-fixed-width-nary-logical-operator (symbol &optional verilog-operator)
  "Declare the necessary functions for SYMBOL.

Use VERILOG-OPERATOR if provided for synthesis; otherwise use SYMBOL."
  (unless verilog-operator
    (setq verilog-operator symbol))

  `(progn
     (defmethod compute-type-sexp ((fun (eql ',symbol)) args)
       '(unsigned-byte 1))


     (defmethod apply-type-constraints-sexp ((fun (eql ',symbol)) args)
       (dolist (a args)
	 (ensure-boolean (compute-type a))))


     (defmethod synthesise-sexp ((fun (eql ',symbol)) args)
       (as-infix ,verilog-operator args))))

(define-fixed-width-nary-logical-operator and "&&")
(define-fixed-width-nary-logical-operator or "||")


(defmacro define-fixed-width-unary-logical-operator (symbol &optional verilog-operator)
  "Declare the necessary functions for SYMBOL.

Use VERILOG-OPERATOR if provided for synthesis."
  (unless verilog-operator
    (setq verilog-operator symbol))

  `(progn
     (defmethod compute-type-sexp ((fun (eql ',symbol)) args)
       '(unsigned-byte 1))


     (defmethod apply-type-constraints-sexp ((fun (eql ',symbol)) args)
       (destructuring-bind (v)
	   args
	 (ensure-boolean (compute-type v))))


     (defmethod synthesise-sexp ((fun (eql ',symbol)) args)
       (destructuring-bind (v)
	   args
	 (as-literal "(")
	 (as-literal ,(format nil " ~a" verilog-operator))
	 (as-literal "(")
	 (synthesise v)
	 (as-literal ")")
	 (as-literal ")")))))

(define-fixed-width-unary-logical-operator not "!")
