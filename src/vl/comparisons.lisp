;;;; Synthesisable comparison operators
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

(defun ensure-boolean (ty)
  "Ensure TY is a boolean (bit).

Signals TYPE-MISMATCH if TY isn't boolean, which may be ignored
in many applications."
  (ensure-subtype ty 'bit))


;;; ---------- Assertedness ----------

(defmethod compute-type-sexp ((fun (eql 'asserted-p)) args)
  '(unsigned-byte 1))


(defmethod apply-type-constraints-sexp ((fun (eql 'asserted-p)) args)
  (destructuring-bind (v)
      args
    (ensure-fixed-width (compute-type v))))


(defmethod simple-expression-form-p-sexp ((fun (eql 'asserted-p)) args)
  (simple-expression-form-p v))


(defmethod synthesise-sexp ((fun (eql 'asserted-p)) args)
  (destructuring-bind (v)
      args
    (as-literal "(")
    (synthesise v)
    (as-literal " != 0)")))


;;; ---------- Maths ----------

(defmacro define-fixed-width-binary-maths-comparator (symbol &optional verilog-operator)
  "Declare the necessary functions for SYMBOL.

Use VERILOG-OPERATOR if provided for synthesis."
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


     (defmethod simple-expression-form-p-sexp ((fun (eql ',symbol)) args)
       (every #'simple-expression-form-p args))


     (defmethod synthesise-sexp ((fun (eql ',symbol)) args)
       (destructuring-bind (l r)
	   args
	 (as-literal "(")
	 (synthesise l)
	 (as-literal ,(format nil " ~a " verilog-operator))
	 (synthesise r)
	 (as-literal ")")))))


;; equality
(define-fixed-width-binary-maths-comparator = "==")
(define-fixed-width-binary-maths-comparator /= "!=")

;; ordering
(define-fixed-width-binary-maths-comparator <)
(define-fixed-width-binary-maths-comparator <=)
(define-fixed-width-binary-maths-comparator >)
(define-fixed-width-binary-maths-comparator >=)
