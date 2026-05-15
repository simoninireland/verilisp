;;;; Synthesisable operators
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


;;; ---------- Helpers ----------

(defun ensure-number-of-arguments (fun args n)
  "Ensure that ARGS has exactly N arguments.

A NOT-SYNTHESISABLE error is raised if the arguments are wrong."
   (if (/= (length args) n)
      (error 'not-synthesisable :hint (format nil "Operator needs exactly ~a arguments" n))))


;;; ---------- Addition-like operators ----------

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


;;; +

(defpassmethod compute-type (+ &rest args)
  (compute-type-addition args))


(defpassmethod apply-type-constraints (+ &rest args)
  (apply-type-constraints-addition args))


(defpassmethod simple-expression-form-p (+ &rest args)
  (every #'simple-expression-form-p args))


(defpassmethod synthesise (+ &rest args)
  (as-infix '+ args))


;;; -

(defpassmethod compute-type (- &rest args)
  (if (= (length args) 1)
      ;; unary negation
      (let ((ty (compute-type (car args))))
	`(signed-byte (1+ (bitwidth ',ty))))

      ;; general subtraction
      ;; we force subtractions to be signed
      (let ((ty (compute-type-addition args)))
	`(signed-byte (bitwidth ',ty)))))


(defpassmethod apply-type-constraints (- &rest args)
  (:same-as +))


(defpassmethod simple-expression-form-p (- &rest args)
  (:same-as +))


(defpassmethod synthesise (- &rest args)
  (if (= (length args) 1)
      ;;;; unary minus
      (progn
	(as-literal "(- ")
	(synthesise (car args))
	(as-literal ")"))

      ;;;; application
      (as-infix '- args)))


;;; *

(defpassmethod compute-type (* &rest args)
  (:same-as +))

(defpassmethod apply-type-constraints (* &rest args)
  (:same-as +))


(defpassmethod simple-expression-form-p (* &rest args)
  (:same-as +))


(defpassmethod synthesise (* &rest args)
  (as-infix '* args))


;;; ---------- Shifts ----------

;;; Verilog provides left and right shift operators; Common Lisp uses ash
;;; and switches depending on the sign of the shift (negative for right).
;;; That behaviour seems impossible to synthesise without using an extra
;;; register, so we provide two different operators instead. (This will
;;; change if I can figure out a way to synthesise ash.)
;;;
;;; The right shift (>>) operator behaves like ash in that it does
;;; sign extension automat6ically based on the type of the value. This
;;; means that Verilog's >>> (arithmetic shoft right) is generated implicitly
;;; by type, rather than being provided explicitly.

;;; <<

(defpassmethod compute-type (<< val offset)
  (declare (optimize debug))

  (let ((tyval (compute-type val))
	(tyoffset (compute-type offset)))

    ;; the width is the width of the value plus the
    ;; maximum number that can be in the offset
    `(and ,tyval (unsigned-byte (bitwidth ',tyoffset)))))


(defpassmethod simple-expression-form-p (<< &rest args)
  (every #'simple-expression-form-p args))


(defpassmethod synthesise (<< &rest args)
  (as-infix '<< args))


;;; TODO: Should we be able to pass an option to the into-auguments schema
;;; to change the function tag?

(defpassmethod lispify (<< args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(ash ,@vals)))


;;; >>

(defpassmethod compute-type (>> val offset)
  (let ((tyval (compute-type val))
	(tyoffset (compute-type offset)))

    ;; type is the same as the value, since
    ;; shifting right can only make it smaller
    tyval))


(defpassmethod simple-expression-form-p (>> &rest args)
  (:same-as <<))


(defpassmethod synthesise (>> val offset)
  (let ((ty (compute-type val)))
    (if (signed-byte-p ty)
	;; value is signed, do arithmetic shift
	(as-infix '>>> (list val offset))

	;; value is unsigned, do logical shift
	(as-infix '>> (list val offset)))))


(defpassmethod lispify (>> &rest args)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg))
		      args)))
    `(ash ,(car vals) (- ,(cadr vals)))))


;;; ---------- Bitwise operators ----------

;; LOGAND is the prototype

(defpassmethod compute-type (logand l r)
  '(unsigned-byte 1))


(defpassmethod apply-type-constraints (logand l r)
  (ensure-fixed-width (compute-type l))
  (ensure-fixed-width (compute-type r)))


(defpassmethod simple-expression-form-p (logand &rest args)
  (every #'simple-expression-form-p args))


(defpassmethod synthesise (logand l r)
  (as-literal "(")
  (synthesise l)
  (as-literal "&")
  (synthesise r)
  (as-literal ")"))


; LOGIOR

(defpassmethod compute-type (logior l r)
  '(unsigned-byte 1))


(defpassmethod apply-type-constraints (logior l r)
  (:same-as logand))


(defpassmethod simple-expression-form-p (logior &rest args)
  (:same-as logand))


(defpassmethod synthesise (logior l r)
  (as-literal "(")
  (synthesise l)
  (as-literal "|")
  (synthesise r)
  (as-literal ")"))


;; LOGXOR

(defpassmethod compute-type (logxor l r)
  '(unsigned-byte 1))


(defpassmethod apply-type-constraints (logxor l r)
  (:same-as logand))


(defpassmethod simple-expression-form-p (logxor &rest args)
  (:same-as logand))


(defpassmethod synthesise (logxor l r)
  (as-literal "(")
  (synthesise l)
  (as-literal "^")
  (synthesise r)
  (as-literal ")"))


(defpassmethod compute-type (lognot v)
  '(unsigned-byte 1))


(defpassmethod apply-type-constraints (lognot v)
  (ensure-fixed-width (compute-type v)))


(defpassmethod simple-expression-form-p (lognot v)
  (simple-expression-form-p v))


(defpassmethod synthesise (lognot v)
  (as-literal "(")
  (as-literal "~")
  (synthesise v)
  (as-literal ")"))


;;; ---------- Logical ----------

;;; AND

(defpassmethod compute-type (and &rest args)
  '(unsigned-byte 1))


(defpassmethod apply-type-constraints (and &rest args)
  (dolist (a args)
    (ensure-boolean (compute-type a))))


(defpassmethod simple-expression-form-p (and &rest args)
  (every #'simple-expression-form-p args))


(defpassmethod synthesise (and &rest args)
  (as-infix "&&" args))


;;; OR

(defpassmethod compute-type (or &rest args)
  '(unsigned-byte 1))


(defpassmethod apply-type-constraints (or &rest args)
  (:same-as and))


(defpassmethod simple-expression-form-p (or &rest args)
  (:same-as and))


(defpassmethod synthesise (or &rest args)
  (as-infix "||" args))


;;; NOT

(defpassmethod compute-type (not v)
  '(unsigned-byte 1))


(defpassmethod apply-type-constraints (not v)
  (ensure-boolean (compute-type v)))


(defpassmethod simple-expression-form-p (not v)
  (simple-expression-form-p v))


(defpassmethod synthesise (not v)
  (as-literal "(")
  (as-literal "~")
  (as-literal "(")
  (synthesise v)
  (as-literal ")")
  (as-literal ")"))
