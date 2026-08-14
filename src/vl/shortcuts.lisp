;;;; Core macros offering various shortcuts
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


;;; ---------- Single-armed conditionals ----------

(defcoremacro/vl when (condition &body body)
  "Execute BODY when CONDITION is true."
  `(if ,condition
       (progn
	 ,@body)))


(defcoremacro/vl unless (condition &body body)
  "Execute BODY unless CONDITION is true."
  `(if (not ,condition)
       (progn
	 ,@body)))


;;; ---------- Increment and decrement ----------

;; These work because places in Verilisp can't be side-effecting.

(defcoremacro/vl incf (place &optional (value 1))
  "Increment PLACE by VALUE, which defaults to 1."
  `(setf ,place (+ ,place ,value)))


(defcoremacro/vl decf (place &optional (value 1))
  "Decrement PLACE by VALUE, which defaults to 1."
  `(setf ,place (- ,place ,value)))


;;; ---------- Quick common tests ----------

(defcoremacro/vl 0= (arg)
  "Test whether ARG is equal to zero."
  `(= ,arg 0))


(defcoremacro/vl 0/= (arg)
  "Test whether ARG is not equal to zero."
  `(/= ,arg 0))


;;; ---------- Quick common maths operations ----------

(defcoremacro/vl 1+ (arg)
  "Return ARG plus one."
  `(+ ,arg 1))


(defcoremacro/vl 1- (arg)
  "Return ARG minus one."
  `(- ,arg 1))


(defcoremacro/vl 2* (arg)
  "Return ARG times two."
  `(<< ,arg 1))


(defcoremacro/vl 2/ (arg)
  "Return ARG divided by two."
  `(>> ,arg 1))


;;; ---------- Assertion tests and constructs ----------

(defcoremacro/vl asserted-p (arg)
  "Test whether ARG is non-zero."
  `(0/= ,arg))


(defcoremacro/vl with-asserted (arg &body body)
  "Evaluate BODY with ARG asserted.

ARG should be a generalised place, typically one bit wide. It is set
to 1 before executing BODY, and to 0 afterwards /regardless/ of what
it's value was before."
  `(progn
     (setf ,arg 1)
     ,@body
     (setf ,arg 0)))
