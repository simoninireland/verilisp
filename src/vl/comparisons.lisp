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

;;; The comparison operators. We provide all the ones commonly used in
;;; Common Lisp: there are others we should perhaps add too.


(defun ensure-boolean (ty)
  "Ensure TY is a boolean (bit).

Signals TYPE-MISMATCH if TY isn't boolean, which may be ignored
in many applications."
  (ensure-subtype ty 'bit))


;;; ---------- Assertedness ----------

(defpassmethod compute-type (asserted-p v)
  'bit)


(defpassmethod simple-expression-p (asserted-p v)
  (simple-expression-p v))


(defpassmethod synthesise (asserted-p v)
  (as-literal "(")
  (synthesise v)
  (as-literal " != 0)"))


;;; ---------- Maths ----------

;;; =
;;; This is the prototype for all the other maths comparisons

;;; TODO: Extend to handle arbitrary numbers of arguments like Common Lisp.

(defpassmethod compute-type (= l r)
  (ensure-fixed-width (compute-type l))
  (ensure-fixed-width (compute-type r))

  'bit)


(defpassmethod simple-expression-p (= &rest args)
  (every #'simple-expression-p args))


(defpassmethod synthesise (= l r)
  (as-literal "(")
  (synthesise l)
  (as-literal "==")
  (synthesise r)
  (as-literal ")"))


;;; /=

(defpassmethod compute-type (/= l r)
  (:same-as =))
(defpassmethod simple-expression-p (/= l r)
  (:same-as =))

(defpassmethod synthesise (/= l r)
  (as-literal "(")
  (synthesise l)
  (as-literal "!=")
  (synthesise r)
  (as-literal ")"))


;;; <
(defpassmethod compute-type (< l r)
  (:same-as =))
(defpassmethod simple-expression-p (< l r)
  (:same-as =))

(defpassmethod synthesise (< l r)
  (as-literal "(")
  (synthesise l)
  (as-literal "<")
  (synthesise r)
  (as-literal ")"))


;;; <=
(defpassmethod compute-type (<= l r)
  (:same-as =))
(defpassmethod simple-expression-p (<= l r)
  (:same-as =))

(defpassmethod synthesise (<= l r)
  (as-literal "(")
  (synthesise l)
  (as-literal "<=")
  (synthesise r)
  (as-literal ")"))


;;; >
(defpassmethod compute-type (> l r)
  (:same-as =))
(defpassmethod simple-expression-p (> l r)
  (:same-as =))

(defpassmethod synthesise (> l r)
  (as-literal "(")
  (synthesise l)
  (as-literal ">")
  (synthesise r)
  (as-literal ")"))


;;; >=
(defpassmethod compute-type (>= l r)
  (:same-as =))
(defpassmethod simple-expression-p (>= l r)
  (:same-as =))

(defpassmethod synthesise (>= l r)
  (as-literal "(")
  (synthesise l)
  (as-literal ">=")
  (synthesise r)
  (as-literal ")"))
