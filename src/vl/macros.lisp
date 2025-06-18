;; (Re-)implementations of some macros
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

;; We re-implement some macros to avoid possible issues with the standard
;; versions generating non-synthesisable Lisp. The names of these macros
;; are suffixed /vl, and we alias them using ADD-MACRO in the loader.

(in-package :vl)
(declaim (optimize debug))


;; ---------- Single-armed conditionals ----------

(defmacro when/vl (cond &body body)
  "Execute BODY when COND is true."
  `(if ,cond
       (progn
	 ,@body)))


(defmacro unless/vl (cond &body body)
  "Execute BODY unless COND is true."
  `(if (not,cond)
       (progn
	 ,@body)))


;; ---------- Declaring wires, constants, and registers ----------

(defun add-representation-to-decl (rep decl)
  "Represent DECL as REP.

If DECL already has an :AS key whose value isn't REP, a
REPRESENTATION-MISMATCH error is signalled."
  (destructuring-bind (n v &rest keys)
      decl
    (if-let ((m (member :as keys)))
      ;; decl has a representation, check it
      (if (not (eql (cadr m) rep))
	  ;; different representation, signal the mismatch
	  (error 'representation-mismatch :expected rep
					  :got (cadr m)
					  :hint "Don't add explicit reepresentations within representation-declaring forms")

	  ;; same representation, return unchanged
	  decl)

      ;; expand the decl with the representation
      `(,n ,v :as ,rep ,@keys))))


(defun add-representation-to-decls (rep decls)
  "Add REP as the representation for all DECLS."
   (mapcar (curry #'add-representation-to-decl rep)
	   decls))


(defmacro let-wires (decls &body body)
  "Declare all DECLS as wires in BODY."
  `(let ,(add-representation-to-decls :wire decls)
     ,@body))


(defmacro let-registers (decls &body body)
  "Declare all DECLS as registers in BODY."
  `(let ,(add-representation-to-decls :register decls)
     ,@body))


(defmacro let-constants (decls &body body)
  "Declare all DECLS as constants in BODY."
  `(let ,(add-representation-to-decls :constant decls)
     ,@body))



;; ---------- Increment and decrement ----------

;; These work because places in Verilisp can't be side-effecting.

(defmacro incf/vl (place &optional (value 1))
  "Increment PLACE by VALUE, which defaults to 1."
  `(setf ,place (+ ,place ,value)))


(defmacro decf/vl (place &optional (value 1))
  "Decrement PLACE by VALUE, which defaults to 1."
  `(setf ,place (- ,place ,value)))


;; ---------- Quick common tests ----------

(defmacro 0= (arg)
  "Test whether ARG is equal to zero."
  `(= ,arg 0))


(defmacro 0/= (arg)
  "Test whether ARG is not equal to zero."
  `(/= ,arg 0))


;; ---------- Quick common maths operations ----------

(defmacro 1+/vl (arg)
  "Return ARG plus one."
  `(+ ,arg 1))


(defmacro 1-/vl (arg)
  "Return ARG minus one."
  `(- ,arg 1))


(defmacro 2* (arg)
  "Return ARG time two."
  `(<< ,arg 1))
