;; Synthesisable comparison operators
;;
;; Copyright (C) 2024 Simon Dobson
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

(in-package :vl)

(defun ensure-boolean (ty)
  "Ensure TY is a boolean (bit).

Signals TYPE-MISMATCH if TY isn't boolean, which may be ignored
in many applications."
  (ensure-subtype ty 'bit))


;; ---------- Equality and inequality ----------

(defmethod typecheck-sexp ((fun (eql '=)) args)
  (destructuring-bind (l r)
      args
    (let ((ty1 (typecheck l))
	  (ty2 (typecheck r)))
      (ensure-subtype ty2 ty1))

    '(unsigned-byte 1)))


(defmethod synthesise-sexp ((fun (eql '=)) args)
  (destructuring-bind (l r)
      args
    (as-literal "(")
    (synthesise l)
    (as-literal " == ")
    (synthesise r)
    (as-literal ")")))


(defmethod typecheck-sexp ((fun (eql '/=)) args)
  (destructuring-bind (l r)
      args
    (let ((ty1 (typecheck l))
	  (ty2 (typecheck r)))
       (ensure-subtype ty2 ty1))

    '(unsigned-byte 1)))


(defmethod synthesise-sexp ((fun (eql '/=)) args)
  (destructuring-bind (l r)
      args
    (as-literal "(")
    (synthesise l)
    (as-literal " != ")
    (synthesise r)
    (as-literal ")")))


;; ---------- Assertedness ----------

(defmethod typecheck-sexp ((fun (eql 'asserted-p)) args)
  (destructuring-bind (v)
      args
    (let ((ty (typecheck v)))
      '(unsigned-byte 1))))


(defmethod synthesise-sexp ((fun (eql 'asserted-p)) args)
  (destructuring-bind (v)
      args
    (as-literal "(")
    (synthesise v)
    (as-literal " != 0")))


;; ---------- Maths ----------

(defmethod typecheck-sexp ((fun (eql '<)) args)
  (destructuring-bind (l r)
      args
    (ensure-fixed-width (typecheck l))
    (ensure-fixed-width (typecheck r))

    '(unsigned-byte 1)))


(defmethod synthesise-sexp ((fun (eql '<)) args)
  (destructuring-bind (l r)
      args
    (as-literal "(")
    (synthesise l)
    (as-literal " < ")
    (synthesise r)
    (as-literal ")")))


(defmethod typecheck-sexp ((fun (eql '>)) args)
  (destructuring-bind (l r)
      args
    (ensure-fixed-width (typecheck l))
    (ensure-fixed-width (typecheck r))

    '(unsigned-byte 1)))


(defmethod synthesise-sexp ((fun (eql '>)) args)
  (destructuring-bind (l r)
      args
    (as-literal "(")
    (synthesise l)
    (as-literal " > ")
    (synthesise r)
    (as-literal ")")))


(defmethod typecheck-sexp ((fun (eql '<=)) args)
  (destructuring-bind (l r)
      args
    (ensure-fixed-width (typecheck l))
    (ensure-fixed-width (typecheck r))

    '(unsigned-byte 1)))


(defmethod synthesise-sexp ((fun (eql '<=)) args)
  (destructuring-bind (l r)
      args
    (as-literal "(")
    (synthesise l)
    (as-literal " <= ")
    (synthesise r)
    (as-literal ")")))


(defmethod typecheck-sexp ((fun (eql '>=)) args)
  (destructuring-bind (l r)
      args
    (ensure-fixed-width (typecheck l))
    (ensure-fixed-width (typecheck r))

    '(unsigned-byte 1)))


(defmethod synthesise-sexp ((fun (eql '>=)) args)
  (destructuring-bind (l r)
      args
    (as-literal "(")
    (synthesise l)
    (as-literal " >= ")
    (synthesise r)
    (as-literal ")")))
