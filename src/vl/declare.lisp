;; Variable annotation declarations
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


(defgeneric declare-annotation (tag args)
  (:documentation "Handle the annotation declaration TAG with ARGS.

Methods on this function should add information about the
annotation to the current frame of the global environment,
so use in later passes.

Unrecognised annotations are ignored with a warning.")
  (:method (tag args)
    (warn 'unrecognised-declaration :tag tag)))


(defmethod add-frames-sexp ((fun (eql 'declare)) args)
  (mapc (lambda (dec)
	  (with-current-form dec
	    (destructuring-bind (tag &rest decargs)
		dec
	      (declare-annotation tag decargs))))
	args)

  ;; return form unaltered
  `(declare ,@args))


(defmethod compute-type-sexp ((fun (eql 'declare)) args)
  t)


(defmethod apply-type-constraints-sexp ((fun (eql 'declare)) args)
  nil)


(defmethod float-let-blocks-sexp ((fun (eql 'declare)) args)
  ;; delete declarations when blocks are floated
  '(() ()))


(defmethod read-variables-sexp ((fun (eql 'declare)) args)
  '())


(defmethod compute-dependencies-sexp ((fun (eql 'declare)) args))


(defmethod synthesise-sexp ((fun (eql 'declare)) args)
  nil)


;; ---------- Standard annotations ----------

(defmethod declare-annotation ((tag (eql 'type)) args)
  (destructuring-bind (ty &rest vars)
      args
    (dolist (n vars)
      (ensure-variable-declared-in-current-frame n)

      ;; set the explicit type
      (set-variable-property n 'type ty)

      ;; ... and add as a constraint
      (add-type-constraint n ty))))


(defmethod declare-annotation ((tag (eql 'width)) args)
  (destructuring-bind (width &rest vars)
      args
    ;; width is a shortcut for an unsigned-byte type
    (declare-annotation 'type (cons `(unsigned-byte ,width) vars))))


(defmethod declare-annotation ((tag (eql 'as)) args)
  (destructuring-bind (rep &rest vars)
      args
    (dolist (n vars)
      (ensure-variable-declared-in-current-frame n)
      (set-variable-property n 'as rep))))


(defmethod declare-annotation ((tag (eql 'direction)) args)
  (destructuring-bind (rep &rest vars)
      args
    (dolist (n vars)
      (ensure-variable-declared-in-current-frame n)
      (set-variable-property n 'direction rep))))


(defmethod declare-annotation ((tag (eql 'ignore)) args)
  (dolist (n args)
    (ensure-variable-declared-in-current-frame n)
    (set-variable-property n 'ignore t)))


(defmethod declare-annotation ((tag (eql 'ignorable)) args)
  (dolist (n args)
    (ensure-variable-declared-in-current-frame n)
    (set-variable-property n 'ignorable t)))
