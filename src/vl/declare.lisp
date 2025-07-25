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
so use in later passes."))


(defmethod add-frames-sexp ((fun (eql 'declare)) args)
  (mapc (lambda (dec)
	  (with-current-form dec
	    (destructuring-bind (tag &rest decargs)
		dec

	      (handler-case
		  (declare-annotation tag decargs)

		(vl-error (c)
		  (error c))

		(error ()
		  ;; other errors are interpreted as an
		  ;; unkown declaration
		  ;TODO: Might be too wide?
		  (warn 'unrecognised-declaration :tag tag))))))
	args)

  ;; return form unaltered
  `(declare ,@args))


(defmethod compute-type-sexp ((fun (eql 'declare)) args)
  t)


(defmethod apply-type-constraints-sexp ((fun (eql 'declare)) args)
  nil)


(defmethod read-variables-sexp ((fun (eql 'declare)) args)
  '())


(defmethod synthesise-sexp ((fun (eql 'declare)) args)
  nil)


;; ---------- Standard annotations ----------

(defmethod declare-annotation ((tag (eql 'type)) args)
  (destructuring-bind (ty &rest vars)
      args
    (dolist (n vars)
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
    (mapc (lambda (n)
	    (set-variable-property n 'as rep))
	  vars)))


(defmethod declare-annotation ((tag (eql 'direction)) args)
  (destructuring-bind (rep &rest vars)
      args
    (mapc (lambda (n)
	    (set-variable-property n 'direction rep))
	  vars)))


(defmethod declare-annotation ((tag (eql 'ignore)) args)
  (mapc (lambda (n)
	  (set-variable-property n 'ignore t))
	args))


(defmethod declare-annotation ((tag (eql 'ignorable)) args)
  (mapc (lambda (n)
	  (set-variable-property n 'ignorable t))
	args))
