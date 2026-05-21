;;;; Recursion schemata
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

(in-package :verilisp/dsl)

;;; These are used to flesh-out the bodies of form-level functions.
;;; They actually don't have to be recursive at all: they'll be used
;;; to build the structurally recursive pass functions. In essence
;;; they provide the base case for a pass' recursion over forms
;;; in the language.
;;;
;;; The name of the schema is used in the :SCHEMA option to
;;; DEFPASS and DEFPASSMETHOD to choose the schema when
;;; no body is provided.
;;;
;;; There are several standard schemata defined already. New schemata
;;; can be defined for specific purposes using
;;; DEFINE-RECURSION-SCHEMA, which installs the new schema for use.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *recursion-schemata* nil
    "A list of recursion schemata function names."))


(defmacro define-recursion-schema (schema-name schema-args &body body)
  "Define a new recursion schema.

The SCHEMA-ARGS should contain two arguments which will contain the
name of the variable holding the form head and the name of the
variable holding the form arguments. It will implicitly be passed
three keyword arguments:

- :PASS-NAME containing the pass name
- :OPTION containing the list of arguments passed along with the schema name in the :SCHEMA clause
- :EXTRA containing the extra arguments of the pass.

The body should return the code to be inserted into the form-level
function, as a macro would."
  ;; check the schema prototype is correct
  (when (/= (length schema-args) 2)
    (error 'dsl-error :hint (format nil "Recursion schemata take exactly two arguments (not ~s)" schema-args)))

  (let ((docstring "A recursion schema"))
    ;; extract optional docstring
    (when (stringp (car body))
      (setq docstring (car body))
      (setq body (cdr body)))

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; wrap the schema up in a generic function
       (defgeneric ,schema-name ,(append schema-args '(&key pass-name option extra))
	 (:documentation ,docstring)

	 (:method ,(append schema-args '(&key pass-name option extra))
	   (declare (ignorable ,@schema-args pass-name option extra))

	   ,@body))

       ;; install the function as a valid schema
       (appendf *recursion-schemata* (list ',schema-name)))))


(defun recursion-schema-p (schema-name)
  "Test whether SCHEMA-NAME names a recursion schema."
  (member schema-name *recursion-schemata*))


(defun ensure-recursion-schema (schema-name)
  "Ensure that SCHEMA-NAME names a recursion schema."
  (unless (recursion-schema-p schema-name)
      (error 'dsl-error :hint (format nil "No recursion schema ~s defined" schema-name))))


;;; ---------- Standard schemata ----------

(define-recursion-schema fail-unknown-form (fun args)
  "Any call to this schema fails with an UNKNOWN-FORM error."
  (error 'unknown-form :form (cons fun args)
		       :hint (format nil "Define an entry for ~s handling ~s"
				     pass-name fun)))


(define-recursion-schema constant-form (fun args)
  "A recursion schema with a constant default :OPTION form.

The OPTION form consists of the cdr of the :SCHEMA clause, or NIL if
there was none."
  option)


(define-recursion-schema into-arguments (fun args)
  "A recursion schema that recurses into ARGS.

The form returned is a list of the form (FUN . VARGS) where VARGS
are the results of the recursive calls. If the results are irrelevant
use the OVER-ARGUMENTS schema."
  (let ((pass-f (if extra
		    (apply #'rcurry pass-name extra)
		    pass-name)))

    (let ((vargs (mapcar pass-f args)))
      (cons fun vargs))))


(define-recursion-schema over-arguments (fun args)
  "A recursion schema that maps the pass over the arguments.

The results of the map-over are discarded: to get the result,
use the INTO-ARGUMENTS schema."
  (mapc pass-name args))


(define-recursion-schema into-function-and-arguments (fun args)
  "A recursion schema that recurses into both FUN and ARGS.

The form returned is a list of the form (VFUN . VARGS) where VFUN and
VARGS are the results of the recursive calls."
  (let ((vfun (apply pass-name fun))
	(vargss (mapcar pass-name args)))
    (cons vfun vargs)))


(define-recursion-schema into-arguments-all-non-nil (fun args)
  "A recursion schema that recurses into all arguments and checks they're all non-NIL.

This is usually used for predicates over code."
  (every pass-name args))


(define-recursion-schema into-arguments-union (fun args)
  "A recursion schema that recurses into all arguments and unions the results."
  (union-all (mapcar pass-name args)))
