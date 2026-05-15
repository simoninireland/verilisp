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
;;; to build the structurally recursive pass functions.
;;;
;;; The name of the schema is used in the :SCHEMA option to
;;; DEFPASS and DEFPASSMETHOD to choose the schema when
;;; no body is provided.
;;;
;;; There are several standard schemata defined already. New schemata
;;; can be defined for specific purposes using
;;; DEFINE-RECURSION-SCHEMA, which installs the new schema for use.


(defparameter *recursion-schemata* nil
  "A list of recursion scheme function names.")


;;; TODO: Re-structure so that the schema function has &allow-other-keys,
;;; and let the implemnentations pull out :option and :extra

(defmacro define-recursion-schema (schema-name schema-args &body body)
  "Define a new recursion schema.

The schema should take three arguments which will contain the name of
the variable holding the form head, the name of the variable holding
the form arguments, and the name of the pass. It will implicitly be
passed two keyword arguments, :OPTION containing the aruments passed
along with the schema name in the option clause, and :EXTRA containing
the extra arguments on the pass. It should return the code to be
inserted into the form-level function, as a macro would."
  ;; check the schema prototype is correct
  (when (< (length schema-args) 3)
    (error 'dsl-error :hint (format nil "Recursion schemata take at least three arguments (not ~s)" schema-args)))

  (let ((docstring "A recursion schema"))
    ;; extract optional docstring
    (when (stringp (car body))
      (setq docstring (car body))
      (setq body (cdr body)))

    `(progn
       ;; wrap the schema up in a function
       (defun ,schema-name ,(append schema-args
			     '(&key option extra))
	 ,docstring
	 ,@body)

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

(define-recursion-schema fail-unknown-form (fun args pass-name)
  "Any call to this schema fails with an UNKNOWN-FORM error."
  `(error 'unknown-form :form (cons ,fun ,args)
			:hint (format nil "Define an entry for ~s handling ~s"
				      ',pass-name ,fun)))


(define-recursion-schema constant-form (fun args pass-name)
  "A recursion schema with a constant default OPTION form.

The OPTION form consists of the second argument of the :SCHEMA
clause, or NIL if there was none. The form can be a constant or a form
that will be evaluated within the body of the handler."
  option)


(define-recursion-schema into-arguments (fun args pass-name)
  "A recursion schema that recurses into ARGS.

The form returned is a list of the form (FUN . VARGS) where VARGS
are the results of the recursive calls. If the results are irrelevant
use the OVER-ARGUMENTS schema."
  (let ((pass-f (if extra
		    `(rcurry #',pass-name ,@extra)
		    `#',pass-name)))

    (with-gensyms (vals)
      `(let ((,vals (mapcar ,pass-f ,args)))
	 (cons ,fun ,vals)))))


(define-recursion-schema over-arguments (fun args pass-name)
  "A recursion schema that maps the pass over the arguments.

The results of the map-over are discarded: to get the result,
use the INTO-ARGUMENTS schema."
  `(mapc #',pass-name ,args))


(define-recursion-schema into-function-and-arguments (fun args pass-name)
  "A recursion schema that recurses into both FUN and ARGS.

The form returned is a list of the form (VFUN . VARGS) where VFUN and
VARGS are the results of the recursive calls."
  (with-gensyms (vfun vals)
    `(let ((,vfun (,pass-name ,fun))
	   (,vals (mapcar #',pass-name ,args)))
       (cons ,vfun ,vals))))


(define-recursion-schema into-arguments-all-non-nil (fun args pass-name)
  "A recursion schema that recurses into all arguments and checks they're all non-NIL.

This is usually used for predicates over code."
  `(every #',pass-name ,args))


(define-recursion-schema into-arguments-union (fun args pass-name)
  "A recursion schema that recurses into all arguments and unions the results."
  `(union-all (mapcar #',pass-name ,args)))
