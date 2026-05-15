;;;; Evaluating Verilisp as Lisp
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

;;; These functions allow code that is either Lisp or Verilisp to be evaluated.
;;; Verilisp code is Lispified and then evaluated in a static environment,
;;; meaning these functions can be used to expand code at compile-time for
;;; operations like constant folding.


;;; ---------- Static tests ----------

(defun constant-p (form)
  "Test whether FORM is a constant.

Constants include literals and constant symbols, but do
not include module parameters: for that use STATIC-CONSTANT-P."
  (or (integerp form)
      (and (symbolp form)
	   (variable-declared-p form)
	   (eql (get-representation form) 'constant))))


(defun static-constant-p (form)
  "Test whether FORM is a static constant.

Static constants include constants as identified by
CONSTANT-P and module parameters."
  (or (integerp form)
      (and (symbolp form)
	   (variable-declared-p form)
	   (member (get-representation form) '(parameter
					       constant)))))


(defun static-p (form)
  "Test whether FORM is statically known.

This just evaluates FORM and throws away the result."
  (handler-case
      (progn
	(eval-in-static-environment form)
	t)
    (error () nil)))


;;; ---------- Environment closure ----------

;;; These functions work on explicitly-provided environments, *not*
;;; the current global environment -- although the given environment
;;; will typically be derived from there.

(defun make-environment-alist (env)
  "Return a list of name/value pairs of the elements of ENV.

Each variable appears only once, with shallower declarations
shadowing deeper ones.

The pairs can be used in LET blocks, or as an alist."
  (labels ((remove-seen (seen l)
	   (if (null l)
	       (list seen '())

	       (destructuring-bind (rseen rl)
		   (remove-seen seen (cdr l))
		 (destructuring-bind (n v)
		     (car l)
		   (if (member n rseen)
		       (list rseen rl)

		       (list (cons n rseen)
			     (cons (list n v) rl))))))))

    (let ((decls (map-environment (lambda (n env)
				    (list n (get-frame-property n 'initial-value env :default 0)))
				  env)))
      (cadr (remove-seen '() decls)))))


(defun close-form-in-environment (form env)
  "Close FORM as a Lisp term in ENV.

Each declaration in scope is converted into a LET declaration with the
same name and initial value. FORM is then placed as the body of this
LET form.

Note that FORM should be Lisp, not Verilisp: to evaluate Verilisp forms
first use LISPIFY to generate proper Lisp for evaluation."
  (if (empty-environment-p env)
      form

      ;; expand the static enviroment and close over it
      (let* ((ext (make-environment-alist env))
	     (ns (alist-keys ext)))
	`(let ,ext
	   (declare (ignorable ,@ns)) ;; don't warn about un-used variables
	   ,form))))


;;; ---------- Closure against specific sub-environments ----------

;;; These functions close a form against a specific sub-environment of
;;; the current global environment.


(defun close-form-in-constant-environment (form)
  "Close FORM as Lisp in the constant part of the current environment.

The environment is filtered to include only the constant elements
of the environment, as identified by CONSTANT-P. In other words, the
form is placed in an environment with no non-constant information."
  (let ((constantenv (filter-environment (lambda (n env)
					   (constant-p n))
					 (current-frame))))
    (close-form-in-environment form constantenv)))


(defun close-form-in-static-environment (form)
  "Close FORM as Lisp in the static part of the current environment.

The environment is filtered to include only the static elements of
the environment, as identified by STATIC-CONSTANT-P. In other words,
the form is placed into an environment that is known at compile time."
  (let ((staticenv (filter-environment (lambda (n env)
					 (static-constant-p n))
				       (current-frame))))
    (close-form-in-environment form staticenv)))


;;; ---------- Lisp evaluation ----------

;; These functions evaluate Lisp expressions.

(defun eval-lisp-in-static-environment (form)
  "Evaluate Lisp FORM in the static part of the current environment.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters. In other words,
the evaluation takes place in an environment that is known at
compile time.

FORM should be in Lisp: to evaluate Verilisp forms use
EVAL-IN-STATIC-ENVIRONMENT."
  (declare (optimize debug))

  (let ((closed-form (close-form-in-static-environment form)))
    (handler-bind
	((warning (lambda (c)
		    (declare (ignore c))
		    (muffle-warning))))

      ;; any warnings caused by this evaluation will be silently
      ;; ignored; any errors will be propagated
      (eval closed-form))))


;;; ---------- Verilisp evaluation ----------

;;; The forms passed to these functions should be Verilisp, which is
;;; Lispified before closure and evaluation

(defun eval-in-static-environment (form)
  "Evaluate Verilisp FORM as Lisp in the static part of the current environment.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters. In other words,
the evaluation takes place in an environment that is known at
compile time.

The resulting form is evaluated as Lisp, and so will signal
standard Lisp conditions. To run Lisp (rather than Verilisp) use
EVAL-LISP-IN-STATIC-ENVIRONMENT."
  (with-current-form form
    (handler-bind
	((error (lambda (c)
		  (declare (ignore c))
		  (error 'not-static :hint "Make sure expression is statically determinable"))))

      (eval-lisp-in-static-environment (lispify form)))))


(defun ensure-static (form)
  "Evaluate Verilisp FORM in the static environment, returning its value.

A NOT-STATIC error condition is signalled if FORM does not
evaluate correctly in the static environment."
  (eval-in-static-environment form))


(defun eval-if-static (form)
  "Evauate Verilisp FORM in the static environment, returning its value.

If the form isn't statically constant, return NIL."
  (handler-case
      (eval-in-static-environment form)
    (error () nil)))


(defun eval-if-constant-or-return (form)
  "Evaluate Verilisp FORM in the constant environment, returning its value.

If FORM is not a constant, return FORM unchanged. This is essentially a function
to simplify constant expressions."
   (handler-case
      (eval-in-constant-environment form)
    (error () form)))
