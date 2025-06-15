;; The compiler passes
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

(in-package :vl)
(declaim (optimize debug))


;; ---------- Free variables ----------

(defgeneric free-variables (form)
  (:documentation "Return all free variables in FORM.

A variable is free in a form if it hasn't appeared in a binder
that binds that variable. Use REWRITE-VARIABLES to re-write
free instances to new names.")
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (free-variables-sexp fun args))))


(defgeneric free-variables-sexp (fun args)
  (:documentation "Return all variables free in FUN applied to ARGS.")
  (:method (fun args)
    (foldr #'union (mapcar #'free-variables args) '())))


;; ---------- Variable re-writing ----------

(defgeneric rewrite-variables (form rewrite)
  (:documentation "Re-write free occurrances of variables in FORM.

The re-write rules in REWRITE are an alist mapping variable
names to their new form. No checks are performed.")
  (:method ((form integer) rewrite)
    form)
  (:method ((form symbol) rewrite)
    (if-let ((a (assoc form rewrite
		       :key #'symbol-name
		       :test #'string-equal)))
      ;; reference to rewriteable variable, re-write it
      (cadr a)

      ;; leave alone
      form))
  (:method ((form list) rewrite)
    (destructuring-bind (fun &rest args)
	form
      (rewrite-variables-sexp fun args rewrite))))


(defgeneric rewrite-variables-sexp (fun args rewrite)
  (:documentation "Rewite variables in REWRITE in FUN applied to ARGS.

Note that /everything/ gets re-written by default, including FUN.
(This is the only consistent way to deal with, for example, macros
that haven't yet been expanded in the body of a macro that needs to
re-write variables, such as WITH-BITFIELDS.) Override the default
method to change this behaviour.")
  (:method (fun args rewrite)
    (mapcar (rcurry #'rewrite-variables rewrite)
	    `(,fun ,@args))))


;; ---------- Constant folding ----------

(defgeneric fold-constant-expressions (form)
  (:documentation "Fold constants in expression FORM.

This folds and simplifies expressions, eliminating any
calculations that can be done early.")
  (:method (form)
    form)
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (fold-constant-expressions-sexp fun args))))


(defgeneric fold-constant-expressions-sexp (fun args)
  (:documentation "Fold constant expressions in FUN applied to ARGS.")
  (:method (fun args)
    (cons fun (mapcar #'fold-constant-expressions args))))


;; ---------- Type and width checking and inference ----------

(defgeneric typecheck (form)
  (:documentation "Type-check FORM in the current global environment.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-vl-errors-not-synthesisable
	(with-unknown-forms
	  (with-current-form form
	    (expand-type-parameters (typecheck-sexp fun args))))))))


(defgeneric typecheck-sexp (fun args)
  (:documentation "Type-check the application of FUN to ARGS in the global environment."))


(defgeneric typecheck-sexp-setf (selector val selectorargs &key sync)
  (:documentation "Type-check a SETF form allowing generalised places.

This matches a form (SETF (SELECTOR SELECTORARGS) VAL) and allows
different selectors to be used as generalised places."))


;; ---------- Dependencies ----------

(defgeneric dependencies (form)
  (:documentation "Find all the depenencies in FORM.

Return a list of variables whose dependencies have been changed.")
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (dependencies-sexp fun args))))


(defgeneric dependencies-sexp (fun args)
  (:documentation "Find the dependencies of FUN applied to ARGS.

Return a list of variables whose dependencies have been changed.")
  (:method (fun args)
    (foldr #'union (mapcar #'dependencies args) '())))


(defun traverse-dependencies (ns)
  "Traverse the dependencies for the variables NS.

This returns the dependencies of the NS, and all the dependencies of those
dependencies, and so on recursively. Constants do not count as dependencies
as they can't be updated."
  (foldr #'union (mapcar (lambda (n)
			   (if (static-constant-p n)
			       nil
			       (union (list n)
				      (variable-property n :dependencies :default nil))))
			 ns)
	 '()))


;; ---------- Let block coalescence ----------

(defgeneric float-let-blocks (form)
  (:documentation "Float nested LET blocks in FORM to the outermost level.

Return a list consisting of the new form and any declarations floated.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-vl-errors-not-synthesisable
	(float-let-blocks-sexp fun args)))))


(defgeneric float-let-blocks-sexp (fun args)
  (:documentation "Float nested LET blocks in FUN applied to ARGS.

The default recurses into each element of ARGS and reconstructs
the form with re-written versions of ARGS.

Return a list consisting of the new form and any declarations floated.")
  (:method (fun args)
    (flet ((pairwise-append (old form)
	     (destructuring-bind (oldbody oldenv)
		 old
	       (destructuring-bind (newbody newenv)
		   (float-let-blocks form)
		 (list (if (null oldbody)
			   (list newbody)
			   (append oldbody (list newbody)))
		       (if (null newenv)
			   oldenv
			   (add-frame-to-environment newenv oldenv)))))))

      (destructuring-bind (fargs fenv)
	  (foldr #'pairwise-append args (list '() (make-frame)))
	`((,fun ,@fargs) ,fenv)))))


;; ---------- PROGN coalescence ----------

(defgeneric simplify-progn (form)
  (:documentation "Collapse unnecessary PROGN forms in FORM.

This removes the PROGN around a single other form, as
well as PROGNs nested inside other PROGNs.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-vl-errors-not-synthesisable
	(simplify-progn-sexp fun args)))))


(defgeneric simplify-progn-sexp (fun args)
  (:documentation "Simplify PROGN blocks in FUN applied to ARGS.")
  (:method (fun args)
    `(,fun ,@(mapcar #'simplify-progn args))))


;; ---------- Macro expansion ----------

(defun expand-macros-in-environment (form &optional (f *macro-environment*))
  "Recursively expand all macros in FORM in the global macro environment.

This attaches the frame F before calling EXPAND-MACROS. If F is omitted
(as is usual) then the macros are taken from *MACRO-ENVIRONMENT*."
  (with-frame f
    (expand-macros form)))


(defgeneric expand-macros (form)
  (:documentation "Expand macros in FORM.")
  (:method (form)
    form)
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (expand-macros-sexp fun args))))


(defun expand-descend (fun args)
  "Expand macros in ARGS when FUN applied."
  `(,fun ,@(remove-if #'null (mapcar (lambda (arg)
				       (unless (null arg)
					 (expand-macros arg)))
				     args))))


(defgeneric expand-macros-sexp (fun args)
  (:documentation "Expand macros in FUN applied to ARGS.

The macros available are taken from the global environment. Usually
this will have *MACRO-ENVIRONMENT* attached to it prior to macro expansion.")
  (:method (fun args)
    (declare (optimize debug))
    (with-vl-errors-not-synthesisable
      (if (macro-declared-p fun)
	;; macro is expandable, replace with real name if there is one
	(let ((realfun (variable-property fun :real-name)))
	  (multiple-value-bind (expansion expanded)
	      (macroexpand-1 (cons realfun args))
	    (if expanded
		;; form was expanded by macro, expand the expansion
		(expand-macros expansion)

		;; form wasn't expanded, descend into the form
		(expand-descend fun args))))

	;; macro is not expandable, descend into the form
	(expand-descend fun args)))))


;; ---------- Synthesis ----------

(defgeneric synthesise (form)
  (:documentation "Synthesise the Verilog for FORM in the current environment.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-vl-errors-not-synthesisable
	(with-current-form form
	  (synthesise-sexp fun args)
	  t)))))


(defgeneric synthesise-sexp (fun args)
  (:documentation "Write the synthesised Verilog of FUN called with ARGS in the current environment."))


;; ---------- Lispification ----------

(defgeneric lispify (form)
  (:documentation "Convert FORM to a Lisp expression.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-vl-errors-not-synthesisable
	(lispify-sexp fun args)))))


(defgeneric lispify-sexp (fun args)
  (:documentation "Convert FUN applied to ARGS to Lisp.

The default leaves the expression unchanged, i.e., assumes that
this Verilisp fragment is valid Lisp.")
  (:method (fun args)
    (let ((lispargs (mapcar #'lispify args)))
      `(,fun ,@lispargs))))
