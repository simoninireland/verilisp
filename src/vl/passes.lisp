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

(in-package :verilisp/core)
(declaim (optimize debug))


;; ---------- Free variables ----------

(defgeneric read-variables (form)
  (:documentation "Return all variables in FORM that are read from.")
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (read-variables-sexp fun args))))


(defgeneric read-variables-sexp (fun args)
  (:documentation "Return all variables that are read in FUN applied to ARGS.

Methods on this functon should return a set consisting of the
variables that could be read during the evaluation of FORM.

The default is to combine all the variables in the arguments.")
  (:method (fun args)
    (foldr #'union (mapcar #'read-variables args) '())))


(defgeneric read-variables-sexp-setf (selector val selectorargs)
  (:documentation "Return all variables that are read in an assignment..

Methods on this function should return a set consisting of the
variables that could be read while using FORM as a generalised place."))


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


;; ---------- Applying and removing frames ----------

(defgeneric add-frames (form)
  (:documentation "Add frames to forms that need to maintain an environment.")
  (:method (form)
    form)
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-unknown-forms
	(with-current-form form
	  (add-frames-sexp fun args))))))


(defgeneric add-frames-sexp (fun args)
  (:documentation "Add frames to FUN applied to ARGS.

Mathods on this function should add a local frame to the form for later
use and recurse into sub-forms.

The frame should be populated with the names of any variables introduced
by the form: this allows later passes to interrogate the locally-defined
environment, and to add and access properties of those variables.

The way the frame is stored is not specified, but the functions
ADD-FRAMES-TO-DECLS and GET-LOCAL-FRAME-AND-DECLS pefrom adding
and accessing by extending the list of declarations found in LET and
MODULE forms. The WITH-LOCAL-FRAME macro can then be used to apply
the frame automatically in other methods.")
  (:method (fun args)
    `(,fun ,@(mapcar #'add-frames args))))


(defun add-local-frame-to-decls (decls &optional (f (make-frame)))
  "Add a local frame F to DECLS.

A new, empty, frame is added if F is omitted.

Return the new decls. If DECLS was originally NULL, this will be
a new list containing just the frame; if not, then the frame will have
been added to the end destructively."
   (if (null decls)
      ;; no decls, return a new list
      (setf decls (list (list 'local-frame f)))

      ;; existing decls, add frame as a new decl
      (setf (cdr (last decls)) (list (list 'local-frame f))))

  decls)


(defun get-local-frame-and-decls (decls)
  "Return a list consisting of the local frame and the remaining real decls from DECLS."
  (declare (optimize debug))

  (labels ((local-frame-p (decl)
	     (and (listp decl)
		  (eql (car decl) 'local-frame)))

	   (decl-p (decl)
	     (not (local-frame-p decl))))

    (let ((f-decls (filter-by-predicates decls #'local-frame-p #'decl-p)))
      (when (null (car f-decls))
	(error 'no-local-frame))

      ;; return local frame as a singleton, followed by the "real" decls
      (let ((f (cadr (caar f-decls))))
	(cons f (list (cadr f-decls)))))))


(defun get-local-frame (decls)
  "Retrieve the local frame from DECLS."
  (if-let ((m (assoc 'local-frame decls)))
    (cadr m)

    (error "No local frame?")))


(defmacro with-local-frame (decls &body body)
  "Run BODY in a global environment including the locally-applied frame from DECLS.

DECLS should be a variable holding the declarations, which is re-bound
within BODY to hold only the 'real' declarations with the local frame
removed."

  ;; ensure we get passed a variable name, not an expression
  (unless (symbolp decls)
    (error "Non-symbol ~a passed to WITH-LOCAL-FRAME" decls))

  ;; extract frame and decls, and run BODY in a suitable environment
  (with-gensyms (f-decls cached-frame)
    `(let* ((,f-decls (get-local-frame-and-decls ,decls))
	    (,cached-frame (car ,f-decls)))
       (with-frame ,cached-frame
	 (let ((,decls (cadr ,f-decls)))
	   (declare (ignorable ,decls))

	   ,@body)))))


;; ---------- Type checking and inference ----------

(defgeneric apply-type-constraints (form)
  (:documentation "Evaluate type constraints to constraining variables in FORM.")
  (:method (form)
    nil)

  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (apply-type-constraints-sexp fun args))))


(defgeneric apply-type-constraints-sexp (fun args)
  (:documentation "Apply type constraints in FUN applied to ARGS.

Methods on this function should apply any type constraints they place
upon FUN.")
  (:method (fun args)
    (mapc #'apply-type-constraints args)))


(defgeneric compute-type (form)
  (:documentation "Compute the type of FORM.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-unknown-forms
	(with-current-form form
	  (compute-type-sexp fun args))))))


(defgeneric compute-type-sexp (fun args)
  (:documentation "Compute the type of the application of FUN to ARGS."))


(defgeneric compute-type-sexp-setf (selector val selectorargs)
  (:documentation "Compute the type of a SETF form allowing generalised places.

This matches a form (SETF (SELECTOR SELECTORARGS) VAL) and allows
different selectors to be used as generalised places."))


(defun typecheck (form)
  "Perform a typechecking pass over FORM.

This extracts types and applies any constraints needed to infer the
types of variables."
  (let ((ty (compute-type form)))

    ;; check any remaining constraints after inference
    (apply-type-constraints form)

    ;; infer representations on the tree
    (infer-representation form)

    ty))


;; ---------- Generalised places ----------

(defgeneric generalised-place-p (form)
  (:documentation "Test whether FORM is a generalised place.

Generalised places can appear as the target of SETF forms. (In other
languages they are sometimes referred to as *lvalues*.) This is
separate, but related to, their type: a generalised place has a type,
but is also SETF-able.")
  (:method ((form integer))
    nil)
  (:method ((form symbol))
    (writeable-p form))
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (generalised-place-sexp-p fun args))))


(defgeneric generalised-place-sexp-p (selector selectorargs)
  (:documentation "Test whether SELECTOR applied to SELECTORARGS identifies a generalised place.

Methods on this function should identify those forms that are generalised places.
Usually this will only involve examining SELECTOR.")
  (:method (selector selectorargs)
    nil))


;; ---------- Dependencies ----------

(defun add-dependencies (n deps)
  "Add variables DEPS as dependencies for N.

A dependency is a variable that's read in assigning values to N."
  (let ((old-deps (variable-property n 'depends-on)))
    (set-variable-property n 'depends-on (union deps old-deps)))

  ;; any variables we depend on are read by definition
  (dolist (m deps)
    (set-variable-property m 'read t)))


(defun traverse-dependencies (ns)
  "Traverse the dependencies for the variables NS.

NS can be a variable name or a list of variables.

Return the dependencies of the NS, and all the dependencies of those
dependencies, and so on recursively. Constants do not count as
dependencies as they can't be updated."

  ;; get the direct dependencies
  (let ((direct (foldr #'union
		       (mapcar (lambda (n)
				 (variable-property n 'depends-on :default nil))
			       (if (listp ns)
				   ns
				   (list ns)))
		       '())))

    ;; traverse to further dependencies
    (foldr #'union (mapcar (lambda (n)
			     (if (static-constant-p n)
				 nil
				 (union (list n)
					(variable-property n 'depends-on :default nil
							   ))))
			   direct)
	   '())))


;; ---------- Representation inference ----------

(defgeneric infer-representation (form)
  (:documentation "Infer the representations of variables in FORM.")
  (:method (form)
    nil)
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (infer-representation-sexp fun args))))


(defgeneric infer-representation-sexp (fun args)
  (:documentation "Infer the representations of variables in FUN applied to ARGS.

Methods on this function should annotate the variables with
appropriate representations (the AS property). This will generally
only happen in binders.")
  (:method (fun args)
    (mapc #'infer-representation args)))



;; ---------- Let block coalescence ----------

(defgeneric float-let-blocks (form)
  (:documentation "Float nested LET blocks in FORM to the outermost level.

Return a list consisting of the new form and any declarations floated.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (float-let-blocks-sexp fun args))))


(defun float-merge (forms)
  "Float LET blocks in FORMS left to right.

Return the re-written FORMS and a merged environment."
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

    (foldr #'pairwise-append forms (list '() (make-frame)))))


(defgeneric float-let-blocks-sexp (fun args)
  (:documentation "Float nested LET blocks in FUN applied to ARGS.

The default recurses into each element of ARGS and reconstructs
the form with re-written versions of ARGS.

Return a list consisting of the new form and any declarations floated.")
  (:method (fun args)
    (destructuring-bind (fargs fenv)
	  (float-merge args)
	`((,fun ,@fargs) ,fenv))))


;; ---------- PROGN coalescence ----------

(defgeneric simplify-progn (form)
  (:documentation "Collapse unnecessary PROGN forms in FORM.

This removes the PROGN around a single other form, as
well as PROGNs nested inside other PROGNs.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (simplify-progn-sexp fun args))))


(defgeneric simplify-progn-sexp (fun args)
  (:documentation "Simplify PROGN blocks in FUN applied to ARGS.")
  (:method (fun args)
    `(,fun ,@(mapcar #'simplify-progn args))))


;; ---------- Macro expansion ----------

(defun expand-macros-in-environment (form &optional (f *global-environment*))
  "Recursively expand all macros in FORM in an environment.

This attaches the frame F before calling EXPAND-MACROS. If F is omitted
(as is usual) then the macros are taken from *GLOBAL-ENVIRONMENT*."
  (in-frame f
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
  `(,fun ,@(remove-nulls (mapcar (lambda (arg)
				   (unless (null arg)
				     (expand-macros arg)))
				 args))))


(defgeneric expand-macros-sexp (fun args)
  (:documentation "Expand macros in FUN applied to ARGS.

The macros available are taken from the current environment.
Use EXPAND-MACROS-IN-ENVIRONMENT to select a specific environment.")
  (:method (fun args)
    (declare (optimize debug))

    (if (macro-declared-p fun)
	;; macro is expandable, replace with real name if there is one
	(let ((realfun (variable-property fun 'real-name))
	      (f (variable-property fun 'local-frame)))

	  ;; expand the macro in a nested environment that will receive
	  ;; any macros locally defined
	  (with-frame f
	    (multiple-value-bind (expansion expanded)
		(macroexpand-1 (cons realfun args))
	      (if expanded
		  ;; form was expanded by macro, expand the expansion
		  (expand-macros expansion)

		  ;; form wasn't expanded, descend into the form
		  (expand-descend fun args)))))

	;; macro is not expandable, descend into the form
	(expand-descend fun args))))


;; ---------- Synthesis ----------

(defgeneric synthesise (form)
  (:documentation "Synthesise the Verilog for FORM in the current environment.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-current-form form
	(synthesise-sexp fun args)
	t))))


(defgeneric synthesise-sexp (fun args)
  (:documentation "Write the synthesised Verilog of FUN called with ARGS in the current environment."))


;; ---------- Lispification ----------

(defgeneric lispify (form)
  (:documentation "Convert FORM to a Lisp expression.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (lispify-sexp fun args))))


(defgeneric lispify-sexp (fun args)
  (:documentation "Convert FUN applied to ARGS to Lisp.

The default leaves the expression unchanged, i.e., assumes that
this Verilisp fragment is valid Lisp.")
  (:method (fun args)
    (let ((lispargs (mapcar #'lispify args)))
      `(,fun ,@lispargs))))
