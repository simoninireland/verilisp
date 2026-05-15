;;;; The compiler nanopasses and other functions
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

;;; Compiler nanopasses transform the program code, with each pass performing
;;; a single, well-defined transformation.
;;;
;;; Compiler functions compute some value over the code, and are typically
;;; predicates (but can be other things).


;;; ---------- Pass queues ----------

;;; The pass queues correspond to "macro-passes", composed of
;;; nanopasses. Implementations can attach new nanopasses in the
;;; appropriate place.
;;;
;;; The standard nanopasses are added as defined below and added to the
;;; appropriate queues. Note that the order that passes are added to
;;; their queue is probably significant.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-pass-queue expanding)
  (define-pass-queue typing)
  (define-pass-queue transforming)
  (define-pass-queue synthesising))


;;; ---------- Free variables ----------

(defpass read-variables (form)
  (:documentation "Return all variables in FORM that are read from.

This function is used for constructing dependencies of variables.
Return the set of variables as a list.")
  (:schema into-arguments-union))


(defpass read-variables-setf (form)
  (:documentation "Return all variables that are read in an assignment.

This function needs a method for each generalised place, to determine
which variables are read in computing an assignment to this place. The
methods should not return variables that are updated: this is
provided by WRITTEN-VARIABLES-SETF.

Return the set of variables as a list."))


(defpass written-variables-setf (form)
  (:documentation "Return all variables that are written in an assignment.

This function needs a method per generalised place, to determine
which variables are written to during an assignment. The methods
should not return variables that are only read and not updated.

Return the set of variables as a list."))


;;; ---------- Macro expansion ----------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-recursion-schema into-arguments-macros (fun args pass-name)
    "A recursion schema that expands a form as a macro.

The schema simply calls EXPAND-IF-MACRO witrh the appropriate variables."
    (declare (ignore pass-name))

    `(expand-if-macro ,fun ,args)))


(defpass expand-macros (form)
  (:documentation "Expand macros in FORM.")
  (:queue expanding)
  (:schema into-arguments-macros)

  (:method (form)
    form))


(defun expand-if-macro (fun args)
  "Expand a form as a macro.

If FUN is a macro, expand it and then re-expand the resulting
substitution. If it is not a macro, descend into ARGS."
  (declare (optimize debug))

  (if (macro-declared-p fun)
      ;; macro is expandable
      (let ((realfun (variable-property fun 'initial-value)))

	;; expand the macro in a nested environment that will contain
	;; any locally-declared macros
	(with-new-frame
	  (let ((expansion (apply realfun args)))

	    ;; expand the expansion
	    (expand-macros expansion))))

      ;; macro is not expandable, descend into the form
      (expand-descend fun args)))


(defun expand-descend (fun args)
  "Expand macros in ARGS when FUN applied."
  `(,fun ,@(remove-nulls (mapcar (lambda (arg)
				   (unless (null arg)
				     (expand-macros arg)))
				 args))))


(defun expand-macros-in-environment (form &optional (f *global-environment*))
  "Recursively expand all macros in FORM in an environment.

Thos pass expands all macros to convert FORM into Core Verilisp.
Macros are taken from the global environment unless a specific
frame F is provided.

Return the expanded form."
  (in-frame f
    (expand-macros form)))


;;; ---------- Applying and removing frames ----------

(defpass add-frames (form)
  (:documentation "Add frames to FORM.

Frames are used to maintain the lexical environment for the compiler.
Methods on this function should construct frames, associate them with
the appropriate binders so they can be applied in other passes, and
populate them with the variables being declared. These declarations
will then be used by, and extended by, other passes.")
  (:schema into-arguments)
  (:queue expanding)

  (:method (form)
    form))


(defun add-local-frame-to-decls (decls &optional (f (make-frame)))
  "Add a local frame F to DECLS.

Lisp binders typically store declarations as an alist. This function
exploits this commonality by adding a Verilisp frame to the alist
that can then be attached and populated. The macro WITH-LOCAL-FRAME
lets code access the 'real' declarations within an environment
extended with this frame.

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

    (error 'no-local-frame :hint "This is a compiler error.")))


(defmacro with-local-frame (decls &body body)
  "Run BODY in a global environment including the locally-applied frame from DECLS.

DECLS should be a variable holding the declarations, which is re-bound
within BODY to hold only the 'real' declarations with the local frame
removed and attached to the current environment. The original
environment is restored on leaving BODY."

  ;; ensure we get passed a variable name, not an expression
  (unless (symbolp decls)
    (error 'dsl-error :hint (format nil "Non-symbol ~a passed to WITH-LOCAL-FRAME" decls)))

  ;; extract frame and decls, and run BODY in a suitable environment
  (with-gensyms (real-decls local-frame)
    `(destructuring-bind (,local-frame ,real-decls)
	 (get-local-frame-and-decls ,decls)

       ;; attach local frame to environment
       (with-frame ,local-frame

	 ;; re-declare remaining DECLS (without local frame)
	 (let ((,decls ,real-decls))
	   (declare (ignorable ,decls))

	   ;; run body with these decls
	   ,@body)))))


;;; ---------- Dependencies ----------

(defpass compute-dependencies (form)
  (:documentation "Annotate the environment with the dependencies of FORM.

Methods on this function should update the dependencies of variables,
typically in assignments and binders. The function ADD-DEPENDENCIES
can be called to actually add dependencies to the current frame. The
dependencies added should be 'direct', in the sense that the variables
are used in assignments; the TRAVERSE-DEPENDENCIES function can be
used to trace 'indirect' chains of dependencies.

Methods should also mark variables as read or written to using
MARK-VARIABLE-AS-READ and MARK-VARIABLE-AS-WRITTEN.")
  (:schema over-arguments)
  (:queue expanding)

  ;; return the original form (environments updated in place)
  (:post (lambda (form res)
	   (declare (ignore res))

	   form))

  (:method (form))

  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form

      (with-current-form form
	(with-recover-on-error
	    ;; leave dependencies unchanged on error
	    t

	  (compute-dependencies/form fun args))))))


(defun add-dependencies (n deps)
  "Add variables DEPS as dependencies for N.

A dependency is a variable that's read in assigning values to N."
  (let ((old-deps (variable-property n 'depends-on)))
    (set-variable-property n 'depends-on (union deps old-deps)))

  ;; any variables we depend on are read by definition
  (dolist (m deps)
    (set-variable-property m 'read t)))


(defun mark-variable-as-read (n)
  "Annotate N as having been read."
  (set-variable-property n 'read t))


(defun mark-variables-as-read (ns)
  "Annotate all variables in NS as having been read."
  (mapc #'mark-variable-as-read ns))


(defun variable-read-p (n)
  "Test whether N is accessed as part of a read operation."
  (variable-property n 'read))


(defun mark-variable-as-written (n)
  "Annotate N as having been written to."
  (set-variable-property n 'written t))


(defun mark-variables-as-written (ns)
  "Annotate all variables in NS as having been written to."
  (mapc #'mark-variable-as-written ns))


(defun variable-written-p (n)
  "Test whether N is updated over its extent.

This does not include the assignment of any initial value, only
subsequent updates."
  (variable-property n 'written))


(defun traverse-dependencies (ns)
  "Traverse the dependencies for the variables NS.

NS can be a variable name or a list of variables.

Return the dependencies of the NS, and all the dependencies of those
dependencies, and so on recursively. Constants do not count as
dependencies as they can't be updated."

  ;; get the direct dependencies
  (let ((direct (union-all (mapcar (lambda (n)
				 (variable-property n 'depends-on :default nil))
			       (safe-list ns)))))

    ;; traverse to further dependencies
    (union-all (mapcar (lambda (n)
			 (if (static-constant-p n)
			     nil
			     (union (list n)
				    (variable-property n 'depends-on :default nil))))
		       direct))))


;;; ---------- Variable re-writing ----------

(defpass rewrite-variables (form rewrite)
  (:documentation "Re-write free occurrances of variables in FORM.

The REWRITE alist provides the mapping from variables to their new
forms, which may not be variables at all. Methods should only
rewrite free occurrances, not those that appear under binders.")
  (:schema into-arguments)

  (:method ((form integer))
    form)

  (:method ((form symbol))
    (if-let ((a (assoc form rewrite
		       :key #'symbol-name
		       :test #'string-equal)))
      ;; reference to rewriteable variable, re-write it
      (cadr a)

      ;; leave alone
      form)))


;;; ---------- Type checking and inference ----------

(defpass apply-type-constraints (form)
  (:documentation "Evaluate type constraints to constraining variables in FORM.

This pass is called after type-checking and inference, meaning that
the environment will be populated with explicit and inferred types
and other information. Functions on this method should check this
information to decide whether necessary constraints are met, and
signal warnings or errors appropriately.")
  (:schema over-arguments)
  (:queue typing)

  ;; return the original form overall from the pass
  (:post (lambda (form res)
	   form))

  (:method (form)
    nil)

  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (with-current-form form
	(with-recover-on-error
	    ;; leave the constraints alone on error
	    t

	  (apply-type-constraints/form fun args))))))


(defpass compute-type (form)
   (:documentation "Compute the type of FORM.

Methods on this function should add type constraints to the
environment for the variables they use. ADD-TYPE-CONSTRAINTS adds the
constraint to the environment. Methods for binders should solve
(if possible) these constraints for the locally-declared variables.
Later passes can then assume that the type returned by GET-TYPE
reflects the actual type determined by the type-checker. In
particuler, these types are used by APPLY-TYPE-CONSTRAINTS to ensure
that code it type-correct.

Return the type of FORM. Generally speaking it will not be possible to
firmly determine a type locally for a code fragment, so the type
returned may be general and make use of complex type specifiers that
are resolved in the binders that introduce the variables.")
  (:schema fail-unknown-form)
  (:queue typing)

  (:post (lambda (form ty)
	   ;; ensure the top-pevel type is a module
	   (ensure-subtype ty 'module)

	   ;; save this type for later use
	   (setq *last-module-type* ty)

	   ;; return the original form
	   form)))


;;; ---------- Generalised places ----------

(defpass generalised-place-p (form)
  (:documentation "Test whether FORM is a generalised place.

Generalised places can appear as the target of SETF forms. (In other
languages they are sometimes referred to as *lvalues*.) This is
separate, but related to, their type: a generalised place has a type,
but is also SETF-able.")
  (:schema constant-form nil)

  (:method (form)
    nil))


(defun ensure-generalised-place (form)
  "Ensure FORM is a generalised place."
  (unless (generalised-place-p form)
    (error 'not-synthesisable :hint "Make sure the target of the assignment is a generalised, SETF-able, place")))


;;; ---------- Simple expressions ----------

;;; A simple expression is one that Verilog will understand as being
;;; an expression it can synthesise. These are considerably less
;;; general than Lisp's idea of expressions, so it will be necessary
;;; to transform more complex (but legal) Lisp to take the complicated
;;; bits out of the expressions.

(defpass simple-expression-form-p (form)
  (:documentation "Test whether FORM is a simple expression.")
  (:schema constant-form nil)

  (:method ((n integer))
    t)
  (:method ((s symbol))
    (variable-declared-p s)))


;;; ---------- Representation inference ----------

(defpass infer-representation (form)
  (:documentation "Infer the representations of variables in FORM.

This function usually applies only to binders, and makes use of
dependency and access information to determine the correct
representation for each variable. This may also be influenced by
explicit DECLARE declarations, which should be checked for
consistency with the representation implied by the code.")
  (:schema over-arguments)
  (:queue expanding)

  ;; return the original form (environments updated in place)
  (:post (lambda (form res)
	   (declare (ignore res))

	   form))

  (:method (form)
    nil))


;;; ---------- Elaborating state machines ----------

(defpass elaborate-state-machines (form)
  (:documentation "Expand TAGBODY-based state machines into CASE- and IF-based machines.")
  (:schema into-arguments)
  (:queue transforming)

  (:method (form)
    form))


;;; ---------- Let block coalescence ----------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-recursion-schema into-arguments-float-merge (fun args pass-name)
    "A recursion scheme to float LET and LET* blocks."
    `(destructuring-bind (fargs fenv)
	 (float-merge ,args)
       (list (cons ,fun fargs) fenv))))


(defpass float-let-blocks (form)
  (:documentation "Float nested LET blocks in FORM to the outermost level.

Functions on this method should remove any LET blocks in FORM and
return them to be re-applied at a higher level.

The pass should return a list consisting of the new form and an
environment including all the variables locally declared. When run in
a pass queue, the environment will be discarded.")
  (:queue transforming)
  (:schema into-arguments-float-merge)

  ;; return the final re-written form, applying the environment
  ;; and then discarding it
  (:post (lambda (form res)
	   (declare (ignore form))

	   (destructuring-bind (f env)
	       res
	     (float-apply f env)))))


(defun float-merge (forms)
  "Float LET blocks in FORMS left to right.

Return the re-written FORMS and a merged environment."
  (flet ((pairwise-append (old form)
	   (destructuring-bind (oldbody oldenv)
	       old
	     (destructuring-bind (newbody newenv)
		 (float-let-blocks form)

	       (if (null newbody)
		   ;; body was removed, skip
		   old

		   (list (if (null oldbody)
			     (list newbody)

			     (append oldbody (list newbody)))
			 (if (null newenv)
			     oldenv
			     (add-frame-to-environment newenv oldenv))))))))

    (foldr #'pairwise-append forms (list '() (make-frame)))))


(defun float-apply (body env)
  "Apply declarations in ENV around BODY.

The decls are always applied as LET* regardless of the underlying
block structure, which is safe as long as we've uniquified all
variable names."
  (if (and env
	   (not (null (get-environment-names env))))

      ;; there are variables to apply
      ;; declare the floated declarations around the body
      (let ((newdecls (mapcar (lambda (np)
				(destructuring-bind (n props)
				    np
				  (list n
					(get-environment-property n 'initial-value env))))
			      (decls env))))

	;; add the new decls as a local frame
	(setq newdecls (add-local-frame-to-decls newdecls))
	(with-local-frame newdecls
	  (compute-let-local-frame newdecls)

	  ;; copy properties across from environment
	  (dolist (n (get-frame-names (current-frame)))
	    (set-variable-properties n (copy-list (get-environment-properties n env)))))

	;; always a LET*, never a LET
	`(let* ,newdecls
	   ,body))

      ;; no variables to apply, return the body unchanged
      body))


;;; ---------- PROGN coalescence ----------

(defpass simplify-progn (form)
  (:documentation "Collapse unnecessary PROGN forms in FORM.

PROGN blocks can be introduced in a number of ways to group other
forms. Methods on this function should re-write PROGN forms that
are unnecessarily complicated.

Return the simplified form.")
  (:schema into-arguments)
  (:queue transforming))


;;; ---------- Synthesis ----------

(defpass synthesise (form)
  (:documentation "Synthesise the Verilog for FORM.

FORM will be fully elaborated Core Verilisp with fully populated
environments.

The Verilog synthesised should be send to *STANDARD-OUTPUT*: this
may be redirected by higher-level functions.")
  (:schema fail-unknown-form)
  (:queue synthesising))


;;; ---------- Lispification ----------

(defpass lispify (form)
  (:documentation "Convert FORM to a Lisp expression.")
  (:schema into-arguments)

  (:method (quote &rest args)
    ;; leave quoted lisp expressions alone
    `(quote ,@args)))
