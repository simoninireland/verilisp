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


(defpass written-variables (form)
  (:documentation "Return all variables in FORM that are written to.

This function is used for constructing dependencies of variables.
Return the set of variables as a list.")
  (:schema into-arguments-union))


;;; ---------- Macro expansion ----------

(defun expand-descend (fun args)
  "Expand macros in ARGS when FUN applied."
  `(,fun ,@(remove-nulls (mapcar (lambda (arg)
				   (unless (null arg)
				     (expand-macros arg)))
				 args))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-recursion-schema into-arguments-macros (fun args)
    "A recursion schema that expands a form as a macro."
    (if (and (symbolp fun) ; need this to avoid expanding quoted lists of numbers
	     (macro-declared-p fun))
	;; macro is expandable
	(let ((realfun (variable-property fun 'initial-value)))
	  ;; expand the macro in a nested environment that will contain
	  ;; any locally-declared macros
	  (with-new-frame
	    (let ((expansion (apply realfun args)))

	      ;; expand the expansion
	      (expand-macros expansion))))

	;; macro is not expandable, descend into the form
	(expand-descend fun args))))


(defpass expand-macros (form)
  (:documentation "Expand macros in FORM.")
  (:queue expanding)
  (:schema into-arguments-macros)

  (:passmethod (form)
    form))


(defun expand-macros-in-environment (form &optional (f *global-environment*))
  "Recursively expand all macros in FORM in an environment.

This function expands all macros to convert FORM into Core Verilisp.
Macros are taken from the global environment unless a specific
frame F is provided.

Return the expanded form."
  (in-frame f
    (expand-macros form)))


;;; ---------- Applying and removing frames ----------

;;; Lisp binders typically consist of a body with a set of declarations
;;; (decls) that are brought into lexical scope. This is inconvenient for
;;; processing, so we replace the decls list with a single frame that
;;; holds all the same information in a more easily-processed form.
;;;
;;; This happens /before/ any further processing, but /after/ macro
;;; expansion, so that macros all see a standard Lisp source format.
;;;
;;; We need to be able to reverse this transformation to re-produce
;;; source code.

(defpass add-frames (form)
  (:documentation "Add frames to FORM.

Frames are used to maintain the lexical environment for the compiler.
Methods on this function should construct frames, associate them with
the appropriate binders so they can be applied in other passes, and
populate them with the variables being declared. These declarations
will then be used by, and extended by, other passes.")
  (:schema into-arguments)
  (:queue expanding)

  (:passmethod (form)
	       form))


(defun add-decl-to-frame (decl f)
  "Add DECL to frame F."
  (declare (optimize debug))

  (if (listp decl)
      ;; declare name and initial value
      (destructuring-bind (n v)
	  decl
	(declare-environment-variable n `((initial-value ,v)) f))

      ;; declare just name
      (declare-environment-variable decl `() f)))


(defun build-frame-from-decls (decls &optional (f (make-frame)))
  "Populate frame F with all the variables declared in DECLS.

If F is omitted, use a new empty frame.

Return the populated frame."
  (unless (null decls)
    (mapc (rcurry #'add-decl-to-frame f) decls))

  f)


(defun build-decls-from-frame (f)
  "Extract a list of decls from F."
  (let (decls)
    (dolist (n (get-frame-names f))
      (if-let ((v (get-frame-property n 'initial-value f :default nil)))
	(appendf decls `((,n ,v)))

	(appendf decls `(,n))))

    decls))


(defun get-local-frame (decls)
  "Retrieve the local frame from DECLS.

DECLS should be a frame: if not, then the frame-building operation
has somehow been missed."
  (if (typep decls 'frame)
      decls

      (error 'no-local-frame :hint "This is a compiler error.")))


(defmacro with-local-frame (decls &body body)
  "Run BODY in a global environment including the locally-applied frame from DECLS.

DECLS should be a variable holding the declarations, which should be a frame.
The frame is installed and used for BODY.

It is an error to try this on a form that doesn't have DECLS as a frame."

  ;; ensure we get passed a variable name, not an expression
  (unless (symbolp decls)
    (error 'dsl-error :hint (format nil "Non-symbol ~a passed to WITH-LOCAL-FRAME" decls)))

  ;; extract and install the local frame and run BODY in a suitable environment
  `(with-frame (get-local-frame ,decls)

     ;; run the body in this environment
     ,@body))


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
  (:queue typing)

  ;; return the original form (environments updated in place)
  (:post #'return-original-form)

  (:passmethod (form))

  (:passmethod ((form list))
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
  (mark-variables-as-read deps))


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

This does not include the setting of any initial value, only
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


;;; ---------- Representation inference ----------

(defpass infer-representation (form)
  (:documentation "Infer the representations of variables in FORM.

This function usually applies only to binders, and makes use of
dependency and access information to determine the correct
representation for each variable. This may also be influenced by
explicit DECLARE declarations, which should be checked for
consistency with the representation implied by the code.")
  (:schema over-arguments)
  (:queue typing)

  ;; return the original form (environments updated in place)
  (:post #'return-original-form)

  (:passmethod (form)
    form))


;;; ---------- Variable re-writing ----------

(defpass rewrite-variables (form rewrite)
  (:documentation "Re-write free occurrances of variables in FORM.

The REWRITE alist provides the mapping from variables to their new
forms, which may not be variables at all. Methods should only
rewrite free occurrances, not those that appear under binders.")
  (:schema into-arguments)

  (:passmethod ((form integer))
	       form)

  (:passmethod ((form symbol))
	       (if-let ((a (assoc form rewrite
				  :key #'symbol-name
				  :test #'string-equal)))
		 ;; reference to rewriteable variable, re-write it
		 (cadr a)

		 ;; leave alone
		 form)))


;;; ---------- Type checking and inference ----------

(defvar *computed-type* nil
  "Variable holding the type returned by the most recent TYPING pass.")


(defpass compute-type (form)
  (:documentation "Compute the type of FORM.

Verilisp uses a constraint-based type inference system, meaning that a
form constrains -- but doe not strictly determine -- its type based in
the types of its sub-terms, whose types may themselves not be known at
the time of checking.

Methods on this function should work out the type of FORM as a type
expression. They may also apply constraints to any variables, which
will be resolved when typing that variable's binder in the
COMPUTE-VARIABLE-TYPES pass. They do not need to annotate variables
read or written, which is done by the COMPUTE-DEPENDENCIES pass.

The disadvantage of this approach is that type errors are caught where the
variable is declared, not at the proximate cause of the error.

Returns a type expression.")
  (:schema fail-unknown-form)
  (:queue typing)
  (:queue-position :prepend)

  (:post (lambda (form ty)
	   ;; save this type for later use
	   (setq *computed-type* ty)

	   ;; return the original form
	   form)))


(defpass check-all-variables-typed (form)
  (:documentation "Check that all variables in FORM have types.

This shouldn't be needed: the type rules should ensure that all
variables either have types declared ir types inferred (or both).
But just to be sure, this pass checks that there are type
attributes attached to all variables in all frames.")
  (:schema over-arguments)
  (:queue typing)

  ;; return the original form
  (:post #'return-original-form)

  ;; no action on literals or references
  (:passmethod ((form integer))
	       nil)
  (:passmethod ((form symbol))
	       nil))


;;; ---------- Generalised places ----------

;;; Generalised places are Lisp's version of lvalues: things that can be
;;; assigned to. They usually appear in the same way as they are
;;; accessed, but as the first element of a SETF.
;;;
;;; Places appearing in the first place of a SETF have variables that
;;; are written to as well as read from, and we need functions to
;;; return these.

(defpass generalised-place-p (form)
  (:documentation "Test whether FORM is a generalised place.

Generalised places can appear as the target of SETF forms. (In other
languages they are sometimes referred to as *lvalues*.) This is
separate, but related to, their type: a generalised place has a type,
but is also SETF-able.")
  (:schema constant-form nil)

  (:passmethod (form)
	       nil))


(defun ensure-generalised-place (form)
  "Ensure FORM is a generalised place."
  (unless (generalised-place-p form)
    (error 'not-synthesisable :hint "Make sure the target of the assignment is a generalised, SETF-able, place")))


(defpass read-variables-setf (form)
  (:documentation "Return all the variables accessed by FORM in a SETF.

This function is called when FORM is a generalised place in the
assignment (first) position of a SETQ (or SETQ).

Return the set of read variables."))


(defpass written-variables-setf (form)
  (:documentation "Return all the variables updated by FORM in a SETF.

This function is called when FORM is a generalised place in the
assignment (first) position of a SETQ (or SETQ).

Return the set of variables written to."))


(defgeneric compute-type-setf (selector value &rest selargs)
  (:documentation "Compute the type of an assignment.

This function is called when forms of the type
(SETF (SEL . SELARGS) VALUE) are encountered. The lambda
list is the same as for functions defining new generalised places
in Lisp (which Verilisp doesn't yet support).

Return the type of the application."))


;;; ---------- Simple expressions ----------

;;; A simple expression is one that Verilog will understand as being
;;; an expression it can synthesise. These are considerably less
;;; general than Lisp's idea of expressions, so it will be necessary
;;; to transform more complex (but legal) Lisp to take the complicated
;;; bits out of the expressions and into assignments to wires.

(defpass simple-expression-p (form)
  (:documentation "Test whether FORM is a simple expression.")
  (:schema constant-form nil)

  (:passmethod ((n integer))
	       t)
  (:passmethod ((s symbol))
	       (variable-declared-p s)))


;;; ---------- Elaborating state machines ----------

(defpass elaborate-state-machines (form)
  (:documentation "Expand TAGBODY-based state machines into CASE- and IF-based machines.")
  (:schema into-arguments)
  (:queue transforming)

  (:passmethod (form)
    form))


;;; ---------- Let block coalescence ----------

(define-recursion-schema into-arguments-float-merge (fun args)
  "A recursion scheme to float LET and LET* blocks."
  (destructuring-bind (fargs fenv)
      (float-merge args)
    (list (cons fun fargs) fenv)))


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
      `(let* ,env
	 ,body)

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
q
FORM should be fully elaborated Core Verilisp with fully populated
environments.

The Verilog synthesised by tis function should be send to
*STANDARD-OUTPUT*: this may be redirected by higher-level functions.")
  (:schema fail-unknown-form)
  (:queue synthesising))


;;; ---------- Lispification ----------

(defpass lispify (form)
  (:documentation "Convert FORM to a Lisp expression.")
  (:schema into-arguments)

  (:passmethod (quote &rest args)
	       ;; leave quoted lisp expressions alone
	       `(quote ,@args)))
