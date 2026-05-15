;;;; Variable declarations and bindings
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

;;; LET and LET* binders. These mainly differ in their typing behaviour,
;;; with the rest of their behaviour being shared.


;;; ---------- Recursion schema ----------

;;; There is a common recursion schema for binders in which a pass cascades
;;; into the declaration initial values and the body, but not into the names
;;; of the variables being declared.

;; TODO: Doesn't handle extra arguments yet

(define-recursion-schema into-decls-and-body (fun args pass-name)
  "A recursion schema that recurses into initial values and body.

The declaration variables are left unchanged."
  (with-gensyms (decls body vdecls decl vbody)
    `(destructuring-bind (,decls &rest ,body)
	 ,args
       (let ((,vdecls (mapcar (lambda (,decl)
				(if (listp ,decl])
				    (list (car ,decl)
					  (apply #',pass-name (cadr ,decl)))

				    ,decl))
			      ,decls))
	     (,vbody (apply #'pass-name (with-implicit-progn ,body))))

	 (,fun ,decls ,@vbody)))))


;;; ---------- Representationas----------

(deftype representation ()
  "The type of variable representations."
  '(member 'register 'wire 'constant 'parameter 'module))


(defun representation-p (rep)
  "Test REP is a valid variable representation."
  (typep rep 'representation))


(defun ensure-representation (rep)
  "Ensure REP is a valid variable representation.

Signal REPRESENTATION-MISMATCH as an error if not."
  (unless (representation-p rep)
    (error 'representation-mismatch :expected '(register wire constant parameter module)
				    :got rep)))


;;; ---------- Local frames ----------

(defun add-decl-to-frame (decl)
  "Add DECL to the local frame.

The declarations appear in the environment in the same order as
they do in DECLS."
  (declare (optimize debug))

  (if (listp decl)
      ;; declare name and initial value
       (destructuring-bind (n v)
	   decl
	 (declare-variable n `((initial-value ,v))))

       ;; declare just name
       (declare-variable decl '())))


(defun compute-let-local-frame (decls)
  "Populate the local frame of DECLS.

The declarations appear in the environment in the same order as
they do in DECLS."
  (unless (null decls)
    (mapc #'add-decl-to-frame decls)))


(defun add-let-local-frame (fun args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    ;; (let ((decls (add-local-frame-to-decls decls)))
    ;;   (break)
    ;;   (compute-let-local-frame decls)

    ;;   ;; return the form
    ;;   `(,fun ,decls
    ;;	     ,@(with-local-frame decls
    ;;		 (mapcar #'add-frames body))))
    (let ((local-decls (add-local-frame-to-decls decls)))
      (with-local-frame local-decls
	(compute-let-local-frame local-decls)

	(let ((fbody (mapcar #'add-frames body)))
	  `(,fun ,decls
		 ,@fbody))))))


(defpassmethod add-frames (let &rest args)
  (add-let-local-frame 'let args))


(defpassmethod add-frames (let* &rest args)
  (add-let-local-frame 'let* args))


;;; ---------- Dependencies ----------

;;; TODO: change dependencies for LET*

(defun compute-let-dependencies (decls)
  "Compute the dependencies of all variables in the current frame."
  (dolist (n (variables-declared-in-current-frame))
    (let ((decl (assoc-decls n decls)))
      (with-current-form decl
	(with-recover-on-error
	    ;; leave dependencies alone on error
	    t

	  (if-let ((v (get-initial-value n)))
	    (add-dependencies n (read-variables v))))))))


(defpassmethod compute-dependencies (let decls &rest body)
  (with-local-frame decls
    (compute-let-dependencies decls)

    (compute-dependencies (with-implicit-progn body))))


(defpassmethod compute-dependencies (let* &rest args)
  (:same-as let))


;;; ---------- Free variables ----------

;;; TODO: change free variables for LET*

(defpassmethod read-variables (let decls &rest body)
  (declare (optimize debug))

  (with-local-frame decls
    (let ((lns (variables-declared-in-current-frame)))

      ;; compute read variables
      (let ((decl-rvs (union-all (mapcar #'read-variables
					 (remove-nulls (mapcar #'get-initial-value lns)))))
	    (body-rvs (read-variables (with-implicit-progn body))))

	;; remove any variables declared in this binder
	(set-difference (union decl-rvs body-rvs) lns)))))


(defpassmethod read-variables (let* &rest args)
  (:same-as let))


;;; ---------- Typechecking ----------

(defun name-in-decl (decl)
  "Extract the name being declared by DECL.

The name is the first element, whether or not DECL is a list."
  (safe-car decl))


(defun compute-let-env ()
  "Compute the types of the declarations in the current frame for a LET.

LET checks all bindings in an environment that doesn't include the
other bindings."
  (declare (optimize debug))

  ;; for LET we detach the current (shallowest) frame and typecheck
  ;; all the declarations in it within the parent environment, adding
  ;; any properties to the detached frame, and then re-attach it
  (with-detached-frame lenv

    (dolist (n (variables-declared-in-frame lenv))
      (with-recover-on-error
	  ;; leave variable alone
	  nil

	;; constrain the variable with whatever information we have
	(let ((ty (or (get-frame-property n 'type lenv :default nil)
		      (if-let ((v (get-frame-property n 'initial-value lenv)))
			(compute-type v))
		      '(unsigned-byte 1))))

	  ;; array and module types are known at construction, so don't need to be inferred
	  (when (or (subtype-p ty 'array)
		    (subtype-p ty 'module))
	    (set-frame-property n 'type ty lenv)

	    ;; modules also are their own representation
	    (when (subtype-p ty 'module)
	      (set-frame-property n 'as 'module lenv)))

	  ;; constrain the variable
	  (add-frame-type-constraint n ty lenv))))))


(defun compute-let*-env ()
  "Compute the types of the declarations in the current frame for a LET*.

LET* adds bindings incrementally, so each can see those that went before."
  (declare (optimize debug))

  ;; for LET* we detach the current (shallowest) frame, attach a new
  ;; empty frame, and incrementally add declarations as we type-check
  ;; them
  (with-detached-frame lenv

    (let ((nenv (make-frame)))
      (with-frame nenv

	(dolist (n (variables-declared-in-frame lenv))
	  (with-recover-on-error
	      ;; leave variable alone
	      nil

	    ;; constrain the variable with whatever information we have
	    (let ((ty (or (get-frame-property n 'type lenv :default nil)
			  (if-let ((v (get-frame-property n 'initial-value lenv)))
			    (compute-type v))
			  '(unsigned-byte 1))))

	      ;; array and module types are known at construction, so don't need to be inferred
	      (when (or (subtype-p ty 'array)
			(subtype-p ty 'module))
		(set-frame-property n 'type ty lenv)

		;; modules also are their own representation
		(when (subtype-p ty 'module)
		  (set-frame-property n 'as 'module lenv)))

	      ;; constrain the variable
	      (add-frame-type-constraint n ty lenv))

	    ;; add the new variable incrementally to the empty frame
	    ;; with the correct properties
	    (declare-variable n (get-frame-properties n lenv))))))))


(defpassmethod compute-type (let decls &rest body)
  (declare (optimize debug))

  (with-local-frame decls
    (compute-let-env)   ; type-check in parent frame

    ;; compute-type the body
    (compute-type (with-implicit-progn body))))


(defpassmethod compute-type (let* decls &rest body)
  (declare (optimize debug))

  (with-local-frame decls
    (compute-let*-env)   ; type-check in progressively expanded frame

    ;; compute-type the body
    (compute-type (with-implicit-progn body))))


(defpassmethod apply-type-constraints (let decls &rest body)
  (declare (optimize debug))

  (with-local-frame decls
    (dolist (n (variables-declared-in-current-frame))
      (with-recover-on-error
	  ;; leave constraints alone on error
	  t

	;; constrain the variable's type (which must be representable)
	(let* ((constraints (get-type-constraints n))
	       (lubty (if constraints (apply #'lurb constraints))))

	  (let ((ty (get-type n)))
	    (if ty
		;; check against provided type
		(unless (subtype-p lubty ty)
		  (warn 'type-mismatch :expected ty
				       :got lubty
				       :hint "Make sure explicit type matches usage"))

		;; update the type with the constrained type
		(progn
		  (set-variable-property n 'type lubty)
		  (setq ty lubty)))

	    ;; ensure the initial value is a valid element
	    (if-let ((v (get-initial-value n)))
	      (progn
		(ensure-subtype (compute-type v) ty)

		;; cascade into any initial values
		(apply-type-constraints v)))))))

    ;; cascade into the body
    (apply-type-constraints (with-implicit-progn body))))


(defpassmethod apply-type-constraints (let* &rest args)
  (:same-as let))


;;; ---------- Representations ----------

(defpassmethod infer-representation (let decls &rest body)
  (declare (optimize debug))

  (with-local-frame decls
    ;; do representation inference on initial values
    (dolist (n (variables-declared-in-current-frame))
      (if-let ((v (variable-property n 'initial-value)))
	(infer-representation v)))

    ;; do representation inference in body
    (infer-representation (with-implicit-progn body))

    ;; do representation inference on local variables
    (dolist (n (variables-declared-in-current-frame))
      (let ((read (variable-property n 'read))
	    (written (variable-property n 'written))
	    (ignored (variable-property n 'ignore))
	    (ignorable (variable-property n 'ignorable)))

	(let ((rep (if written
		       ;; variable is updated, must be a register
		       'register

		       (if read
			   ;; variable is read and not updated
			   (if-let ((v (get-initial-value n)))
			     (cond ((make-array-form-p v)
				    ;; arrays are always registers
				    'register)

				   ((static-p v)
				    ;; static constant, a constant
				    'constant)

				   (t
				    ;; not constant, a wire
				    'wire))

			     ;; no initial value, assume a wire
			     'wire)

			   ;; variable is unused, and is not a module (which we
			   ;; don't report as they're never used directly)
			   (progn
			     (if (and (not (or ignored ignorable))
				      (not (subtype-p (get-type n) 'module)))
				 ;; not marked as ignored/able
				 (warn 'unused-variable :variable n
							:hint "Make sure variable is needed"))

			     ;; represent as a wire
			     'wire)))))

	  (if (and (or read written)
		   ignored)
	      ;; variable is used despire being marked as ignored
	      (warn 'used-variable :variable n
				   :hint "Why is the variable used when marked as ignored?"))

	  ;; check consistency with assigned representation
	  (if-let ((given (get-representation n)))
	    (when (not (eql rep given))
	      (warn 'representation-mismatch :variable n
					     :got rep
					     :expected given
					     :hint "Make sure the explicitly-assigned representation is appropriate"))

	    ;; update representation if none given
	    (set-variable-property n 'as rep)))))))


(defpassmethod infer-representation (let* &rest args)
  (:same-as let))


;;; ---------- Variable re-writing ----------

(defun rewrite-variables-keys (kvs rewrite)
  "Rewrite variables in the values of the key/value pairs KVS using REWRITE."
  (flet ((rewrite-key-value (l kv)
	   (append l (list (car kv) (rewrite-variables (cadr kv) rewrite)))))
    (foldr #'rewrite-key-value (adjacent-pairs kvs) '())))


(defun rewrite-variables-decl (decl rewrite)
  "Re-write the values of DECL using REWRITE."
  (if (listp decl)
      ;; full decl, recurse into key values
      (destructuring-bind (n v &rest keys)
	  decl
	(if keys
	    `(,n ,(rewrite-variables v rewrite) ,(rewrite-variables-keys keys rewrite))

	    ;; no keys to add
	    `(,n ,(rewrite-variables v rewrite))))

      ;; naked declaration, nothing to do
      decl))


(defpassmethod rewrite-variables (let decls &rest body)
  (let* ((rwdecls (mapcar (rcurry #'rewrite-variables-decl rewrite) decls))

	 ;; remove any re-writes referring to shadowed variables
	 (rwnames (mapcar #'name-in-decl rwdecls))
	 (rwrewrite (remove-if (lambda (rw)
				 (member (car rw) rwnames))
			       rewrite))

	 ;; re-write the body with these new re-writes
	 (rwbody (mapcar (rcurry #'rewrite-variables rwrewrite) body)))

    ;; rewrite the form to use re-written decls and the body
    ;; re-written respecting shadowing
    `(let ,rwdecls
       ,@rwbody)))


(defpassmethod rewrite-variables (let* &rest args)
  (:same-as let))


;;; ---------- Macro expansion ----------

(defun expand-macros-decl (decl)
  "Expand macros in the value of DECL."
  (if (listp decl)
      ;; full declaration, expand the value
      (destructuring-bind (n v)
	  decl
	`(,n ,(expand-macros v)))

      ;; naked name, leave it alone
      decl))


(defun expand-let-macros (fun args)
  (destructuring-bind (decls &rest body)
      args
    (let ((newdecls (mapcar #'expand-macros-decl decls))
	  (newbody (mapcar #'expand-macros body)))

      `(,fun ,newdecls
	     ,@newbody))))


;;; TODO: Should this be a recursion schema too?

(defpassmethod expand-macros (let &rest args)
  (expand-let-macros `let args))


(defpassmethod expand-macros (let* &rest args)
  (expand-let-macros `let* args))


;;; ---------- Elaborating state machines ----------

;;; LET and LET* elaborate state machines the same way, but need to
;;; retain their function tag (or do they?)

(defun elaborate-let-state-machines (fun args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (let ((newbody (with-local-frame decls
		     (mapcar #'elaborate-state-machines body))))
      `(,fun ,decls
	     ,@newbody))))


(defpassmethod elaborate-state-machines (let &rest args)
  (elaborate-let-state-machines 'let args))


(defpassmethod elaborate-state-machines (let* &rest args)
  (elaborate-let-state-machines 'let* args))


;;; ---------- Floating ----------

(defun float-initial-values (newenv)
  "Return a list of assignments to be made for the initial values in NEWENV."
  (let ((regs (remove-if (lambda (n)
			   (not (eql (get-representation n) 'register)))
			 (variables-declared-in-current-frame))))
    (mapcar (lambda (n)
	      `(setq ,n ,(get-initial-value n :default 0)))
	    regs)))


(defpassmethod float-let-blocks (let decls &rest body)
  (declare (optimize debug))

  (destructuring-bind (newbody newenv)
      (float-let-blocks (with-implicit-progn body))

    ;; add our declarations to the environment
    (when (null newenv)
      (setq newenv (make-frame)))
    (with-local-frame decls
      ;; add the new declarations to the front of NEWENV
      (add-frame-to-environment (current-frame) newenv t)

      ;; return the re-written body and the new environment
      (if (in-module-context-p)
	  (list newbody newenv)

	  ;; get any initial value assignments to be added
	  (let ((ivs (float-initial-values newenv)))
	    (list (if ivs
		      ;; initial values, prepend them to the new body
		      `(progn
			 ,@ivs
			 ,newbody)

		      ;; no initial values, return the body
		      newbody)
		  newenv))))))


(defpassmethod float-let-blocks (let* &rest args)
  (:same-as let))


;;; ---------- PROGN simplification ----------

(defun simplify-implied-progn (body)
  "Simplify an implied PROGN represented by BODY.

This removes nested PROGN blocks, singleton PROGNs that can be
repalced by a list of forms, and other simplifications needed
by LET and MODULE forms."
  (foldr (lambda (l arg)
	   (if (and (listp arg)
		    (eql (car arg) 'progn))
	       (append l (cdr arg))
	       (append l (list arg))))
	 body
	 '()))


(defun simplify-let-progn (fun args)
  (destructuring-bind (decls &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(,fun ,decls ,@(simplify-implied-progn newbody)))))


(defpassmethod simplify-progn (let &rest args)
  (simplify-let-progn `let args))


(defpassmethod simplify-progn (let* &rest args)
  (simplify-let-progn `let* args))


;;; ---------- Synthesis ----------

(defun array-type-p (ty)
  "Test whether TY is an array type."
  (subtype-p ty 'array))


(defun synthesise-register (n)
  "Synthesise a register N within a LET block."
  (declare (optimize debug))

  (let ((v (get-initial-value n :default 0)))
    (as-literal "reg ")
    (let* ((type (get-type n))
	   (width (if (array-type-p type)
		      ;; width is the width of the element type
		      (bitwidth (element-type-of-array type))

		      ;; width is of the type itself
		      (bitwidth type))))

      ;; (if (and (fixed-width-p type)
      ;;	       (not (unsigned-byte-p type)))
      ;;	  (as-literal "signed "))

      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width)
	(as-literal " - 1 : 0 ] "))
      (synthesise n)
      (if v
	  (if (make-array-form-p v)
	      ;; synthesise the array bounds and initialisation
	      (synthesise-array-init n v)

	      ;; synthesise the assignment to the initial value
	      (progn
		  (as-literal " = ")
		  (synthesise v))))
      (as-literal ";"))))


(defun synthesise-wire (n)
  "Synthesise a wire N a LET block."
  (let ((v (get-initial-value n :default 0)))
    (as-literal "wire ")
    (let* ((type (get-type n))
	   (width (if (array-type-p type)
		      ;; width is the width of the element type
		      (bitwidth (element-type-of-array type))

		      ;; width is of the type itself
		      (bitwidth type))))

      ;; (if (and (fixed-width-p type)
      ;;	       (not (unsigned-byte-p type)))
      ;;	  (as-literal "signed "))

      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width)
	(as-literal " - 1 : 0 ] "))
      (synthesise n)
      (if (make-array-form-p v)
	  ;; synthesise the array constructor
	  (synthesise-array-init n v)

	  ;; synthesise the assignment to the initial value if there is one
	  (if v
	      (if (static-constant-p v)
		  (let ((iv (ensure-static v)))
		    (unless (= iv 0)
		      ;; initial value isn't statially zero, synthesise
		      (as-literal " = ")
		      (synthesise v)))

		  ;; initial value is an expression, synthesise
		  (progn
		    (as-literal " = ")
		    (synthesise v)))))
      (as-literal";"))))


(defun synthesise-constant (n)
  "Synthesise a constant N within a LET block.

Constants turn into local parameters."
  (let ((v (get-initial-value n :default 0)))
    (as-literal "localparam ")
    (synthesise n)
    (as-literal " = ")
    (synthesise v)
    (as-literal ";")))


(defun synthesise-module-instanciation (n)
  "Synthesise N as a module instanciation."
  (let ((v (get-initial-value n)))
    (synthesise v)))


(defun decl-rhs-form-p (&optional (form (current-form)))
  "Test that the current form is a valid initial value for assignment.

Valid RHSs are either null, array or object constructors, or simple expressions."
  (or (null form)
      (make-array-form-p form)
      (make-instance-form-p form)
      (simple-expression-form-p form)))


(defun synthesise-decl (decl)
  "Synthesise DECL."
  (declare (optimize debug))

  (with-current-form decl
    (let* ((n (name-in-decl decl))
	   (v (get-initial-value n)))

      (progn
	;; check the RHS is valid
	(with-current-form v
	  (unless (decl-rhs-form-p)
	    (error 'not-synthesisable :hint "Initial value must be a simple expression")))

	;; synthesise the different kinds of declaration in Verilog
	(case (get-representation n)
	  ('module
	   (synthesise-module-instanciation n))
	  ('constant
	   (synthesise-constant n))
	  ('register
	   (synthesise-register n))
	  ('wire
	   (synthesise-wire n))
	  (t
	   (synthesise-register n)))))))


(defpassmethod synthesise (let decls &rest body)
  (declare (optimize debug))

  (with-local-frame decls
    ;; synthesise the constants and registers
    (as-block-forms decls :process #'synthesise-decl)

    (if (> (length decls) 0)
	(as-blank-line))

    ;; synthesise the body
    (as-block-forms body)))


(defpassmethod synthesise (let* &rest args)
  (:same-as let))
