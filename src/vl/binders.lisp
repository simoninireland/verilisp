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


;;; ---------- Recursion schemata ----------

;;; These new schemata operations rewcurse into the initial values and the
;;; body, but skip the variable names.

;;; into-arguments

(defun into-arguments-let (fun args &key pass-name option extra)
  "The commonality for LET and LET* schemata."
  (declare (ignore option))

  (destructuring-bind (f &rest body)
      args

    ;; process initial values
    (dolist (n (get-frame-names f))
      ;; process initial values
      (if-let ((iv (get-frame-property n 'initial-value f)))
	(set-frame-property n 'initial-value
			    (apply pass-name (cons iv extra))
			    f)))

    ;; process body
    (let ((newbody (with-local-frame f
		     (apply pass-name (cons (with-implicit-progn body) extra)))))
      `(,fun ,f
	     ,@newbody))))


(defmethod into-arguments ((fun (eql 'let)) args &key pass-name option extra)
  (into-arguments-let 'let args :pass-name pass-name :option option :extra extra))
(defmethod into-arguments ((fun (eql 'let*)) args &key pass-name option extra)
  (into-arguments-let 'let* args :pass-name pass-name :option option :extra extra))


;;; over-arguments

(defmethod over-arguments ((fun (eql 'let)) args &key pass-name option extra)
  (declare (ignore option))

  (destructuring-bind (f &rest body)
      args

    ;; process initial values
    (dolist (n (get-frame-names f))
      ;; process initial values
      (if-let ((iv (get-frame-property n 'initial-value f)))
	(apply pass-name (cons iv extra))))

    ;; process body
    (with-local-frame f
      (apply pass-name (cons (with-implicit-progn body) extra)))))


(defmethod over-arguments ((fun (eql 'let*)) args &key pass-name option extra)
  (over-arguments 'let args :pass-name pasiname :option option :extra extra))


;;; into-arguments-macros

;;; This schemata is specific to EXPAND-MACROS and will be applied
;;; before the ADD-FRAMES pass, so need to work on decls as lists, not
;;; as frames

(defun into-arguments-macros-let (fun args &key pass-name option extra)
  "The commonality for LET and LET* schemata.

Because this is called in the expansion pass the first argument will
be a list of declarations, not a frame (which is applied later)."
  (declare (ignore option) (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (let ((newdecls (mapcar (lambda (decl)
			      (if (listp decl)
				  (list (car decl)
					(apply pass-name (cons (cadr decl) extra)))
				  decl))
			    decls))
	  (newbody (mapcar (apply #'rcurry `(,pass-name ,@extra)) body)))

      `(,fun ,newdecls
	     ,@newbody))))


(defmethod into-arguments-macros ((fun (eql 'let)) args &key pass-name option extra)
  (into-arguments-macros-let 'let args :pass-name pass-name :option option :extra extra))
(defmethod into-arguments-macros ((fun (eql 'let*)) args &key pass-name option extra)
  (into-arguments-macros-let 'let* args :pass-name pass-name :option option :extra extra))


;;; ---------- Representations----------

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

;;; We change a LET or LET* of the form (LET (decls) body ...) into one that
;;; has an environment as its only decl entry

(defun add-let-local-frame (fun args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (let ((local-frame (build-frame-from-decls decls)))
      ;; add frames to the body in this new environment
      (with-local-frame local-frame

	;; return the binder with the frame as its decls
	(let ((fbody (mapcar #'add-frames body)))
	  `(,fun ,local-frame
		 ,@fbody))))))


(defpassmethod add-frames (let &rest args)
  (add-let-local-frame 'let args))


(defpassmethod add-frames (let* &rest args)
  (add-let-local-frame 'let* args))


;;; ---------- Dependencies ----------

;;; TODO: change dependencies for LET*

(defpassmethod compute-dependencies (let f &rest body)
  (with-local-frame f
    (dolist (n (variables-declared-in-current-frame))
      (if-let ((v (get-initial-value n)))
	(add-dependencies n (read-variables v))))

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

;;; TODO: Need to consider representability here -- maybe in LUB?

(defun constrain-initial-value (n nf f)
  "Add any constraints on N from is initial value.

The types are computed in frame F and added to frame NF, which should be
the frame declaring N."
  (declare (optimize debug))

  (if-let ((iv (get-frame-property n 'initial-value nf)))
    ;; we have an initial value, use it for constraints
    (let ((ity (in-frame f
		 (compute-type iv))))

      (if (or (subtype-p ity 'array)
	      (subtype-p ity 'module))
	  (progn
	    ;; modules and arrays are fully elaborated and don't need to be inferred
	    (set-frame-property n 'type ity nf)

	    ;; modules are their own representation
	    (when (subtype-p ity 'module)
	      (set-frame-property n 'as 'module nf)))

	  (progn
	    ;; add the type to the constraints
	    ;;(add-frame-type-constraint n ity nf)
	    ;; TODO: Fix when we do more precise typing
	    (unless (get-frame-property n 'type nf :default nil)
	      (set-frame-property n 'type (get-compiler-flag 'default-variable-type) nf)))))

    ;; otherwise use the default type
    (unless (get-frame-property n 'type nf :default nil)
      (set-frame-property n 'type (get-compiler-flag 'default-variable-type) nf))))


(defun compute-let-env ()
  "Compute the types of the declarations in the current frame for a LET.

LET checks all bindings in an environment that doesn't include the
other bindings."
  (declare (optimize debug))

  ;; for LET we detach the current (shallowest) frame and typecheck
  ;; all the declarations in it within the parent environment, adding
  ;; any properties to the detached frame, and then re-attach it
  (with-detached-frame lenv

    (let ((f (current-frame)))
      (dolist (n (variables-declared-in-frame lenv))
	(constrain-initial-value n lenv f)))))


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

	(let ((f (current-frame)))
	  (dolist (n (variables-declared-in-frame lenv))
	    (constrain-initial-value n lenv f)

	    ;; declare the variable ready for the next one
	    (declare-variable n (get-frame-properties n lenv))))))))


(defpassmethod compute-type (let f &rest body)
  (declare (optimize debug))

  (with-local-frame f
    (compute-let-env)

    ;; the type is the type of the body
    (compute-type (with-implicit-progn body))))


(defpassmethod compute-type (let* f &rest body)
  (declare (optimize debug))

  (with-local-frame f
    (compute-let*-env)

    ;; the type is the type of the body
    (compute-type (with-implicit-progn body))))


(defpassmethod check-all-variables-typed (let f &rest body)
  (with-local-frame f
    (dolist (n (get-frame-variable-names f))
      (unless (get-frame-property n 'type f :default nil)
	(error 'compiler-error :hint (format nil "Variable ~s has no type" n))))

    (check-all-variables-typed (with-implicit-progn body))))


(defpassmethod check-all-variables-typed (let* f &rest body)
  (:same-as let))


;;; ---------- Representations ----------

(defpassmethod infer-representation (let f &rest body)
  (declare (optimize debug))

  (with-local-frame f
    ;; do representation inference on local variables
    (dolist (n (get-frame-variable-names f))
      (let ((read (variable-property n 'read))
	    (written (variable-property n 'written))
	    (ignored (variable-property n 'ignore))
	    (ignorable (variable-property n 'ignorable)))

	(let ((rep (if written
		       ;; variable is updated, must be a register
		       ;; TODO: Is this true? -- or only by default?
		       'register

		       (if read
			   ;; variable is read and not updated
			   (if-let ((v (get-initial-value n)))
			     (cond ((make-array-form-p v)
				    ;; arrays are always registers
				    ;; TODO: Is this true?
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
	    (set-variable-property n 'as rep)))))

    ;; do representation inference in body
    (infer-representation (with-implicit-progn body))))


(defpassmethod infer-representation (let* &rest args)
  (:same-as let))


;;; ---------- Variable re-writing ----------

(defpassmethod rewrite-variables (let f &rest body)
  (declare (optimize debug))

  (dolist (n (get-frame-names f))
    (if-let ((iv (get-frame-property n 'initial-value f :default nil)))
      (let ((rv (rewrite-variables iv rewrite)))
	(set-frame-property n 'initial-value rv f))))

  (let* (;; remove any re-writes referring to shadowed variables
	 (rwnames (get-frame-names f))
	 (rwrewrite (remove-if (lambda (rw)
				 (member (car rw) rwnames))
			       rewrite))

	 ;; re-write the body with these new re-writes
	 (rwbody (rewrite-variables (with-implicit-progn body) rwrewrite)))

    ;; rewrite the form to use re-written decls and the body
    ;; re-written respecting shadowing
    `(let ,f
       ,@rwbody)))


;;; TODO: Change rules for LET*

(defpassmethod rewrite-variables (let* &rest args)
  (:same-as let))


;;; ---------- Floating ----------

(defun float-initial-values (newenv)
  "Return a list of assignments to be made for the initial values in NEWENV.

This creates SETQ assignments for each initial value -- but /not/ for
arrays, which can't be re-assigned and just get declared once."
  (let ((regs (remove-if (lambda (n)
			   (or (get-frame-property n 'floated newenv)
			       (not (eql (get-representation n) 'register))
			       (null (get-initial-value n))
			       (array-type-p (get-type n))))
			 (variables-declared-in-current-frame))))

    ;; mark variables as floated, to prevent further re-assignment if
    ;; they're floated further
    (mapc (lambda (n)
	    (set-frame-property n 'floated t newenv)
	    (set-frame-property n 'initial-value nil newenv))
	  regs)

    ;; return the initial assignments, to be inserted in-place
    (mapcar (lambda (n)
	      `(setq ,n ,(get-initial-value n)))
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

      ;; The initial values are written here for all non-top-level LET
      ;; blocks, so that they're assigned correctly relative to the
      ;; expected semantics. Top-level LET blocks (ouside @ blocks)
      ;; aren't floating any further, so their initial values can stay
      ;; where they are (they're also not marked as floated,
      ;; obviously).
      (let ((ivs (float-initial-values newenv)))

	;; return the re-written body and the new environment
	(list (if ivs
		  ;; initial values, prepend them to the new body
		  `(progn
		     ,@ivs
		     ,newbody)

		  ;; no initial values, return the body
		  newbody)
	      newenv)))))


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

;;; TODO: All this should live in arrays.lisp

(defun displaced-array-p (form)
  "Test whetehr FORM is a MAKE-ARRAY for a displaced array.

Displaced arrays are simply renamings of underlying arrays and so
don't need their own storage."
  (when (eql (car form) 'make-array)
    (destructuring-bind (shape &key
				 initial-element initial-contents
				 element-width element-type
				 displaced-to
				 displaced-index-offset
				 conformal
				 displaced-offset)
	(cdr form)
      (not (null displaced-to)))))


(defun synthesise-register (n)
  "Synthesise a register N within a LET block."
  (declare (optimize debug))

  (let ((v (get-initial-value n)))
    (let ((type (get-type n)))

      ;; only synthesise a declaration for a things that aren't
      ;; displaced arrays (which will be transformed)
      (when (or (not (array-type-p type))
		(not (displaced-array-p v)))

	(let ((width (if (array-type-p type)
			 ;; width is the width of the element type
			 (bitwidth (array-type-element-type type))

			 ;; width is of the type itself
			 (bitwidth type))))

	  (as-literal "reg ")
	  ;; (if (and (fixed-width-p type)
	  ;;	       (not (unsigned-byte-p type)))
	  ;;	  (as-literal "signed "))

	  (when (or (not (numberp width))
		    (> width 1))
	    ;; we have a width (or a width expression)
	    (as-literal "[ ")
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
	  (as-literal ";"))))))


(defun synthesise-wire (n)
  "Synthesise a wire N a LET block."
  (let ((v (get-initial-value n :default 0)))
    (as-literal "wire ")
    (let* ((type (get-type n))
	   (width (if (array-type-p type)
		      ;; width is the width of the element type
		      (bitwidth (array-type-element-type type))

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
		      ;; initial value isn't statically zero, synthesise
		      (as-literal " = ")
		      (synthesise v)))

		  ;; initial value is an expression, synthesise
		  (progn
		    (as-literal " = ")
		    ;; this is a bit of a hack, to ensure that the
		    ;; value is synthesised in an expression context
		    ;; to expand (and constrain) IF and CASE correctly
		    ;;
		    ;; TODO: It'd obviously be better if this was an
		    ;; expresion context in its own right
		    (with-current-form '(setf n v)
		      (synthesise v))))))
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
      (simple-expression-p form)))


(defun synthesise-decl (decl)
  "Synthesise DECL."
  (declare (optimize debug))

  (with-current-form decl
    (let* ((n (safe-car decl))
	   (v (get-initial-value n)))

      (progn
	;; check the RHS is valid
	(with-current-form v
	  (unless (decl-rhs-form-p)
	    (error 'not-synthesisable :hint "Initial value must be a simple expression")))

	;; synthesise the different kinds of declaration in Verilog
	(case (get-representation n)
	  (module
	   (synthesise-module-instanciation n))
	  (constant
	   (synthesise-constant n))
	  (register
	   (synthesise-register n))
	  (wire
	   (synthesise-wire n))
	  (t
	   (synthesise-register n)))))))


(defpassmethod synthesise (let f &rest body)
  (declare (optimize debug))

  (with-local-frame f
    ;; synthesise the constants and registers
    (let ((decls (build-decls-from-frame f)))
      (as-block-forms decls :process #'synthesise-decl)

      (when (> (length decls) 0)
	(as-blank-line)))

    ;; synthesise the body
    (as-block-forms body)))


(defpassmethod synthesise (let* &rest args)
  (:same-as let))


;;; ---------- Lispification ----------

(defpassmethod lispify (let f &rest body)
  (let ((decls (build-decls-from-frame f))
	(newbody (with-local-frame f
		   (lispify (with-implicit-progn body)))))
    `(let ,decls
       ,newbody)))


(defpassmethod lispify (let* f &rest body)
  (let ((decls (build-decls-from-frame f))
	(newbody (with-local-frame f
		   (lispify (with-implicit-progn body)))))
    `(let* ,decls
       ,newbody)))
