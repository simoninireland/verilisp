;; Variable declarations and bindings
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


(defun width-can-store-p (w ty env)
  "Test whether W bits can accommodate the values of TY in ENV."
  (>= (eval-in-static-environment w env)
      (eval-in-static-environment (bitwidth ty env) env)))


(defun ensure-width-can-store (w ty env)
  "Ensure that W bits can accommodate the values of TY in ENV."
  (unless (width-can-store-p w ty env)
    (warn 'width-mismatch :expected (bitwidth ty env) :got w)))


;; ---------- Representationa ----------

(deftype representation ()
  "The type of variable representations.

Valid representations in LET forms are REGISTER, WIRE, or CONSTANT.
(PARAMETER is also valid from MODULE forms.)"
  '(member 'register 'wire 'constant))


(defun representation-p (rep)
  "Test REP is a valid variable representation."
  (typep rep 'representation))


(defun ensure-representation (rep)
  "Ensure REP is a valid variable representation.

Signal REPRESENTATION-MISMATCH as an error if not."
  (unless (representation-p rep)
    (error 'representation-mismatch :expected (list 'register 'wire 'constant) :got rep)))


;; ---------- Local frames ----------

(defmethod add-frames-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args
    (add-local-frame-to-decls decls)

    ;; return the form
    `(let ,decls
       ,@(with-local-frame decls
	   (mapcar #'add-frames body)))))


;; ---------- Typechecking ----------

(defun name-in-decl (decl)
  "Extract the name being declared by DECL.

The name is the first element, whether or not DECL is a list."
  (safe-car decl))


(defun typecheck-decl (decl)
  "Extend ENV with the variable declared in DECL."
  (declare (optimize debug))

  (with-current-form decl
    (with-recover-on-error
	;; on error, return the most general and innocuous result to
	;; allow us to continue
	(set-variable-properties (safe-car decl) `((type (unsigned-byte ,*default-register-width*))
						   (as register)
						   (initial-value 0)
						   (type-constraints (unsigned-byte ,*default-register-width*))))

      (if (listp decl)
	  ;; full declaration
	  (destructuring-bind (n v)
	      decl
	    (ensure-variable-declared n)

	    (let ((type (variable-property n 'type :default nil)))
	      ;; initial inferred type
	      (let ((ity (if type
			     (expand-type-parameters type)

			     ;; pick the narrowest type so it can be widened as needed
			     '(unsigned-byte 1))))

		;; typecheck initial value
		(let ((vty (typecheck v)))
		  (if type
		      ;; type provided, ensure it works
		      (ensure-subtype vty type)

		      ;; no type provided, infer from the value
		      (setq ity vty)))

		(if type
		    (set-variable-property-unless-set n 'type type))
		(set-variable-properties-unless-set n `((inferred-type ,ity)
							(as register)
							(initial-value ,v)
							(type-constraints (,ity)))))))

	  ;; "naked" declaration
	  (set-variable-properties-unless-set decl `((inferred-type (unsigned-byte 1))
						     (type-constraints ((unsigned-byte 1)))
						     (as register)
						     (initial-value 0)))))))


(defun typecheck-env (decls)
  "Type-check the declarations DECLS to extend the current environment."
  (mapc #'typecheck-decl decls))


(defun typecheck-constraints (constraints)
  "Return the type satifying CONSTRAINTS.

Each constraint is a type needed by some operation in the scope of a
variable. The inferred type is the widest type (least upper-bound)
that can accommodate all these constraints, assuming that there is
one.

The resulting type must be representable."
  (foldr #'lurb constraints nil))


(defun typecheck-infer-decl (decl)
  "Update the type for DECL.

This matches stated and inferred types, and updates the environment
with the final type."
  (declare (optimize debug))

  ;; resolve type constraints
  (let* ((n (name-in-decl decl))
	 (constraints (variable-property n 'type-constraints))
	 (inferred-type (typecheck-constraints constraints)))

    ;; check inferred type against any explicit type
    ;; this will signal a problem but not fail the type-checking
    ;; pass, to allow for systems that don't care about precision
    (if-let ((ty (variable-property n 'type :default nil)))
      (progn
	(ensure-subtype inferred-type ty)

	;; the type we use is the one supplied
	(setq inferred-type ty))

      ;; no type provided, note that we inferred it
      (warn 'type-inferred :variable n
			   :inferred inferred-type))

    (set-variable-property n 'type inferred-type)))


(defun typecheck-infer-decls (decls)
  "Infer types on all DECLS."
  (mapc #'typecheck-infer-decl decls))


(defmethod typecheck-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))
  (let ((decls (car args))
	(body (cdr args)))

    (with-local-frame decls
      (typecheck-env decls)

      ;; typecheck the body
      (prog1
	  (typecheck `(progn ,@body))

	;; infer any types based on constraints
	(typecheck-infer-decls decls)))))


;; ---------- Dependencies ----------

(defun dependencies-decl (decl)
  "Calculate the dependencies for DECL.

The dependencies are all the other variables that appear on the
right-hand side of an assignment."
  (with-current-form decl
    (with-recover-on-error
	;; on error, leave dependencies alone
	nil

	(if (listp decl)
	    (destructuring-bind (n v &key &allow-other-keys)
		decl

	      (let ((fvs (remove-if #'static-constant-p (free-variables v)))
		    (depends-on (variable-property n 'depends-on :default nil)))
		(set-variable-property n 'depends-on (union depends-on fvs))))))))


(defmethod dependencies-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args

    (with-local-frame decls
      ;; add dependencies for declared variables
      (mapc #'dependencies-decl decls)

      ;; process the body in this frame, with these dependencies
      (dependencies `(progn ,@body)))))



;; ---------- Free variables ----------

(defun free-variables-decl (decl)
  "Calculate the free variables for DECL.

The free variables are all the variables that appear on the
right-hand side of an assignment."
  (if (listp decl)
      (destructuring-bind (n v)
	  decl
	(declare (ignore n))

	(remove-if #'static-constant-p (free-variables v)))))


(defmethod free-variables-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (with-local-frame decls
      (let ((lns (variables-declared-in-current-frame))
	    (body-fvs (foldr #'union (mapcar #'free-variables body) '()))
	    (decl-fvs (foldr #'union (mapcar #'free-variables-decl decls) '())))

	(set-difference (union body-fvs decl-fvs) lns)))))


(defmethod updated-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (with-local-frame decls
      (let ((lns (variables-declared-in-current-frame))
	    (body-fvs (foldr #'union (mapcar #'updated-variables body) '())))

	(set-difference body-fvs lns)))))


;; ---------- Variable re-writing ----------

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


(defmethod rewrite-variables-sexp ((fun (eql 'let)) args rewrite)
  (destructuring-bind (decls &rest body)
      args
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
	 ,@rwbody))))


;; ---------- Macro expansion ----------

(defun expand-macros-key (l kv)
  "Expand macros in the value part of a key-value pair KV to build L."
  (destructuring-bind (k v)
      kv
    (let ((nv (expand-macros v)))
      (append l (list k nv)))))


(defun expand-macros-decl (decl)
  "Expand macros in the value of DECL."
  (if (listp decl)
      ;; full declaration, expand the value and keys
      (destructuring-bind (n v &rest keys)
	  decl
	(let ((newkeys (foldr #'expand-macros-key
			      (adjacent-pairs keys)
			      '())))
	  `(,n ,(expand-macros v) ,@newkeys)))

      ;; naked name, leave it alone
      decl))


(defmethod expand-macros-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args
    (let ((newdecls (mapcar #'expand-macros-decl decls))
	  (newbody (expand-macros (cons 'progn body))))
      `(let ,newdecls
	 ,newbody))))


;; ---------- Floating ----------

(defmethod float-let-blocks-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (destructuring-bind (newbody newenv)
	(float-let-blocks `(progn ,@body))

      ;; add our declarations to the environment
      (when (null newenv)
	(setq newenv (make-frame)))
      (with-local-frame decls
	;; add the new declarations to the front of NEWENV
	(add-frame-to-environment (current-frame) newenv t)

	;; return the re-written body and the new environment
	(list newbody newenv)))))


;; ---------- PROGN simplification ----------

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


(defmethod simplify-progn-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(let ,decls ,@(simplify-implied-progn newbody)))))


;; ---------- Synthesis ----------

(defun array-type-p (ty)
  "Test whether TY is an array type."
  (subtype-p ty 'array))


(defun array-value-p (form)
  "Test whether FORM is an array constructor."
  (and (listp form)
       (eql (car form) 'make-array)))


(defun module-value-p (form)
  "Test whether FORM is a module constructor."
  (and (listp form)
       (eql (car form) 'make-instance)))


(defun special-value-p (form)
  "Test wether FORM denotes a special value.

Special values are things like array constructors and mdule instanciations."
  (or (array-value-p form)
      (module-value-p form)))


(defun normal-value-p (form)
  "Test whether FORM denotes a ormal value.

Normal values are those that are not special in the sense of
SPECIAL-VALUE-P. Specifically, normal values have a bit-width."
  (not (special-value-p form)))


(defun synthesise-register (n)
  "Synthesise a register N within a LET block."
  (declare (optimize debug))

  (let ((v (get-initial-value n)))
    (as-literal "reg ")
    (let* ((type (get-type n))
	   (width (if (array-type-p type)
		      ;; width is the width of the element type
		      (apply #'bitwidth-type (deconstruct-type (cadr type)))

		      ;; width is of the type itself
		      (apply #'bitwidth-type (deconstruct-type type)))))

      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width)
	(as-literal " - 1 : 0 ] "))
      (synthesise n)
      (if (array-value-p v)
	  ;; synthesise the array bounds and initialisation
	  (synthesise-array-init n v)

	  ;; synthesise the assignment to the initial value
	  (progn
	    (as-literal " = ")
	    (synthesise v)))
      (as-literal ";"))))


(defun synthesise-wire (n)
  "Synthesise a wire N a LET block."
  (let ((v (get-initial-value n)))
    (let* ((type (get-type n))
	   (width (if (array-type-p type)
		      ;; width is the width of the element type
		      (apply #'bitwidth-type (deconstruct-type (cadr type)))

		      ;; width is of the type itself
		      (apply #'bitwidth-type (deconstruct-type type)))))
      (as-literal "wire ")
      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width)
	(as-literal " - 1 : 0 ] "))
      (synthesise n)
      (if (array-value-p v)
	  ;; synthesise the array constructor
	  (synthesise-array-init n v)

	  ;; synthesise the assignment to the initial value
	  (if (static-constant-p v)
	      (let ((iv (ensure-static v)))
		(unless (= iv 0)
		  ;; initial value isn't statially zero, synthesise
		  (as-literal " = ")
		  (synthesise v)))

	      ;; initial value is an expression, synthesise
	      (progn
		(as-literal " = ")
		(synthesise v))))
      (as-literal";"))))


(defun synthesise-constant (n)
  "Synthesise a constant N within a LET block.

Constants turn into local parameters."
  (let ((v (get-initial-value n)))
    (as-literal "localparam ")
    (synthesise n)
    (as-literal " = ")
    (synthesise v)
    (as-literal ";")))


(defun synthesise-module-instanciation (n)
  "Synthesise N as a module instanciation."
  (let ((v (get-initial-value n)))
    (destructuring-bind (modname &rest initargs)
	(cdr v)
      (synthesise-module-instance n modname initargs))))


(defun synthesise-decl (decl)
  "Synthesise DECL."
  (declare (optimize debug))

  (with-current-form decl
    (let* ((n (name-in-decl decl))
	   (v (get-initial-value n)))
      (if (module-value-p v)
	  ;; instanciating a module
	  (synthesise-module-instanciation n)

	  ;; otherwise, creating a variable
	  (case (get-representation n)
	    ('constant
	     (synthesise-constant n))
	    ('register
	     (synthesise-register n))
	    ('wire
	     (synthesise-wire n))
	    (t
	     (synthesise-register n)))))))


(defmethod synthesise-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (let ((decls (car args))
	(body (cdr args)))

    (with-local-frame decls
      ;; synthesise the constants and registers
      (as-block-forms decls :process #'synthesise-decl)

      (if (> (length decls) 0)
	  (as-blank-line))

      ;; synthesise the body
      (as-block-forms body))))
