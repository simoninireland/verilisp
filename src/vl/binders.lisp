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


;; ---------- Representationa ----------

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
    (error 'representation-mismatch :expected '(register wire constant parameter module) :got rep)))


;; ---------- Local frames ----------

(defun add-decl-to-frame (decl)
  "Add DECL to the local frame."
   (if (listp decl)
       ;; declare name and initial value
       (destructuring-bind (n v)
	   decl
	 (declare-variable n `((initial-value ,v))))

       ;; declare just name
       (declare-variable decl '())))


(defun compute-let-local-frame (decls)
  "Populate the local frame of DECLS."
  (unless (null decls)
    (with-local-frame decls
      (mapc #'add-decl-to-frame decls))))


(defmethod add-frames-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args
    (let ((decls (add-local-frame-to-decls decls)))
      (compute-let-local-frame decls)

      ;; return the form
      `(let ,decls
	 ,@(with-local-frame decls
	     (mapcar #'add-frames body))))))


;; ---------- Dependencies ----------

(defun compute-let-dependencies ()
  "Compute the dependencies of all variables in the current frame."
  (dolist (n (variables-declared-in-current-frame))
    (with-recover-on-error
	;; leave dependencies alone on error
	t

      (if-let ((v (get-initial-value n)))
	(add-dependencies n (read-variables v))))))


(defmethod compute-dependencies-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args

    (with-local-frame decls
      (compute-let-dependencies)

      (compute-dependencies (with-implicit-progn body)))))


;; ---------- Free variables ----------

(defmethod read-variables-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (with-local-frame decls
      (let ((lns (variables-declared-in-current-frame)))

	;; compute read variables
	(let ((decl-rvs (foldr #'union (mapcar #'read-variables
					       (remove-nulls (mapcar #'get-initial-value lns)))
			       '()))
	      (body-rvs (read-variables (with-implicit-progn body))))

	  ;; remove any variables declared in this binder
	  (set-difference (union decl-rvs body-rvs) lns))))))


;; ---------- Typechecking ----------

(defun name-in-decl (decl)
  "Extract the name being declared by DECL.

The name is the first element, whether or not DECL is a list."
  (safe-car decl))


(defun compute-let-env ()
  "Compute the types of the declarations in the current frame."
  (declare (optimize debug))

  (dolist (n (variables-declared-in-current-frame))
    (with-recover-on-error
	;; leave variable alone
	nil

      ;; constrain the variable with whatever information we have
      (let ((ty (or (variable-property n 'type :default nil)
		    (if-let ((v (get-initial-value n)))
		      (compute-type v))
		    '(unsigned-byte 1))))

	;; array and module types are known at construction, so don't need to be inferred
	(when (or (subtype-p ty 'array)
		  (subtype-p ty 'module))
	  (set-variable-property n 'type ty)

	  ;; modules also are their own representation
	  (when (subtype-p ty 'module)
	    (set-variable-property n 'as 'module)))

	;; constrain the variable
	(add-type-constraint n ty)))))


(defmethod compute-type-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))
  (let ((decls (car args))
	(body (cdr args)))

    (with-local-frame decls
      (compute-let-env)

      ;; compute-type the body
      (compute-type (with-implicit-progn body)))))


(defmethod apply-type-constraints-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args
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
      (apply-type-constraints (with-implicit-progn body)))))


;; ---------- Representations ----------

(defmethod infer-representation-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

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
	      (ignored (variable-property n 'ignored))
	      (ignorable (variable-property n 'ignorable)))

	  (let ((rep (if written
			 ;; variable is updated, must be a register
			 'register

			 (if read
			     ;; variable is read and not updated
			     (if-let ((v (get-initial-value n)))
			       (cond ((array-value-p v)
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
	      (set-variable-property n 'as rep))))))))


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
	  (newbody (mapcar #'expand-macros body)))
      `(let ,newdecls
	 ,@newbody))))


;; ---------- Transformation ----------

(defun transform-decls (decls)
  "Apply transforms to the values in DECLS."
  (mapcar (lambda (decl)
	    (if (listp decl)
		(destructuring-bind (n v)
		    decl
		  `(,n ,(transform v)))

		decl))
	  decls))


(defmethod transform-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    ;; (with-local-frame decls
    ;;   (let ((newdecls (transform-decls decls))
    ;;	    (newbody (mapcar #'transform body)))
    ;;	;; add the local frame back to the new decls
    ;;	(add-local-frame-to-decls newdecls (current-frame))
    ;;	(break)
    ;;	`(let ,newdecls
    ;;	   ,@newbody)))

    (let ((newbody (with-local-frame decls
		     (mapcar #'transform body))))
      `(let ,decls
	 ,@newbody))))


;; ---------- Floating ----------

(defun float-initial-values (newenv)
  "Return a list of assignments to be made for the initial values in NEWENV."
  (let ((regs (remove-if (lambda (n)
			   (not (eql (get-representation n) 'register)))
			 (variables-declared-in-current-frame))))
    (mapcar (lambda (n)
	      `(setq ,n ,(get-initial-value n :default 0)))
	    regs)))


(defmethod float-let-blocks-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

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
		    newenv)))))))


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

  (let ((v (get-initial-value n :default 0)))
    (as-literal "reg ")
    (let* ((type (get-type n))
	   (width (if (array-type-p type)
		      ;; width is the width of the element type
		      (bitwidth (element-type-of-array type))

		      ;; width is of the type itself
		      (bitwidth type))))

      (if (signed-byte-p type)
	  (as-literal "signed "))

      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width)
	(as-literal " - 1 : 0 ] "))
      (synthesise n)
      (if v
	  (if (array-value-p v)
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

      (if (signed-byte-p type)
	  (as-literal "signed "))

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
