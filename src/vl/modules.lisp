;; Top-level modules
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


;; ---------- Module interfaces ----------

(deftype module (required optional parameters frame)
  "The type of module interfaces.

Interfaces consist of three lists of arguments and the
frame they form.

REQUIRED holds the names of the required arguments. OPTIONAL holds the
names of optional arguments, as a list if they have a default. PARAMETERS
similarly holds the keyword arguments. NEEDED can't be null, but either
of the others can."
  t)


;; No meaningful sub-type relationships at present

(defmethod subtype-type ((ty1tag (eql 'module)) ty1args
			 (ty2tag (eql 'module)) ty2args)
  t)


(defmethod representable-type-sexp-p ((tytag (eql 'module)) tyargs)
  t)


(defun module-required-arguments (ty)
  "Return the list of argument names to module interface TY."
  (elt ty 1))


(defun module-optional-arguments (ty)
  "Return the list of argument names to module interface TY."
  (elt ty 2))


(defun module-arguments (ty)
  "Return the needed and optioal arguments."
  (append (module-required-arguments ty)
	  (module-optional-arguments ty)))


(defun module-parameters (ty)
  "Return the list of parameter names to module interface TY."
  (elt ty 3))


(defun module-frame (ty)
  "Return the frame formed by the module interface."
  (elt ty 4))


(defun parse-module-lambda-list (decls)
  "Parse DECLS as an ordinary lambda-list, returning the required, optional, and key parameters.

NEEDED cannot be null. Each element of OPTIONAL and PARAMETERS will either be
a symbol or a list of a symbol and an default value.

This is not enough information to typecheck module instanciation, which is
why the MODULE type also includes the environment created for these names."
  (multiple-value-bind (req-names opts rest keys allow-p auxs key-p)
      (handler-bind
	  ((error (lambda (c)
		    ;; map any underlying errors to syntax errors
		    (error 'syntax-error :form decls
					 :hint "Make sure lambda list is well-formed"))))
	(parse-ordinary-lambda-list decls))

    (let ((opt-names (mapcar #'safe-car opts))
	  (key-names (mapcar #'safe-car keys)))

      ;; sanity checks
      (unless req-names
	(error 'syntax-error :form decls
			     :hint "Modules lambda lists need at least one variable"))
      (when allow-p
	(error 'syntax-error :form decls
			     :hint "Module lambda lists can't include &allow-other-keys"))
      (when auxs
	(error 'syntax-error :form decls
			     :hint "Module lambda lists can't include &aux parameters"))
      (when rest
	(error 'syntax-error :form decls
			     :hint "Module lambda lists can't include a &rest parameter"))
      (unless (and (set-p req-names)
		   (set-p opt-names)
		   (set-p key-names))
	(error 'syntax-error :form decls
			     :hint "Can't have duplicate variable names in a lambda list"))
      (when (intersection req-names opt-names)
	(error 'syntax-error :form decls
			     :hint "Can't have optional and required parameters with the same names"))
      (when (intersection (union req-names opt-names) key-names)
	(error 'syntax-error :form decls
			     :hint "Can't have keyword and non-keyword parameters with the same names"))

      ;; structure into a consistent form
      (list req-names
	    (mapcar (lambda (opt)
		      (if (null (cadr opt))
			  (car opt)
			  (list (car opt) (cadr opt))))
		    opts)
	    (mapcar (lambda (opt)
		      (if (null (cadr opt))
			  (cadar opt)
			  (list (cadar opt) (cadr opt))))
		    keys)))))


;; ---------- Module late initialisation ----------

(defvar *module-late-initialisation* nil
  "List of functions that synthesise late intiialisation in modules.

This is primarily for initialising arrays representing ROMs, where
the conents are loaded from a file.")


(defun clear-module-late-initialisation ()
  "Clear the late initialisation functions."
  (setq *module-late-initialisation* nil))


(defun module-late-initialisation-p ()
  "Test whether there is late initialisation to be done."
  (not (null *module-late-initialisation*)))


(defun add-module-late-initialisation (f)
  "Add F to be run to synthesise late initialisation for the current module.

Each function should take no arguments, and run the necessary synthesis code."
  (appendf *module-late-initialisation* (list f)))


(defun run-module-late-initialisation ()
  "Run all the late synthesis functions.

The late intiialisations are cleared once they have been run."
  (dolist (f *module-late-initialisation*)
    (funcall f))

  (clear-module-late-initialisation))


;; ---------- Modules ----------

(deftype direction ()
  "The type of dataflow directions.

Valid directions are IN, OUT, and INOUT, and are seen from the
perspective of inside the module."
  '(member in out inout))


(defun direction-p (dir)
  "Test DIR is a valid pin direction."
  (typep dir 'direction))


(defun ensure-direction (dir)
  "Ensure DIR is a valid pin direction.

Signal VALUE-MISMATCH as an error if not."
  (unless (direction-p dir)
    (error 'value-mismatch :expected (list 'in 'out 'inout) :got dir)))


(defun join-args-params (args params)
  "Join ARGS and PARAMS into a single module lambda-list of decls."
  (if params
      (append args (list '&key) params)
      args))


(defun module-parameter-p (n)
  "Test whether N is a module paramater."
  (eql (get-representation n) 'parameter))


(defun compute-module-local-frame (decls)
  "Populate the local frame of DECLS."
  (with-local-frame decls
    (destructuring-bind (reqs opts keys)
	(parse-module-lambda-list decls)

      ;; add all the names
      (dolist (args (list reqs opts keys))
	(mapc #'add-decl-to-frame args))

      ;; mark representations
      (dolist (n reqs)
	(set-variable-property n 'as 'wire))
      (dolist (n (mapcar #'safe-car opts))
	(set-variable-property n 'as 'wire))
      (dolist (n (mapcar #'safe-car keys))
	(set-variable-property n 'as 'parameter)))))


(defun compute-module-interface-type (decls)
  "Return the module interface implied by DECLS."
  (destructuring-bind (reqs opts keys)
      (parse-module-lambda-list decls)

    `(module ,reqs ,opts ,keys ,(current-frame))))


(defmethod add-frames-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (add-local-frame-to-decls decls)
    (compute-module-local-frame decls)

    ;; return the form
    `(module ,modname ,decls
	     ,@(with-local-frame decls
		 (mapcar #'add-frames body)))))


(defmethod compute-type-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args

    (with-local-frame decls
      ;; typecheck the body of the module in its environment
      (compute-type (with-implicit-progn body))

      ;; return the interface type
      (compute-module-interface-type decls))))


(defmethod apply-type-constraints-sexp ((fun (eql 'module)) args)
  (declare (optimize debug))

  (destructuring-bind (modname decls &rest body)
      args

    (with-local-frame decls
      (let ((intf (compute-module-interface-type decls)))
	(dolist (n (variables-declared-in-current-frame))
	  (if (member n (module-arguments intf))
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
		      (apply-type-constraints v))))))))

      ;; cascade into the body
      (apply-type-constraints (with-implicit-progn body)))))


(defmethod read-variables-sexp ((fun (eql 'module)) args)
  '())


(defmethod infer-representation-sexp ((fun (eql 'module)) args)
  (declare (optimize debug))

  (destructuring-bind (modname decls &rest body)
      args

    (with-local-frame decls
      ;; do representation inference in body
      (infer-representation (with-implicit-progn body))

      ;; do direction inference on local variables (not parameters)
      (dolist (n (remove-if (lambda (n)
			      (eql (get-representation n) 'parameter))
			    (variables-declared-in-current-frame)))
	(let ((read (variable-property n 'read))
	      (written (variable-property n 'written))
	      (ignored (variable-property n 'ignored))
	      (ignorable (variable-property n 'ignorable)))

	  (let ((dir (if written
			 (if read
			     ;; variable is read and updated
			     'inout

			     ;; variable is just written to
			     'out
			     )

			 (if read
			     ;; variable is read but not updated
			     'in

			     ;; variable is neither read nor written
			     (progn
			       (if (not (or ignored ignorable))
				   ;; not marked as ignored/able
				   (warn 'unused-variable :variable n
							  :hint "Make sure variable is needed"))

			       ;; treat as read-only
			       'in)))))

	    (if (and (or read written)
		     ignored)
		;; variable is used despire being marked as ignored
		(warn 'used-variable :variable n
				     :hint "Why is the variable used when marked as ignored?"))

	    ;; check consistency with assigned direction
	    (if-let ((given (get-direction n)))
	      (when (not (eql dir given))
		(warn 'direction-mismatch :variable n
					  :got dir
					  :expected given
					  :hint "Make sure the explicitly-assigned direction is appropriate"))

	      ;; update direction if none given
	      (set-variable-property n 'direction dir))))))))


(defmethod float-let-blocks-sexp ((fun (eql 'module)) args)
  (declare (optimize debug))

  (destructuring-bind (modname decls &rest body)
      args

    ;; extract any declarations
    (destructuring-bind (newbody newenv)
	(float-let-blocks (with-implicit-progn body))

      (list
       `(module ,modname
		,decls

		,(if newenv
		     ;; declare the floated declarations around the body
		     (let ((newdecls (mapcar (lambda (np)
					       (destructuring-bind (n props)
						   np
						 (list n
						       (get-environment-property n 'initial-value newenv))))
					     (decls newenv))))

		       ;; add the new decls as a local frame
		       (setq newdecls (add-local-frame-to-decls newdecls))
		       (compute-let-local-frame newdecls)
		       (with-local-frame newdecls
			 (dolist (np (decls newenv))
			   (destructuring-bind (n props)
			       np
			     (set-variable-properties n (copy-list props)))))

		       `(let ,newdecls
			  ,newbody))

		     ;; no declarations, just use the new body
		     newbody))

       ;; no remaining variables to float
       (make-frame)))))


(defmethod simplify-progn-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(module ,modname ,decls ,@(simplify-implied-progn newbody)))))


(defmethod transform-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args

    `(module ,modname ,decls
	     ,@(with-local-frame decls
		 (mapcar #'transform body)))))


(defun synthesise-param (n)
  "Return the code for parameter N."
  (let ((v (get-initial-value n)))
    ;; parameter with an initial value
    (as-literal "parameter ")
    (synthesise n)
    (as-literal " = ")
    (synthesise (get-initial-value n))))


(defun synthesise-arg (n)
  "Return the code for argument N."
  (declare (optimize debug))

  (let ((type (get-type n))
	(direction (get-direction n))
	(as (get-representation n)))

    (let* ((width (bitwidth type))
	   (w (eval-in-static-environment width)))
      (as-literal (format nil "~a ~a"
			  (case direction
			    (in    "input")
			    (out   "output")
			    (inout "inout"))
			  (if (and (integerp w)
				   (= w 1))
			      ""
			      (format nil "[ ~(~a~) - 1 : 0 ] " width))))
      (synthesise n))))


(defmethod synthesise-sexp ((fun (eql 'module)) args)
  (when (not (in-top-level-context-p))
    (error "Nested modules aren't allowed"))

  (destructuring-bind (modname decls &rest body)
      args

    (with-local-frame decls
      (as-literal "module ")
      (synthesise modname)

      ;; parameters
      (if-let ((params (get-frame-names (filter-frame (lambda (n env)
							(eql (get-frame-property n 'as env)
							     'parameter))
						      (current-frame)))))
	  (as-argument-list params :before " #(" :after ")"
				   :sep ", "
				   :process #'synthesise-param))

      ;; arguments
      (if-let ((args (get-frame-names (filter-frame (lambda (n env)
						      (member (get-frame-property n 'as env)
							      '(wire register)))
					  (current-frame)))))
	(as-argument-list args :before "(" :after ");"
			       :sep ", "
			       :process #'synthesise-arg))
      (as-blank-line)

      ;; body
      (with-indentation
	(synthesise `(progn ,@body)))

      ;; late initialisation (if any)
      (when (module-late-initialisation-p)
	(as-blank-line)
	(as-literal "initial begin" :newline t)
	(with-indentation
	  (run-module-late-initialisation))
	(as-literal "end" :newline t))

      (as-blank-line)
      (as-literal "endmodule // ")
      (as-literal (format nil "~(~a~)" (ensure-legal-identifier modname)) :newline t)
      (as-blank-line))))


;; ---------- Module instanciation ----------

(defmethod compute-type-sexp ((fun (eql 'make-instance)) args)
  (declare (optimize debug))

  (destructuring-bind (modname &rest initargs)
      args

    ;; skip over leading quote of module name,
    ;; for compatability with Common Lisp usage
    (unquote modname)

    ;; add type constraints for all variables in the interface
    (let ((intf (get-module-interface modname))
	  (modargs (adjacent-pairs initargs)))

      (with-frame (module-frame intf)

	(dolist (n (module-arguments intf))
	  (let ((v (cadr (assoc (module-argument-name-to-keyword n) modargs)))
		(ty (get-type n)))
	    (if (and (not (null v))
		     (symbolp v))
		(add-type-constraint v ty))))))))


(defun module-argument-name-to-keyword (n)
  "Return the keyword form of N, as used in a MAKE-INSTANCE call."
  (make-keyword n))


(defun ensure-module-arguments-match-interface (modname initargs intf)
  "Ensure that INITARGS match the reqirements of INTF of MODNAME."
  (declare (optimize debug))

  (with-frame (module-frame intf)
    (let* ((kv (adjacent-pairs initargs))
	   (ks (alist-keys kv)))

      ;; make sure there are no duplicate arguments
      (unless (set-p ks)
	(error 'not-importable :module modname
			:hint "Check for duplicate arguments"))


      ;; make sure all required arguments are present
      (unless (every (lambda (n)
		       (member (module-argument-name-to-keyword n)
			       ks))
		     (module-required-arguments intf))
	(error 'not-importable :module modname
			       :hint "Make sure all required arguments are provided"))

      ;; make sure all arguments are in the interface
      (unless (every (lambda (k)
		       (or (member k (mapcar (compose #'module-argument-name-to-keyword #'safe-car)
					     (module-arguments intf)))
			   (member k (mapcar (compose #'module-argument-name-to-keyword #'safe-car)
					     (module-parameters intf)))))
		     ks)
	(error 'not-importable :module modname
			       :hint "Make sure all arguments are declare on the interface")))))


(defmethod apply-type-constraints-sexp ((fun (eql 'make-instance)) args)
  (declare (optimize debug))

  (destructuring-bind (modname &rest initargs)
      args

    ;; skip over leading quote of module name,
    ;; for compatability with Common Lisp usage
    (unquote modname)

    ;; check arguments
    (let ((intf (get-module-interface modname)))
      (ensure-module-arguments-match-interface modname initargs intf)

      (with-frame (module-frame intf)
	(let ((kv (adjacent-pairs initargs)))

	  ;; required arguments
	  (dolist (n (module-required-arguments intf))
	    (let ((v (cadr (assoc (module-argument-name-to-keyword n) kv))))
	      (ensure-subtype (compute-type v) (get-type n))))

	  ;; optional arguments
	  (dolist (n (module-required-arguments intf))
	    (if-let ((m (assoc (module-argument-name-to-keyword n) kv)))
	      (let ((v (cadr m)))
		(ensure-subtype (compute-type v) (get-type n))))))))))


(defmethod read-variables-sexp ((fun (eql 'make-instance)) args)
  (destructuring-bind (modname &rest initargs)
      args

    (let ((kv (adjacent-pairs initargs)))
      (foldr #'union (mapcar #'read-variables (mapcar #'safe-cadr kv)) '()))))


(defmethod rewrite-variables-sexp ((fun (eql 'make-instance)) args rewrites)
  (labels ((rewrite-args (l)
	     (if (null l)
		 l
		 (append (list (car l)
			       (rewrite-variables (cadr l) rewrites))
			 (rewrite-args (cddr l))))))

    (destructuring-bind (modname &rest initargs)
	args
      `(,fun ,modname ,@(rewrite-args initargs)))))


(defun synthesise-param-binding (n kv)
  "Synthesise the binding of parameter N in KV."
  (if-let ((m (assoc n kv
		     :key #'symbol-name
		     :test #'string-equal)))
    (let ((v (cadr m)))
      (as-literal ".")
      (synthesise n)
      (as-literal "(")
      (synthesise v)
      (as-literal ")"))))


(defun synthesise-arg-binding (n kv)
  "Synthesise the binding of N from KV"
  (let ((v (cadr (assoc n kv
		       :key #'symbol-name
		       :test #'string-equal))))
    (when v
      (as-literal ".")
      (synthesise n)
      (as-literal "(")
      (synthesise v)
      (as-literal ")"))))


(defun synthesise-module-instance-params (paramdecls kv)
  "Synthesise the parameter bindings PARAMDECLS in KV."
  (declare (optimize debug))
  (let ((paramsgiven (intersection (mapcar (compose #'make-keyword #'safe-car) paramdecls)
				   (alist-keys kv))))

    (if paramsgiven
	(progn
	  (as-literal " ")
	  (as-argument-list paramsgiven
			    :before "#(" :after ")"
			    :process (rcurry #'synthesise-param-binding kv)))

	(as-literal " "))))


(defun synthesise-module-instance-args (argdecls kv)
  "Synthesise the argument bindings ARGDECLS from KV."
  (as-argument-list argdecls
		    :before "(" :after ");"
		    :process (rcurry #'synthesise-arg-binding kv)))


(defmethod synthesise-sexp ((fun (eql 'make-instance)) args)
  (destructuring-bind (modname &rest initargs)
      args

    ;; skip over leading quote of module name,
    ;; for compatability with Common Lisp usage
    (unquote modname)

    (let ((intf (get-module-interface modname))
	  (kv (adjacent-pairs initargs)))
      (synthesise modname)
      (synthesise-module-instance-params (module-parameters intf) kv)
      (synthesise modname)
      (as-literal " ")
      (synthesise-module-instance-args (module-arguments intf) kv))))
