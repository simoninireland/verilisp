;;;; Top-level modules
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


;;; ---------- Module interfaces ----------

(deftype module (required optional parameters)
  "The type of module interfaces.

Interfaces consist of three lists of arguments and the
frame they form.

REQUIRED holds the names of the required arguments. OPTIONAL holds the
names of optional arguments, as a list if they have a default. PARAMETERS
similarly holds the keyword arguments. REQUIRED can't be null, but either
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


;;; ---------- Module late initialisation ----------

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


;;; ---------- Modules ----------

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


(defun parse-module-lambda-list (ll)
  "Parse a module's lambda-list.

This uses the PARSE-ORDINARY-LAMBDA-LIST from Alexandria, and adds some
extra consraints.

Return three lists of required, optional, and paraneter declarations. The
latter two may contain defaault arguments."
  (with-current-form ll

    (multiple-value-bind (req-names opts rest keys allow-p auxs key-p)
	(handler-case
	    ;; use Alexndria's function to parse the lambda-list
	    (parse-ordinary-lambda-list ll)

	  (error (c)
	    ;; map any underlying errors to syntax errors
	    (error 'syntax-error :hint "Make sure lambda list is well-formed")))

      (let ((opt-names (mapcar #'car opts))
	    (key-names (mapcar (lambda (e)
				 (cadr (car e)))
			       keys)))

	;; sanity checks
	(unless req-names
	  (error 'syntax-error :hint "Module lambda lists need at least one variable"))
	(when allow-p
	  (error 'syntax-error :hint "Module lambda lists can't include &allow-other-keys"))
	(when auxs
	  (error 'syntax-error :hint "Module lambda lists can't include &aux parameters"))
	(when rest
	  (error 'syntax-error :hint "Module lambda lists can't include a &rest parameter"))
	(unless (and (set-p req-names)
		     (set-p opt-names)
		     (set-p key-names))
	  (error 'syntax-error :hint "Can't have duplicate variable names in a lambda list"))
	(if-let ((dups (intersection req-names opt-names)))
	  (error 'syntax-error :hint (format nil "Can't have optional and required parameters with the same names (~s)" dups)))
	(if-let ((dups (intersection (union req-names opt-names) key-names)))
	  (error 'syntax-error :hint (format nil "Can't have keyword and non-keyword parameters with the same names (~s)" dups)))

	;; form into lists of decls
	(list req-names
	      (mapcar (lambda (e)
			(if-let ((v (cadr e)))
			  (list (car e)
				v)
			  (car e)))
		      opts)
	      (mapcar (lambda (e)
			(if-let ((v (cadr e)))
			  (list (cadr (car e))
				v)
			  (cadr (car e))))
		      keys))))))


(defun build-frame-from-lambda-list (ll)
  "Parse LL as an ordinary lambda-list.

Each parameter is annotated with whether it is required, optional, or
keyword. This allows the dfull lambda list to be reconstructed when
required.

Return the frame containing the parameters."
  (declare (optimize debug))

  (destructuring-bind (req-names opts keys)
      (parse-module-lambda-list ll)

    ;; structure into a frame
    (let ((f (make-frame)))
      (dolist (n req-names)
	;; reequired parameters have no initial value
	(declare-environment-variable n '((required t)) f))

      (dolist (nv opts)
	;; optonal parameters may have initial values
	(if (listp nv)
	    (destructuring-bind (n v)
		nv
	      (declare-environment-variable n `((initial-value ,v)
						(required nil))
					    f))

	    (declare-environment-variable nv `((required nil)) f)))

      (dolist (nv keys)
	;; key parameters may have initial values
	(if (listp nv)
	    (destructuring-bind (n v)
		nv
	      (declare-environment-variable n `((initial-value ,v)
						(as parameter))
					    f))

	    (declare-environment-variable nv `((as parameter)) f)))

      ;; return the frame
      f)))


(defun build-module-interface-decls-from-frame (f)
  "Extract the decls for the required, optional, and parameters to a module.

F should be the module's local frame."
  (let (reqs opts parms)
    (dolist (n (get-frame-names f))
      (if (eql (get-frame-property n 'as f :default nil) 'parameter)
	  ;; name is a parameter (which may have initial values)
	  (if-let ((v (get-frame-property n 'initial-value f :default nil)))
	    (appendf parms (list (list n v)))
	    (appendf parms (list n)))

	  (if (get-frame-property n 'required f :default nil)
	      ;; name is a required argument (which never have initial values)
	      (appendf reqs (list n))

	      ;; name is an optional argument (which may have initial values)
	      (if-let ((v (get-frame-property n 'initial-value f :default nil)))
		(appendf opts (list (list n v)))
		(appendf opts (list n))))))

    (list reqs opts parms)))


(defun build-module-interface-type-from-frame (f)
  "Return the module interface implied by F."
  (destructuring-bind (reqs opts parms)
      (build-module-interface-decls-from-frame f)

    `(module ,reqs
	    ,(mapcar #'safe-car opts)
	    ,(mapcar #'safe-car parms))))


(defpassmethod add-frames (module modname decls &rest body)
  (declare (optimize debug))

  (let ((local-frame (build-frame-from-lambda-list decls)))
    ;; add frames to the body in this new environment
    (with-local-frame local-frame

      ;; return the binder with the frame as its decls
      (let ((fbody (mapcar #'add-frames body)))
	`(module ,modname ,local-frame
		 ,@fbody)))))


(defpassmethod compute-type (module modname f &rest body)
  (declare (optimize debug))

  (with-local-frame f
    ;; constraint interface variables
    (dolist (n (variables-declared-in-current-frame))
      (let* ((ty (or (variable-property n 'type :default nil)
		     '(unsigned-byte 1))))
	(add-type-constraint n ty)))

    ;; typecheck the body of the module in its environment
    (compute-type (with-implicit-progn body))

    ;; return the interface type
    (build-module-interface-type-from-frame f)))


(defpassmethod apply-type-constraints (module modname f &rest body)
  (declare (optimize debug))

  (with-local-frame f
    (let ((intf (build-module-interface-type-from-frame f)))
      (dolist (n (get-frame-names f))
	(if (member n (module-arguments intf))
	    ;; constrain the variable's type (which must be representable)
	    (let* ((constraints (get-type-constraints n))
		   (lubty (if constraints (apply #'lub constraints))))

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
    (apply-type-constraints (with-implicit-progn body))))


(defpassmethod read-variables (module &rest args)
  '())


(defpassmethod compute-dependencies (module modname f &rest body)
  (with-local-frame f
    (compute-dependencies (with-implicit-progn body))))


(defpassmethod infer-representation (module modname f &rest body)
  (declare (optimize debug))

  (with-local-frame f
    ;; do representation inference in body
    (infer-representation (with-implicit-progn body))

    ;; do direction inference on local variables (not parameters)
    (dolist (n (remove-if (lambda (n)
			    (eql (get-representation n) 'parameter))
			  (get-frame-names f)))

      (let ((read (variable-property n 'read))
	    (written (variable-property n 'written))
	    (ignored (variable-property n 'ignore))
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
	    (set-variable-property n 'direction dir)))))))


;;; The top-level module grabs the floated LET blocks and coalesces them
;;; into a single block.

(defpassmethod float-let-blocks (module modname f &rest body)
  (declare (optimize debug))

  ;; extract any declarations
  (destructuring-bind (newbody newenv)
      (float-let-blocks (with-implicit-progn body))

    (list
     `(module ,modname
	      ,f

	      ,(float-apply newbody newenv))

     ;; no remaining variables to float
     (make-frame))))


(defpassmethod simplify-progn (module modname f &rest body)
  (let ((newbody (mapcar #'simplify-progn body)))
    `(module ,modname ,f ,@(simplify-implied-progn newbody))))


(defpassmethod elaborate-state-machines (module modname f &rest body)
  `(module ,modname ,f
	   ,@(with-local-frame f
	       (mapcar #'elaborate-state-machines body))))


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
      (as-literal (format nil "~a ~a ~a"
			  (case direction
			    (in    "input")
			    (out   "output")
			    (inout "inout"))
			  (if (eql as 'register)
			      "reg"
			      "")
			  (if (and (integerp w)
				   (= w 1))
			      ""
			      (format nil "[ ~(~a~) - 1 : 0 ] " width))))
      (synthesise n))))


(defpassmethod synthesise (module modname f &rest body)
  (declare (optimize debug))
  (when (not (in-top-level-context-p))
    (error 'not-synthesisable :hint "Nested modules don't make sense"))

  (with-local-frame f

    (destructuring-bind (reqs opts keys)
	(build-module-interface-decls-from-frame f)

      (as-literal "module ")
      (synthesise modname)

      ;; parameters
      (when (> (length keys) 0)
	(as-argument-list (mapcar 'safe-car keys)
			  :before " #(" :after ")"
			  :sep ", "
			  :process #'synthesise-param))

      ;; arguments
      (as-argument-list (append reqs (mapcar #'safe-car opts))
			:before "(" :after ");"
			:sep ", "
			:process #'synthesise-arg)
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


;;; ---------- Module instanciation ----------

(defun module-argument-name-to-keyword (n)
  "Return the keyword form of N, as used in a MAKE-INSTANCE call."
  (make-keyword n))


(defpassmethod compute-type (make-instance modname &rest initargs)
  (declare (optimize debug))

  ;; skip over leading quote of module name,
  ;; for compatability with Common Lisp usage
  (unquote modname)

  ;; add type constraints for all variables in the interface
  (let ((intf (get-module-interface modname))
	(modargs (adjacent-pairs initargs))
	(f (get-module-frame modname)))

    (dolist (n (module-arguments intf))
      (let ((v (cadr (assoc (module-argument-name-to-keyword n) modargs))))
	(when (and (not (null v))
		   (symbolp v))
	  (add-type-constraint v (with-frame f (get-type n))))))

    intf))


(defpassmethod infer-representation (make-instance modname &rest initargs)
  (declare (optimize debug))

  ;; skip over leading quote of module name,
  ;; for compatability with Common Lisp usage
  (unquote modname)

  (let ((intf (get-module-interface modname))
	(modargs (adjacent-pairs initargs))
	(f (get-module-frame modname)))

    ;; convert directions into read/written constraints
    (dolist (n (module-arguments intf))
      (let* ((v (cadr (assoc (module-argument-name-to-keyword n) modargs)))
	     (rs (read-variables v)))

	(unless (null rs)
	  (let ((dir (with-frame f (get-direction n))))

	    (when (eql dir 'in)
	      (mark-variables-as-read rs))
	    (when (member dir '(out inout))
	      (mark-variables-as-written rs))))))))


(defun ensure-module-arguments-match-interface (modname initargs intf)
  "Ensure that INITARGS match the reqirements of INTF of MODNAME."
  (declare (optimize debug))

  (let ((f (get-module-frame modname)))
    (with-frame f
      (let* ((kv (adjacent-pairs initargs))
	     (ks (alist-keys kv)))

	;; make sure there are no duplicate arguments
	(unless (set-p ks)
	  (let ((ns (duplicates ks)))
	    (error 'not-importable :module modname
				   :arg ns
				   :hint "Check for duplicate arguments")))

	;; make sure all required arguments are present
	(dolist (n (module-required-arguments intf))
	  (let ((k (module-argument-name-to-keyword n)))
	    (unless (member k ks)
	      (error 'not-importable :module modname
				     :arg k
				     :hint "Make sure all required arguments are provided"))))

	;; make sure all arguments are in the interface
	(dolist (k ks)
	  (unless (member k (mapcar (compose #'module-argument-name-to-keyword #'safe-car)
				    (union (module-arguments intf)
					   (module-parameters intf))))
	    (error 'not-importable :module modname
				   :arg k
				   :hint "Make sure all arguments provided are declared in the interface")))))))


(defpassmethod apply-type-constraints (make-instance modname &rest initargs)
  (declare (optimize debug))

  ;; skip over leading quote of module name,
  ;; for compatability with Common Lisp usage
  (unquote modname)

  ;; check arguments
  (let ((intf (get-module-interface modname))
	(f (get-module-frame modname)))

    (ensure-module-arguments-match-interface modname initargs intf)

    (let ((kv (adjacent-pairs initargs)))
      ;; required arguments
      (dolist (n (module-required-arguments intf))
	(let ((v (cadr (assoc (module-argument-name-to-keyword n) kv)))
	      (ty (with-frame f
		    (get-type n))))
	  (ensure-subtype (compute-type v) ty)))

      ;; optional arguments
      (dolist (n (module-required-arguments intf))
	(if-let ((m (assoc (module-argument-name-to-keyword n) kv)))
	  (let ((v (cadr m))
		(ty (with-frame f
		      (get-type n))))
	    (ensure-subtype (compute-type v) ty))))

      ;; if an argument is written to, it must be a generalised place
      (let ((written-args (with-frame f
			    (remove-if-not #'variable-written-p (module-arguments intf)))))
	(dolist (n written-args)
	  (let* ((k (module-argument-name-to-keyword n))
		 (v (cadr (assoc k kv))))

	    (unless (generalised-place-p v)
	      (error 'not-importable :module modname
				     :arg k
				     :hint "Argument must be a generalised place"))))))))


(defpassmethod read-variables (make-instance modname &rest initargs)
  (let ((kv (adjacent-pairs initargs)))
    (union-all (mapcar #'read-variables (mapcar #'safe-cadr kv)))))


(defpassmethod rewrite-variables (make-instance modname &rest initargs)
  (labels ((rewrite-args (l)
	     (if (null l)
		 l
		 (append (list (car l)
			       (rewrite-variables (cadr l) rewrite))
			 (rewrite-args (cddr l))))))

    `(make-instance ,modname ,@(rewrite-args initargs))))


(defun synthesise-param-binding (n kv)
  "Synthesise the binding of parameter N in KV."
  (if-let ((m (assoc n kv
		     :key #'symbol-name
		     :test #'string-equal)))
    (let ((v (cadr m)))
      (as-literal ".")
      (synthesise n)
      (as-literal "(")
      (synthesise (eval-in-static-environment v))
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


(defpassmethod synthesise (make-instance modname &rest initargs)
  ;; skip over leading quote of module name,
  ;; for compatability with Common Lisp usage
  (unquote modname)

  (let ((intf (get-module-interface modname))
	(kv (adjacent-pairs initargs)))
    (synthesise modname)
    (synthesise-module-instance-params (module-parameters intf) kv)
    (synthesise modname)
    (as-literal " ")
    (synthesise-module-instance-args (module-arguments intf) kv)))
