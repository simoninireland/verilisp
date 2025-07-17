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

(deftype module-interface (parameters arguments frame)
  "The type of module interfaces.

Interfaces consist of two lists, of parameters and arguments, and the
frame they form."
  t)


;; No meaningful sub-type relationships at present

(defmethod subtype-type ((ty1tag (eql 'module-interface)) ty1args
			 (ty2tag (eql 'module-interface)) ty2args)
  t)


(defun module-interface-parameters (ty)
  "Return the list of parameter decls to modfule interface TY."
  (cadr ty))


(defun module-interface-arguments (ty)
  "Return the list of argument decls to modfule interface TY."
  (caddr ty))


(defun module-interface-frame (ty)
  "Return the frame formed by the module interface."
  (cadddr ty))


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


(defun split-args-params (decls)
  "Split DECLS into arguments and parameters.

Arguments come first, and can either be bare symbols or lists of name
and properties. Parameters come after any :key marker and consists of
either bare names ot lists of names and values."
  (let ((i (position '&key decls)))
    (if i
	;; parameters (and possibly arguments)
	(list (subseq decls 0 i)
	      (subseq decls (1+ i)))

	;; just arguments
	(list decls nil))))


(defun join-args-params (args params)
  "Join ARGS and PARAMS into a single module lambda-list of decls."
  (if params
      (append args (list '&key) params)
      args))


(defun module-parameter-p (n)
  "Test whether N is a module paramater."
  (eql (get-representation n) 'parameter))


(defun typecheck-module-param (decl)
  "Type-check a module parameter declaration DECL.

The value of the parameter, if provided, is evaluated as a Lisp
expression in the current Lisp environment, *not* in Verilisp's
environment. This means that parameter values can't be defined in terms
of other parameter values."
  (declare (optimize debug))
  (with-current-form decl
    (if (listp decl)
	;; standard declaration
	(destructuring-bind (n v)
	    decl

	  (let ((val (eval v)))
	    (set-variable-properties n `((initial-value ,val)
					 (as parameter)))))

	;; naked paramater
	(set-variable-properties decl `((initial-value 0)
					(as parameter))))))


(defun typecheck-module-params (decls)
  "Type-check the module parameter declarations DECLS."
  (mapc #'typecheck-module-param decls))


(defun typecheck-module-arg (n)
  "Type-check a module argument declaration N."
  (declare (optimize debug))

  (with-current-form n
    ;; set defaults
    (set-variable-properties-unless-set n '((as wire)))))


(defun typecheck-module-args (decls)
  "Type-check the module argument declarations DECLS."
  (mapc #'typecheck-module-arg decls))


(defun env-from-module-decls (args params)
  "Pop[ulate the global environment with the PARAMS and ARGS declarations of a module interface."
  (typecheck-module-params params)
  (typecheck-module-args args))


(defun make-module-environment (decls)
  "Populate the global environment from the DECLS of a module."
  (destructuring-bind (modargs modparams)
      (split-args-params decls)

    ;; catch modules with no wires or registers
    (unless (> (length modargs) 0)
      (error 'not-synthesisable :hint "Module must import at least one wire or register"))

    ;; create the environment
    (env-from-module-decls modargs modparams)))


(defun make-module-interface-type (decls)
  "Return the module interface type of the DECLS of a module."
  (destructuring-bind (modargs modparams)
      (split-args-params decls)
    `(module-interface ,modparams ,modargs ,(current-frame))))


(defmethod add-frames-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (add-local-frame-to-decls decls)

    ;; return the form
    `(module ,modname ,decls
	     ,@(with-local-frame decls
		 (mapcar #'add-frames body)))))


(defmethod typecheck-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args

    (with-local-frame decls
      (make-module-environment decls)

      ;; typecheck the body of the module in its environment
      (typecheck (cons 'progn body))

      ;; return the interface type
      (make-module-interface-type decls))))


(defmethod read-written-variables-sexp ((fun (eql 'module)) args)
  '(() ()))


(defmethod dependencies-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (with-local-frame decls

      ;; no need to check decls or parameters as they're never dependent
      ;; (or are they?...)
      (dependencies `(progn body)))))


(defmethod float-let-blocks-sexp ((fun (eql 'module)) args)
  (declare (optimize debug))

  (destructuring-bind (modname decls &rest body)
      args

    ;; extract any declarations
    (let ((declarations (if (eql (caar body) 'declare)
			    (prog1
				(car body)
			      (setq body (cdr body))))))

      (destructuring-bind (newbody newenv)
	  (float-let-blocks `(progn ,@body))

	(list
	 `(module ,modname
		  ,@(if declarations
			(list decls declarations)
			(list decls))
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
	 (make-frame))))))



(defmethod simplify-progn-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(module ,modname ,decls ,@(simplify-implied-progn newbody)))))


(defun synthesise-param (decl)
  "Return the code for parameter N."
  (if (listp decl)
      ;; parameter with an initial value
      (destructuring-bind (n &rest rest)
	  decl
	(as-literal "parameter ")
	(synthesise n)
	(as-literal " = ")
	(synthesise (get-initial-value n)))

      ;; naked parameter
      (progn
	(as-literal "parameter ")
	(synthesise decl))))


(defun synthesise-arg (n)
  "Return the code for argument N."
  (declare (optimize debug))

  (let ((type (get-type n))
	(direction (variable-property n 'direction))
	(as (variable-property n 'as)))

    (let ((width (apply #'bitwidth-type (deconstruct-type type))))
      (as-literal (format nil "~a ~a"
			  (case direction
			    ('in    "input")
			    ('out   "output")
			    ('inout "inout"))
			  (if (and (integerp width)
				   (= width 1))
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

      (destructuring-bind (args params)
	  (split-args-params decls)
	;; parameters
	(if params
	    (as-argument-list params :before " #(" :after ")"
				     :sep ", "
				     :process #'synthesise-param))

	;; arguments
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

(defun argument-for-module-interface-p (n intf)
  "Test whether N is an argument of INTF."
  (not (null (member n (module-interface-arguments intf)
		     :key #'symbol-name
		     :test #'string-equal))))


(defun parameter-for-module-interface-p (n intf)
  "Test whether N is a parameter of INTF."
  (not (null (assoc n (module-interface-parameters intf)
		    :key #'symbol-name
		    :test #'string-equal))))


(defun module-arguments-match-interface-p (intf modargs)
  "Test that MODARGS conform to INTF.

All the arguments in INTF must be provided in MODARGS. All the
MODARGS must refer to an argument or a parameter of INTF."
  (and
   ;; every module argument is provided
   (every (lambda (arg)
	    (member arg modargs :test #'string-equal))
	  (mapcar #'symbol-name (module-interface-arguments intf)))

   ;; every modarg is either a module argument or parameter
   (every (lambda (arg)
	    (or (argument-for-module-interface-p arg intf)
		(parameter-for-module-interface-p arg intf)))
	  modargs)))


(defun ensure-module-arguments-match-interface (modname intf modargs)
  "Ensure that MODARGS or module MODNAME match INTF.

This is tested according to MODULE-ARGUMENTS-MATH-INTERFACE-P
and causes a NOT-IMPORTABLE error if not."
  (unless (module-arguments-match-interface-p intf modargs)
    (error 'not-importable :module modname
			   :hint "Check that arguments in the import match the module type")))


(defun keys-to-arguments (modname modargs)
  "Extract the keys from MODARGS when importing MODNAME."
  (labels ((every-argument (l)
	     "Return a list containing every argument element of L."
	     (cond ((null l)
		    '())
		   (t
		    (cons (car l)
			  (every-argument (cddr l)))))))

    (unless (evenp (length modargs))
      (error 'not-importable :module modname
			     :hint "Uneven number of module arguments"))

    (mapcar #'symbol-name (every-argument modargs))))


(defun values-to-arguments (modname modargs)
  "Extract the values from MODARGS when importing MODNAME."
  (labels ((every-value (l)
	     "Return a list containing every argument value of L."
	     (cond ((null l)
		    '())
		   (t
		    (cons (cadr l)
			  (every-value (cddr l)))))))

    (unless (evenp (length modargs))
      (error 'not-importable :module modname
			     :hint "Uneven number of module arguments"))

    (every-value modargs)))


(defmethod typecheck-sexp ((fun (eql 'make-instance)) args)
  (declare (optimize debug))

  (destructuring-bind (modname &rest initargs)
      args

    ;; skip over leading quote of module name,
    ;; for compatability with Common Lisp usage
    (unquote modname)

    (let ((intf (get-module-interface modname))
	  (modargs (keys-to-arguments modname initargs)))
      ;; ensure we have all the arguments we need
      (ensure-module-arguments-match-interface modname intf modargs)

      ;; typecheck the provided arguments against the interface
      (let ((f (module-interface-frame intf))
	    (initargs-plist (plist-alist initargs)))
	(dolist (arg modargs)
	  (let ((v (cdr (assoc arg initargs-plist
			       :key #'symbol-name
			       :test #'string-equal))))
	    (cond ((argument-for-module-interface-p arg intf)
		   (let ((tyval (typecheck v))
			 (tyarg (get-frame-property arg 'type f)))
		     (ensure-subtype tyval tyarg)))

		  ((parameter-for-module-interface-p arg intf)
		   (typecheck (eval-in-static-environment v)))

		  (t
		   (error 'unknown-variable :variable arg
					    :hint "Make sure variable is an argument to ~a" modname)))))

	intf))))


(defmethod dependencies-sexp ((fun (eql 'make-instance)) args)
  ;; skip (because everything has to be an expression)
  ;TODO: Is this the right design, or should we depend on the expressions?
  nil)


(defmethod read-written-variables-sexp ((fun (eql 'make-instance)) args)
  (destructuring-bind (modname &rest initargs)
      args

    ;; for compatability with Common Lisp usage
    (unquote modname)

    (merge-read-written-variables (values-to-arguments modname initargs))))


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


(defun synthesise-param-binding (decl args)
  "Synthesise the binding of parameter DECLs from ARGS in ENV."
  (destructuring-bind (n v)
      decl
    (if-let ((m (assoc n args
		       :key #'symbol-name
		       :test #'string-equal)))
      (let ((v (cdr m)))
	(as-literal ".")
	(synthesise n)
	(as-literal "(")
	(synthesise v)
	(as-literal ")")))))


(defun synthesise-arg-binding (n args)
  "Synthesise the binding of N from ARGS in ENV."
  (let ((v (cdr (assoc n args
		       :key #'symbol-name
		       :test #'string-equal))))
    (as-literal ".")
    (synthesise n)
    (as-literal "(")
    (synthesise v)
    (as-literal ")")))


(defun synthesise-module-instance-params (initargs intf)
  "Synthesise the parameter bindings INITARGS of INTF."
  (declare (optimize debug))
  (let ((paramdecls (module-interface-parameters intf)))
    ;; extract all the parameters actually specified
    (let* ((paramkeys (alist-keys paramdecls))
	   (args-alist (plist-alist initargs) )
	   (argkeys (alist-keys args-alist))
	   (paramsgiven (intersection paramkeys argkeys
				      :key #'symbol-name :test #'string-equal))
	   (paramdeclsgiven (remove-if (lambda (ndecl)
					 (not (member (symbol-name (car ndecl)) paramsgiven
						      :key #'symbol-name :test #'string-equal)))
				       paramdecls)))

      (if paramdeclsgiven
	  (progn
	    (as-literal " ")
	    (as-argument-list paramdeclsgiven
			      :before "#(" :after ")"
			      :process (rcurry #'synthesise-param-binding args-alist)))

	  (as-literal " ")))))


(defun synthesise-module-instance-args (initargs intf)
  "Synthesise the argument bindings INITARGS of INTF."
  (let ((argdecls (module-interface-arguments intf)))
    (as-argument-list argdecls
		      :before "(" :after ");"
		      :process (rcurry #'synthesise-arg-binding (plist-alist initargs)))))


(defun synthesise-module-instance (n modname initargs)
  "Synthesise the module instanciation MODNAME with given INITARGS assigning the instance to N."
  ;; skip over leading quote of module name,
  ;; for compatability with Common Lisp usage
  (unquote modname)

  (let ((intf (get-module-interface modname)))
    (synthesise modname)
    (synthesise-module-instance-params initargs intf)
    (synthesise n)
    (as-literal " ")
    (synthesise-module-instance-args initargs intf)))


(defmethod synthesise-sexp ((fun (eql 'make-instance)) args)
  (labels ((args-to-alist (plist)
	     "Convert a plist of arguments to an alist, respecting sub-lists. "
	     (if (null plist)
		 plist
		 (cons (list (car plist) (cadr plist))
		       (args-to-alist (cddr plist))))))

    (destructuring-bind (modname &rest initargs)
	args

      ;; skip over leading quote of module name,
      ;; for compatability with Common Lisp usage
      (unquote modname)

      (let ((intf (get-module-interface modname))
	    (modargs (keys-to-arguments modname initargs)))

	;; arguments
	(as-argument-list (arguments intf)
			  :before "(" :after ");"
			  :process (rcurry #'synthesise-arg-binding (args-to-alist initargs)))))))
