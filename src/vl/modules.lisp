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

(in-package :vl)
(declaim (optimize debug))


;; ---------- Module interfaces ----------

(deftype module-interface (&optional (parameters ()) (arguments ()))
  "The type of module interfaces.

Interfaces consist of two lists, of parameters and arguments. At present
we don't define any sub-typing relationships."
  t)


(defun module-interface-parameters (ty)
  "Return the list of parameter decls to modfule interface TY."
  (cadr ty))


(defun module-interface-arguments (ty)
  "Return the list of argument decls to modfule interface TY."
  (caddr ty))


(defmethod expand-type-parameters-type ((ty (eql 'module-interface)) args)
  `(module-interface ,@args))


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

Valid directions are :IN, :OUT, and :INOUT, and are seen from the
perspective of inside the module."
  '(member :in :out :inout))


(defun direction-p (dir)
  "Test DIR is a valid pin direction."
  (typep dir 'direction))


(defun ensure-direction (dir)
  "Ensure DIR is a valid pin direction.

Signal VALUE-MISMATCH as an error if not."
  (unless (direction-p dir)
    (error 'value-mismatch :expected (list :in :out :inout) :got dir)))


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
  "Test whether N is a module paramater.

Module parameters have type :lisp to indicate that they should
be interpolated."
  (eql (get-type n) :lisp))


(defun typecheck-module-param (decl)
  "Type-check a module parameter declaration DECL.

The value of the parameter, if provided, is evaluated as a Lisp
expression in the current Lisp environment, *not* in Verilisp's
environment. This means that parameter values can't be defined in terms
of other parameter values."
  (declare (optimize debug))
  (if (listp decl)
      ;; standard declaration
      (destructuring-bind (n v)
	  decl

	(let ((val (eval v)))
	  (declare-variable n `((:initial-value ,val)
				(:as :parameter)))))

      ;; naked paramater
      (declare-variable decl `((:initial-value 0)
			       (:as :parameter)))))


(defun typecheck-module-params (decls)
  "Type-check the module parameter declarations DECLS."
  (mapc #'typecheck-module-param decls))


(defun typecheck-module-arg (decl)
  "Type-check a module argument declaration DECL."
  (destructuring-bind (n &key
			   type
			   width
			   (direction :in)
			   (as :wire))
      decl
    (ensure-direction direction)

    ;; if we have a width, it's a shortcut for unsigned-byte
    (if width
	(let* ((w (eval-in-static-environment width))
	       (ty `(unsigned-byte ,w)))
	  (if type
	      ;; if we have a type, it must match
	      (ensure-subtype ty type)

	      ;; if not, re-assign is to the shortcut
	      (setq type ty))))

    (declare-variable n `((:type ,type)
			  (:direction ,direction)))))


(defun typecheck-module-args (decls)
  "Type-check the module argument declarations DECLS."
  (mapc #'typecheck-module-arg decls))


(defun env-from-module-decls (args params)
  "Create an environment extending ENV from PARAMS and ARGS declarations of a module interface."
  (typecheck-module-params params)
  (typecheck-module-args args))


(defun make-module-environment (decls)
  "Return an environment built from the DECLS of a module."
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
      (split-args-params (decls-without-cached-frame decls))
    `(module-interface ,modparams ,modargs)))


(defmethod typecheck-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args

    (with-new-frame
      (make-module-environment decls)

      ;; typecheck the body of the module in its environment
      (typecheck (cons 'progn body))

      ;; cache the shallowest frame for use in later passes
      (setf (cdr (last decls))
	    (list (list 'frame (detach-frame *global-environment*))))

      ;; return the interface type
      (make-module-interface-type decls))))


(defmethod dependencies-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (with-frame (get-cached-frame decls)

      ;; no need to check decls or parameters as they're never dependent
      ;; (or are they?...)
      (dependencies `(progn body)))))


(defmethod float-let-blocks-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (destructuring-bind (newbody newenv)
	(float-let-blocks `(progn ,@body))
      (list
       `(module ,modname ,decls
		,(if newenv
		     ;; declare the floated declarations around the body
		     (let ((newdecls (mapcar (lambda (np)
					       (destructuring-bind (n &rest props)
						   np
						 (list n
						       (get-environment-property n :initial-value newenv))))
					     (decls newenv))))

		       ;; cache the environment in the decls
		       (appendf newdecls (list (list 'frame newenv)))

		       `(let ,newdecls
			  ,newbody))

		     ;; no declarations, just use the new body
		     newbody))
       (make-frame)))))


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


(defun synthesise-arg (decl)
  "Return the code for argument N."
  (declare (optimize debug))

  (destructuring-bind (n &key direction type (as :wire))
      decl
    (let ((width (bitwidth type)))
      (as-literal (format nil "~a ~a"
			  (case direction
			    (:in    "input")
			    (:out   "output")
			    (:inout "inout"))
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

    (with-frame (get-cached-frame decls)
      (as-literal "module ")
      (synthesise modname)

      (destructuring-bind (args params)
	  (split-args-params (decls-without-cached-frame decls))
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

(defun get-argument-or-parameter (n decls)
  "Retrieve argument or parameter N from DECLS.

N should be a string, which is matched against DECLS by symbol name."
  (if-let ((v (assoc n decls :key #'symbol-name :test #'string-equal)))
    (cdr v)))


(defun argument-for-module-interface-p (a intf)
  "Test whether A is an argument of INTF."
  (not (null (get-argument-or-parameter a (module-interface-arguments intf)))))


(defun parameter-for-module-interface-p (a intf)
  "Test whether A is a parameter of INTF."
  (not (null (get-argument-or-parameter a (module-interface-parameters intf)))))


(defun module-arguments-match-interface-p (intf modargs)
  "Test that MODARGS conform to INTF.

All the arguments in INTF must be provided in MODARGS. All the
MODARGS must refer to an argument or a parameter of INTF."
  (and
   ;; every module argument is provided
   (every (lambda (arg)
	    (member arg modargs :test #'string-equal))
	  (mapcar #'symbol-name (mapcar #'car (module-interface-arguments intf))))

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


(defun env-from-module-interface (intf)
  "Return an environment corresponding to INTF."
  (env-from-module-decls (module-interface-arguments intf)
			 (module-interface-parameters intf)))


(defmethod typecheck-sexp ((fun (eql 'make-instance)) args)
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
      (with-new-frame
	(env-from-module-interface intf)

	(let ((initargs-plist (plist-alist initargs)))
	  (dolist (arg modargs)
	    (let ((v (cdr (assoc arg initargs-plist
				 :key #'symbol-name
				 :test #'string-equal))))
	      (cond ((argument-for-module-interface-p arg intf)
		     (let ((tyval (typecheck v))
			   (tyarg (get-type arg)))
		       (ensure-subtype tyval tyarg)))
		    ((parameter-for-module-interface-p arg intf)
		     (let ((tyval (typecheck (eval-in-static-environment v)))
			   (tyarg (get-type arg)))
		       (ensure-subtype tyval tyarg))))))

	  intf)))))


(defmethod dependencies-sexp ((fun (eql 'make-instance)) args)
  ;; skip (because everything has to be an expression)
  nil)


(defmethod free-variables-sexp ((fun (eql 'make-instance)) args)
  (destructuring-bind (modname &rest initargs)
      args

    ;; for compatability with Common Lisp usage
    (unquote modname)

    (let ((modargs (values-to-arguments modname initargs)))
      (foldr #'union (mapcar #'free-variables modargs) '()))))


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


(defun synthesise-arg-binding (decl args)
  "Synthesise the binding of DECL from ARGS in ENV."
  (destructuring-bind (n &key &allow-other-keys)
      decl
    (let ((v (cdr (assoc n args
			 :key #'symbol-name
			 :test #'string-equal))))
      (as-literal ".")
      (synthesise n)
      (as-literal "(")
      (synthesise v)
      (as-literal ")"))))


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
