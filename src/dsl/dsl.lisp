;;;; Helper macros for building passes in the Verilisp DSL
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

(in-package :verilisp/dsl)

;;; This macro set makes it easier to code-up and read Verilisp's
;;; internal definitions and passes. It abstracts over the style
;;; of coding-up the generic functions needed to define compiler
;;; nanopasses, and lets us set up queues of passes to be run in
;;; sequence.


;;; ---------- Pass function name generation ----------

(defun pass-top-level-function-name (pass-name)
  "Return the top-level function name for PASS."
  (intern (upcase (symbol-name pass-name))))


(defun pass-wrap-level-function-name (pass-name)
  "Return the wrapper-level function name for PASS."
  (intern (upcase (concat (symbol-name (pass-top-level-function-name pass-name))
			  "/wrapper"))))


(defun pass-form-level-function-name (pass-name)
  "Return the form-level function name for PASS.

The default is the pass name followed by a suffix."
  (intern (upcase (concat (symbol-name (pass-top-level-function-name pass-name))
			  "/form"))))


(defun top-level-function-method-p (args)
  "Test whether ARGS are for a top-level generic function."
  (= (length args) 1))


(defun form-level-function-method-p (args)
  "Test whether ARGS are for a form-level generic function."
  (> (length args) 1))


;;; ---------- Defining a pass ----------

(defun add-pass (pass-name extra-args)
  "Add a pass called PASS-NAME that takes EXTRA-ARGS in addition to the form it works over.

If PASS-NAME exists already it is overridden."
  (setf (get pass-name 'pass-p) t)
  (setf (get pass-name 'pass-extra-args) extra-args))


(defun pass-p (pass-name)
  "Test whether PASS-NAME is defined as a pass."
  (get pass-name 'pass-p nil))


(defun ensure-pass (pass-name)
  "Ensure that PASS-NAME is declared as a pass."
  (unless (pass-p pass-name)
    (error 'dsl-error :hint (format nil  "No pass ~s defined" pass-name))))


(defun get-pass-extra-args (pass-name)
  "Return the extra arguments passed to PASS-NAME."
  (ensure-pass pass-name)
  (get pass-name 'pass-extra-args nil))


!(defmacro defpass (pass-name form-arg &rest opts)
  "Define a compiler nanopass called PASS-NAME.

FORM-ARG should be a list of a single argument that names the
form being operated on, plus any extraarguments to the pass.

The macro follows the same form as DEFGENERIC: a name and lambda list
followed by a (possibly empty) alist of options for the construction
of the pass. The options are:

- (:DOCUMENTATION \"docstring\"): install DOCSTRING
- (:QUEUE queue-name): install the pass onto QUEUE-NAME
- (:QUEUE-POSITION pos): install pass at POS, which can be :APPEND or :PREPEND
- (:SCHEMA schema): use SCHEMA as the default recursion scheme for forms
- (:PASSMETHOD (lambda-list) body): install a method with the given form
- (:PRE fun): run FUN before running the pass
- (:POST fun): run FUN after running the pass, returning its value

For :PRE and :POST, FUN should be a function designator. The function
for :PRE is passed FORM, and its result is used as the initial form
for the pass. The function for :POST is passed the modified FORM as
retruned by :PRE (if present) and the result of the pass, and the pass
overall returns its value.

By default the pass is not added to a queue. The default recuursion
schema is FAIL-UNKNOWN-FORM. Some schemata accept an extra argument,
as described in DEFINE-RECURSION-SCHEMA."
  (declare (optimize debug))

  (unless (>= (length form-arg) 1)
    (error 'dsl-error "Passes take a single argument"))

  (let ((docstring "A compiler nanopass.")
	(form (car form-arg))
	(extra-args (cdr form-arg))
	queue-tag
	(queue-position :append)
	(schema 'fail-unknown-form)
	schema-options
	atom-methods
	form-methods
	preprocessing
	postprocessing)

    ;; fill in the options over the defaults
    (dolist (opt opts)
      (destructuring-bind (name &rest value)
	  opt
	(case name
	  (:documentation
	   (setq docstring (car value)))

	  (:queue
	   (ensure-pass-queue (car value))
	   (setq queue-tag (car value)))

	  (:queue-position
	   (unless (member (car value) '(:append :prepend))
	     (error 'dsl-error :hint (format nil "Queue position must be :APPEND or :PREPEND (not ~s)" (car value))))
	   (setq queue-position (car value)))

	  ;; TODO: Should we allow function designators here, so that
	  ;; schemata can be added as code directly?

	  (:schema
	   (unless (recursion-schema-p (safe-car schema))
	     (error 'dsl-error :hint (format nil "Unrecogised recursion schema ~s" (safe-car schema))))
	   (setq schema (safe-car value))
	   (setq schema-options (cdr value)))

	  (:passmethod
	   (destructuring-bind (args &rest body)
	       value
	     (if (top-level-function-method-p args)
		 ;; atom method, installed onto top-level generic function
		 (appendf atom-methods (list value))

		 ;; form method, installed onto form-level generic function
		 (appendf form-methods (list value)))))

	  (:pre
	   (setq preprocessing (safe-car value)))

	  (:post
	   (setq postprocessing (safe-car value)))

	  (t
	   (error 'dsl-error :hint (format nil "Unrecognised pass option ~s" name))))))

    ;; sanity checks
    (if (and (or preprocessing postprocessing)
	     (not queue-tag))
	(error 'dsl-error :hint ":PRE and :POST only make sense for passes on a pass queue"))

    (let ((top-level-f (pass-top-level-function-name pass-name))
	  (form-level-f (pass-form-level-function-name pass-name))
	  (wrap-level-f (pass-wrap-level-function-name pass-name)))

      (with-gensyms (fun args res)

	`(eval-when (:compile-toplevel :load-toplevel :execute)

	   ;; define the top-level generic function
	   (defgeneric ,top-level-f (,form ,@extra-args)
	     (:documentation ,docstring)

	     ;; add the explicit methods for atoms forms
	     ,@(mapcar (lambda (m)
			 (destructuring-bind (margs &rest mbody)
			     m
			   `(:method ,(append margs extra-args)
			      ,@mbody)))
		       atom-methods)

	     ;; call form-level function for non-atom forms
	     (:method ((,form list) ,@extra-args)
	       (destructuring-bind (,fun &rest ,args)
		   ,form
		 (with-current-form ,form
		   (,form-level-f ,fun ,args ,@extra-args)))))

	   ;; declare the function as a pass, with its extra argument names
	   ,(if (null extra-args)
		`(add-pass ',pass-name nil)
		`(add-pass ',pass-name ',extra-args))

	   ;; define the form-level function
	   (defgeneric ,form-level-f (,fun ,args ,@extra-args)
	     (:documentation ,(format nil "Pass form-level handler for ~s" pass-name))

	     ;; default schema
	     (:method (,fun ,args ,@extra-args)
	       (,schema ,fun ,args
			:pass-name ',pass-name
			,@(if schema-options
			      (if (> (length schema-options) 1)
				  `(:option ,schema-options)
				  `(:option ,(car schema-options))))
			,@(if extra-args
			      `(:extra (list ,@extra-args)))))

	     ;; explicit methods for forms
	     ,@(mapcar (lambda (m)
			 (let ((mfun (caar m))
			       (margs (cdar m))
			       (body (cdr m)))
			   `(:method ((,fun (eql ',mfun)) ,args ,@extra-args)
			      (destructuring-bind ,margs
				  ,args
				,@body))))
		       form-methods))

	   ;; define the wrapper function
	   (defun ,wrap-level-f (,form ,@extra-args)
	     ,(format nil "Pass wrapper function for ~s" pass-name)

	     ;; pre-process
	     ,@(when preprocessing
		 `((setq ,form (funcall ,preprocessing ,form))))

	     ;; call pass
	     (let ((,res (,top-level-f ,form ,@extra-args)))

	       ;; post-process
	       ,@(if postprocessing
		     `((funcall ,postprocessing ,form ,res))
		     `(,res))))

	   ,@(when queue-tag
	       ;; store the pass name in the correct queue if one is given
	       `((eval-when (:compile-toplevel :load-toplevel :execute)
		   (add-pass-to-queue ',pass-name ',queue-tag
				      :prepend ,(eql queue-position :prepend))))))))))


(defmacro defpassmethod (pass-name form &body body)
  "Define a pass method.

The body of the method can contain leading option clauses. The
options are:

- (:DOCUMENTATION \"docstring\"): install DOCSTRING
- (:SCHEMA schema): use SCHEMA as the schema for this method
- (:SAME-AS fun): use the same behaviour as for function form FUN

If there is no :SCHEMA option then the rest of BODY is treated as the
body of the method."
  (declare (optimize debug))

  ;; unlike ordinary methods, we need a pass to add to
  (ensure-pass pass-name)

  (let ((docstring (format nil "Method for ~s in pass ~s." form pass-name))
	schema
	same-as)

    ;; consume leading option forms to extract "real" body
    (labels ((consume-options (b)
	       (let ((opt (car b)))
		 (if (stringp opt)
		     (progn
		       (setq docstring opt)
		       (consume-options (cdr b)))
		     (if (listp opt)
			 (case (car opt)
			   (:documentation
			    (setq docstring (cadr opt))
			    (consume-options (cdr b)))

			   (:schema
			    (ensure-recursion-schema (cadr opt))
			    (setq schema (cadr opt))
			    (consume-options (cdr b)))

			   (:same-as
			    (setq same-as (cadr opt))
			    (consume-options (cdr b)))

			   (t
			    b))

			 b)))))

      (setq body (consume-options body)))

    ;; sanity checks on options
    (cond ((and same-as
		(or body
		    schema))
	   (error 'dsl-error :hint "Method can't be the same as another and have a body of schema of its own"))

	  ((and (not same-as)
		(or (and schema
			 body)
		    (and (not schema)
			 (not body))))
	   (error 'dsl-error :hint "Method needs a schema or a body")))

    ;; synthesise the method
    (let ((top-level-f (pass-top-level-function-name pass-name))
	  (form-level-f (pass-form-level-function-name pass-name))
	  (extra-args (get-pass-extra-args pass-name)))

      (with-gensyms (fun args)

	(if same-as
	    ;; method is the same as another
	    (destructuring-bind (mfun &rest margs)
		form

	      `(defmethod ,form-level-f ((,fun (eql ',mfun))
					 ,@(append (list args) extra-args))
		 ,docstring

		 (,form-level-f ',same-as ,@(append (list args) extra-args))))

	    ;; method has a body
	    (if (top-level-function-method-p form)
		;; method is for an atom
		`(defmethod ,top-level-f ,form
		   ,docstring

		   ,@body)

		;; method is for a structured form
		(destructuring-bind (mfun &rest margs)
		    form

		  `(defmethod ,form-level-f ((,fun (eql ',mfun))
					     ,@(append (list args) extra-args))
		     ,docstring

		     ;; TODO: Need better lambda-list handling for other cases too
		     ,(if (eql (car margs) '&rest)
			  ;; capturing all the arguments
			  `(let ((,(cadr margs) ,args))
			     ,@(if schema
				   (list `(,schema ,fun ,args :pass-name ',pass-name
							      :extra ,extra-args))
				   body))

			  ;; "proper" arguments
			  `(destructuring-bind ,margs
			       ,args
			     ,@(if schema
				   (list `(,schema ,fun ,args :pass-name ',pass-name
							      :extra ,extra-args))
				   body)))))))))))
