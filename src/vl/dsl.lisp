;;;; Helper macros for the Verilisp DSL
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

;;; This macro set makes it easier to code-up and read Verilisp's
;;; internal definitions and passes. It abstracts over the style
;;; of coding-up the generic functions needed to define compiler
;;; nanopasses, and lets us set up queues of passes to be run in
;;; sequence.


;;; ---------- Pass queues ----------

(defclass pass-queue ()
  ((queue
    :documentation "The passes."
    :initform nil
    :accessor pass-queue-queue))
  (:documentation "A queue of compiler nanopasses.

The queue can be updated as passes are added, with new passes being
appended (default) or prepended (if required). The queue can be run
in one operation, with the program being passed along as modified."))


(defun add-pass-to-queue (pass-name queue &key prepend)
  "Add PASS-NAME to QUEUE.

If :PREPEND is non-NIL then the pass is added to the front of the
queue; otherwise it is added to the back."
  (if prepend
      (setf (pass-queue-queue queue) (cons pass-name (pass-queue-queue queue)))
      (appendf (pass-queue-queue queue) (list pass-name))))


(defun clear-pass-queue (queue)
  "Clear all passes from QUEUE."
  (setf (pass-queue-queue queue) nil))


(defun run-pass-queue (queue form)
  "Run the passes of QUEUE in order against FORM.

The return value of each pass is used as the input to the next."
  (let ((f form))
    (dolist (pass (pass-queue-queue queue))
      (setq f (funcall pass f)))

    ;; return the result of the last pass
    f))


;;; Defining pass queues

(defparameter *pass-queue-tags* nil
  "Alist from tags used in DEFPASS/VL to pass queues.")


(defun pass-queue-p (tag)
  "Test whether TAG is the name of a defined pass queue."
  (assoc tag *pass-queue-tags*))


(defun ensure-pass-queue (tag)
  "Ensure TAG is the name of a defined pass queue."
  (unless (pass-queue-p tag)
      (error 'dsl-error :hint (format nil "No pass queue ~s defined" tag))))


(defun get-pass-queue (tag)
  "Return the pass queue named TAG."
  (if-let ((a (assoc tag *pass-queue-tags*)))
    (cadr a)
    (error 'dsl-error :hint (format nil "No pass queue ~s defined" tag))))


(defmacro define-pass-queue/vl (tag)
  "Define and install a new pass queue with TAG.

TAG is used when defining passes to select the appropriate queue."
  `(let ((queue (make-instance 'pass-queue)))
     (appendf *pass-queue-tags* (list (list ',tag queue)))))


;;; The standard pass queues
;;; The pass queues correspond to "macro-passes", composed of
;;; nanopasses. They are run in the following order, which allows
;;; implementations to attach new nanopasses in the appropriate place.
;;; The standard nanopasses are added at system initialisation.

(define-pass-queue/vl pre-typing)
(define-pass-queue/vl typing)
(define-pass-queue/vl post-typing)
(define-pass-queue/vl synthesis)


;;; ---------- Pass-defining macros ----------

;;; Pass function name generation

(defun pass-top-level-function-name (pass-name)
  "Return the top-level function name for PASS."
  (intern (upcase (symbol-name pass-name))))


(defun top-level-function-method-p (args)
  "Test whether ARGS are for a top-level generic function."
  (= (length args) 1))


(defun pass-form-level-function-name (pass-name)
  "Return the form-level function name for PASS.

The default is the pass name followed by a suffix."
  (intern (upcase (concat (symbol-name (pass-top-level-function-name pass-name))
			  "/form"))))


(defun form-level-function-method-p (args)
  "Test whether ARGS are for a form-level generic function."
  (> (length args) 1))


;;; Defining recursion schemata
;;; These are used to flesh-out the bodies of form-level functions.
;;; They actually don't have to be recursive at all: they'll be used
;;; to build the structurally recursive pass functions.
;;;
;;; The name of the schema is used in the :SCHEMA option to
;;; DEFPASS/VL and DEFPASS<ETHOD/VL to choose the schema when
;;; no body is provided.

(defparameter *recursion-schemata* nil
  "A list of recursion scheme function names.")


(defmacro define-recursion-schema/vl (schema-name schema-args &body body)
  "Define a new recursion schema.

The schema should take three arguments which will contain the name of
the variable holding the form head, the name of the variable holding
the form arguments, and the name of the pass. It should return the
code to be inserted into the form-level function, as a macro would."
  ;; check the schema prototype is correct
  (unless (= (length schema-args) 3)
    (error 'dsl-error :hint (format nil "Recursion schemata take three arguments (not ~s)" schema-args)))

  (let ((docstring "A recursion schema"))
    (when (stringp (car body))
      (setq docstring (car body))
      (setq body (cdr body)))

    `(progn
       ;; wrap the schema up in a function
       (defun ,schema-name ,schema-args
	 ,docstring
	 ,@body)

       ;; install the function as a valid schema
       (appendf *recursion-schemata* (list ',schema-name)))))


(defun recursion-schema-p (schema-name)
  "Test whether SCHEMA-NAME names a recursion schema."
  (member schema-name *recursion-schemata*))


(defun ensure-recursion-schema (schema-name)
  "Ensure that SCHEMA-NAME names a recursion schema."
  (unless (recursion-schema-p schema-name)
      (error 'dsl-error :hint (format nil "No recursion schema ~s defined" schema-name))))


;;; Standard schemata

(define-recursion-schema/vl fail-unknown-form (fun args pass-name)
  "Any call to this schema fails with an UNKNOWN-FORM error."
  `(error 'unknown-form :form (cons ,fun ,args)
			:hint (format nil "Define an entry for ~s handling ~s"
				      ',pass-name ,fun)))


(define-recursion-schema/vl into-arguments (fun args pass-name)
  "A recursion schema that recurses into ARGS.

The form returned is a list of the form (FUN . VARGS) where VARGS
are the results of the recursive calls. If the results are irrelevant
use the OVER-ARGUMENTS schema."
  `(mapc #',pass-name ,args))


(define-recursion-schema/vl over-arguments (fun args pass-name)
  "A recursion schema that maps the pass over the arguments.

The results of the map-over are discarded: to get the result,
use the INTO-ARGUMENTS schema."
  (with-gensyms (vals)
    `(let ((,vals (mapcar #',pass-name ,args)))
       (cons ,fun ,vals))))


(define-recursion-schema/vl into-function-and-arguments (fun args pass-name)
  "A recursion schema that recurses into both FUN and ARGS.

The form returned is a list of the form (VFUN . VARGS) where VFUN and
VARGS are the results of the recursive calls."
  (with-gensyms (vfun vals)
    `(let ((,vfun (,pass-name ,fun))
	   (,vals (mapcar #',pass-name ,args)))
       (cons ,vfun ,vals))))


(define-recursion-schema/vl into-arguments-all-non-nil (fun args pass-name)
  "A recursion schema that recurses into all arguments and checks they're all non-NIL.

This is usually used for predicates over code."
  `(every #',pass-name ,args))


(define-recursion-schema/vl into-arguments-union (fun args pass-name)
  "A recursion schema that recurses into all arguments and unions the results."
  `(foldr #'union (mapcar #',pass-name ,args) '()))


;;; Define a pass

(defmacro defpass/vl (pass-name extra-args &rest opts)
  "Define a compiler nanopass called PASS-NAME.

EXTRA-ARGS are ignored at the moment, and should be NIL.

The macros follows the same form as DEFGENERIC: a name and lambda list
followed by a (possibly empty) alist of options for the construction
of ther pass. The options are:

- (:DOCUMENTATION \"docstring\"): install DOCSTRING
- (:QUEUE queue-name): install the pass onto QUEUE-NAME
- (:QUEUE-POSITION pos): install pass at POS, which can be :APPEND or :PREPEND
- (:SCHEMA schema): use SCHEMA as the default recursion scheme for forms
- (:METHOD (lambda-list) body): install a method with the given form

The default queue is POST-TYPING, appended to by default. The default
recuursion schema is FAIL-BY-DEFAULT."
  (declare (optimize debug))

  (let ((docstring "A compiler nanopass.")
	(queue-tag 'post-typing)
	(queue-position :append)
	(schema 'fail-unknown-form)
	(atom-methods nil)
	(form-methods nil))

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

	  (:schema
	   (ensure-recursion-schema (car value))
	   (setq schema (car value)))

	  (:method
	      (destructuring-bind (args &rest body)
		  value
		(if (top-level-function-method-p args)
		    ;; atom method, installed onto top-level generic function
		    (appendf atom-methods (list value))

		    ;; form method, installed onto form-level generic function
		    (appendf form-methods (list value))))))))

    (let ((top-level-f (pass-top-level-function-name pass-name))
	  (form-level-f (pass-form-level-function-name pass-name)))

      (with-gensyms (form fun args)

	`(progn
	   ;; store the pass name in the correct queue
	   (add-pass-to-queue ',top-level-f
			      (cadr (assoc ',queue-tag *pass-queue-tags*))
			      :prepend ,(eql queue-position :prepend))

	   ;; define the top-level generic
	   (defgeneric ,top-level-f (,form ,@extra-args)
	     (:documentation ,docstring)
	     ,@(mapcar (lambda (m)
			 `(:method ,@m))
		       atom-methods)

	     ;; call form-level function for non-atom forms
	     (:method ((,form list) ,@extra-args)
	       (destructuring-bind (,fun &rest ,args)
		   ,form
		 (with-current-form ,form
		   (,form-level-f ,fun ,args ,@extra-args)))))

	   ;; define the form-level function
	   (defgeneric ,form-level-f (,fun ,args ,@extra-args)
	     (:documentation ,(concat "Form-level handler for " (symbol-name pass-name)))

	     ;; default schema
	     (:method (,fun ,args ,@extra-args)
	       ,(funcall schema fun args pass-name))

	     ;; explicit methods
	     ,@(mapcar (lambda (m)
			 (let ((mfun (caar m))
			       (margs (cdar m))
			       (body (cdr m)))
			   `(:method ((,fun (eql ',mfun)) ,args)
			      (destructuring-bind ,margs
				  ,args
				,@body))))
		       form-methods)))))))


(defmacro defpassmethod/vl (pass-name form &body body)
  "Define a pass method.

The body of the method can contain leading option clauses. The
options are:

- (:DOCUMENTATION \"docstring\"): install DOCSTRING
- (:SCHEMA schema): use SCHEMA as the schema for this method
- (:SAME-AS fun): use the same behaviour as for function form FUN

If there is no :SCHEMA option then the rest of BODY is treated as the
body of the method."
  (declare (optimize debug))
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

    ;; if we have a schema we mustn't have a body
    (if (and schema
	     body)
	(error 'dsl-error :hint "Method can have a schema or a body, but not both"))

    ;; if we have a same-as we mustn't have a body or a schema
    (if (and same-as
	     (or body
		 schema))
	(error 'dsl-error :hint "Method can't be the same as another and have a body of schema of its own"))

    ;; synthesise the method
    (let ((top-level-f (pass-top-level-function-name pass-name))
	  (form-level-f (pass-form-level-function-name pass-name)))

      (with-gensyms (fun args)

	(if same-as
	    ;; method is the same as another
	    (destructuring-bind (mfun &rest margs)
		form
	      `(defmethod ,form-level-f ((,fun (eql ',mfun)) ,args)
		 ,docstring
		 (,form-level-f ',same-as ,args)))

	    (if (top-level-function-method-p form)
		;; method is for an atom
		`(defmethod ,top-level-f ,form
		   ,docstring
		   ,@body)

		;; method is for a structured form
		(destructuring-bind (mfun &rest margs)
		    form

		  `(defmethod ,form-level-f ((,fun (eql ',mfun)) ,args)
		     ,docstring
		     (destructuring-bind ,margs
			 ,args
		       ,@(if schema
			     (list (funcall schema fun args pass-name))
			     body))))))))))
