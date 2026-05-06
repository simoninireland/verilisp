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
  "Alist from schemata tags to code returning the form of the recursion.")


(defmacro define-recursion-schema/vl (schema-name schema-args &body body)
  "Define a new recursion schema."
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
       (appendf *recursion-schemata* (list #',schema-name)))))


;;; Standard schemata

(define-recursion-schema/vl fail (tag args pass-name)
  "Any call to this schema fails with an UNKNOWN_FORM error."
  (error 'unknown-form :hint (format nil "Define an entry for ~s handling ~s"
				     pass-name tag)))


(define-recursion-schema/vl recurse-into-arguments (fun args pass-name)
  "A recursion schema that recurses into ARGS.

The form returned is a list of the form (FUN . VALS) where VALS
are the results of the recursive calls."
  (with-gensyms (vals)
    `(let ((,vals (mapcar #'pass-name args)))
       (cons ,fun ,vals))))


;; Define a pass

;; TODO: Need to introduce gensyms to the generic functions to avoid capture

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
"
  (declare (optimize debug))

  (let ((docstring "A compiler nanopass.")
	(queue-tag 'post-typing)
	(queue-position :append)
	(schema :recurse)
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
	   (progn
	     (ensure-pass-queue (car value))
	     (setq queue-tag (car value))))

	  (:queue-position
	   (progn
	     (unless (member (car value) '(:append :prepend))
	       (error 'dsl-error :hint (format nil "Queue position must be :APPEND or :PREPEND (not _s)" (car value))))
	     (setq queue-position (car value))))

	  ;; not handling :schema yet

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

      `(progn
	 ;; store the pass name in the correct queue
	 (add-pass-to-queue ',top-level-f
			    (cadr (assoc ',queue-tag *pass-queue-tags*))
			    :prepend ,(eql queue-position :prepend))

	 ;; define the top-level generic
	 (defgeneric ,top-level-f (form ,@extra-args)
	   (:documentation ,docstring)
	   ,@(mapcar (lambda (m)
		       `(:method ,@m))
		     atom-methods)

	   ;; call form-level function for non-atom forms
	   (:method ((form list) ,@extra-args)
	     (destructuring-bind (tag &rest args)
		 form
	       (with-current-form form
		 (,form-level-f tag args ,@extra-args)))))

	 ;; define the form-level function
	 (defgeneric ,form-level-f (tag args ,@extra-args)
	   (:documentation ,(concat "Form-level handler for " (symbol-name pass-name)))
	   ,@(mapcar (lambda (m)
		       (let ((tag (caar m))
			     (args (cdar m))
			     (body (cdr m)))
			 `(:method ((tag (eql ',tag)) args)
			    (destructuring-bind ,args
				args
			      ,@body))))
		     form-methods))))))


(defmacro defpassmethod/vl (pass-name form &body body)
  "Define a pass method.

Pass methods switch on the FORM. If FORM is a list containing only
an atom or a single specialiser, the method is added to the top-level
function. If FORM is a more complicated argument list, the method is added
to the form-level function."
  (declare (optimize debug))
  (if (top-level-function-method-p form)
      ;; method is for an atom
      `(defmethod ,(pass-top-level-function-name pass-name) ,form
	 ,@body)

      ;; method is for a structured form
      (destructuring-bind (tag &rest args)
	  form

	`(defmethod ,(pass-form-level-function-name pass-name) ((tag (eql ',tag)) args)
	   (destructuring-bind ,args
	       args
	     ,@body)))))


;;; ---------- Function-defining macros ----------



;;; ---------- Example ----------

;; (defpass/vl float-let ()
;;   (:documentation "Float LET and LET* blocks to the top of a module.")
;;   (:default nil))
;;   (:queue :post-typing)
;;   (:method ((n integer))
;;     n))

;; (defpassmethod/vl float-let ((n integer))
;;   n)

;; (defpassmethod/vl float-let (* &rest args)
;;   (:scheme recurse)))

;; (defcodefunction/vl typecheck (form)
;;   (:documentation "Type-check FORM.")
;;   (:queue typing)
;;   (:default (error 'unknown-form :hint "Provide a way to type-check the form."))
;;   )


;; (defcodemethod/vl typecheck (+ &rest operands)
;;   (let ((tys (mapcar #'typecheck operands)))
;;     (unless (every (rcurry #'subtype-p 'number) tys)
;;       (error 'type-error :expected-type 'number))
;;     (lub tys)))
