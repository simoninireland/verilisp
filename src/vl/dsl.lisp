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


;;; ---------- Pass function generation ----------

(defun pass-top-level-function-name (pass-name)
  "Return the top-level function name for PASS."
  (intern (upcase (symbol-name pass-name))))


(defun pass-form-level-function-name (pass-name)
  "Return the form-level function name for PASS.

The default is the pass name followed by a suffix."
  (intern (upcase (concat (symbol-name (pass-top-level-function-name pass-name)) "/form"))))


;;; ---------- Macros ----------

;;; The standard pass queues

(defparameter *pre-typing-passes* (make-instance 'pass-queue)
  "The passes that are run before typing.

Pre-typing passes obviously don't have access to type information:
they have to operate purely on syntax. Macro expansion is an example
of a pre-typing pass.")


(defparameter *typing-passes* (make-instance 'pass-queue)
  "The passes that perform type-checking.

These passes populate the environment structure.")


(defparameter *post-typing-passes* (make-instance 'pass-queue)
  "The passes that are run after typeing.

Post-typing passes have access to the complete environment formed
by the type-checking passes.")


(defparameter *synthesis-passes* (make-instance 'pass-queue)
  "The synthesis passes.

These passes are run after everything else to convert the Verilisp
code into Verilog.")


;;; Mapping from tags to queues

(defparameter *pass-queue-tags* `((:pre-typing  ,*pre-typing-passes*)
				  (:typing      ,*typing-passes*)
				  (:post-typing ,*post-typing-passes*)
				  (:synthesis   ,*synthesis-passes*))
  "Alist from tags used in DEFPASS/VL to pass queues.")


;;; Define a new pass

(defmacro defpass/vl (&rest args)
  "Define a compiler nanopass.

The arguments consist of an optional pass queue tag, an optional
placement of the pass in that pass queue, an optional recursion
scheme, a (possibly empty) list of arguments that appear after the
form being processed, and an optional docstring.

Placements can be :APPEND or :PREPEND. Recursion schemata can be
:RECURSE or :FAIL.

The default behaviour is to define the new pass and append it to the
post-typing pass queue, recursing into the arguments of any forms
encountered."
  (declare (optimize debug))
  (let ((queue-tag (if-let ((tq (assoc (car args) *pass-queue-tags*)))
		     (progn
		       (setf args (cdr args))
		       (car tq))

		     ;; default uses the post-typing queue
		     :post-typing))
	(prepend-to-queue-p (cond ((eql (car args) :prepend)
				   (setf args (cdr args))
				   t)
				  ((eql (car args) :append)
				   (setf args (cdr args))
				   nil)
				  (t
				   ;; default is to append
				   nil)))
	(recursion-scheme (cond ((member (car args) '(:recurse :fail))
				 (setf args (cdr args))
				 (car args))
				(t
				 ;; default is to recurse
				 :recurse)))
	(pass-name (car args))
	(extra-args (cadr args))
	(docstring (progn
		     (setf args (cddr args))
		     (if (not (null args))
			 (if (stringp (car args))
			     (progn
			       (setf args (cdr args))
			       (car args))))))
	(body (if (not (null args))
		  (car args))))

    `(progn
       ;; store the pass name in the correct queue
       (add-pass-to-queue ',pass-name (cadr (assoc ,queue-tag *pass-queue-tags*)) :prepend ,prepend-to-queue-p)

       ;; define the generic functions
       (defgeneric ,(pass-top-level-function-name pass-name) (form ,@extra-args)
	 (:documentation ,(if docstring
			      docstring
			      "Compiler nanopass."))
	 ,(if body
	      ;; explicit body
	      `(:method (form ,@extra-args)
		 ,@body)

	      ;; default body signals an error on atom forms
	      `(:method (form ,@extra-args)
		 (declare (ignore form))
		 (error 'unknown-form)))

	 ;; call form-level function for non-atom forms
	 (:method ((form list) ,@extra-args)
	   (destructuring-bind (tag &rest args)
	       form
	     (with-current-form form
	       (,(pass-form-level-function-name pass-name) tag args ,@extra-args)))))

       (defgeneric ,(pass-form-level-function-name pass-name) (tag args ,@extra-args)
	 (:documentation ,(concat "Form-level handler for " (symbol-name pass-name)))

	 ,(cond ((eql recursion-scheme :fail)
		 ;; default body signals an error
		 `(:method (tag args ,@extra-args)
		    (declare (ignore tag args ,@extra-args))
		    (error 'unknown-form)))

		((eql recursion-scheme :recurse)
		 ;; default body recurses into arguments
		 (let ((rf (if extra-args
			       `(rcurry #',(pass-form-level-function-name pass-name) ,@extra-args)
			       `#',(pass-form-level-function-name pass-name))))
		   `(:method (tag args ,@extra-args)
		      (cons tag (mapcar ,rf args)))))

		(t
		 (error 'dsl-error :hint (format nil "Recursion scheme must be one of :RECURSE or :FAIL not ~s)" recursion-scheme))))))))


(defmacro defpassmethod/vl (pass-name form &body body)
  "Define a pass method.

Pass method switch on the FORM. If FORM is a list containing only
an atom or a single specialiser, the method is added to the top-level
function. If FORM is a more complicated argument list, the method is added
to the form-level function."
  (declare (optimize debug))
  (if (= (length form) 1)
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


;;; ---------- Example ----------

;; (defpass/vl float-let :post-typing ()
;;   "Float LET and LET* blocks to the top of a module.")


;; (defpassmethod/vl float-let ((n integer))
;;   n)


;; (defpassmethod/vl float-let (* &rest args)
;;   `(* ,@ (mapcar #'float-let args)))
