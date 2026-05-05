;;;; The compiler context
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


;;; ---------- Environment ----------

(defparameter *core-environment* (empty-environment)
  "The core environment for the compiler.

This frame contains all the core elements of Verilisp such as the
core macros. It shouldn't change after the system is loaded.")


(defparameter *global-environment* (attach-frame (make-frame) *core-environment*)
  "The global environment for the compiler.

This frame contains everything added to Verilisp in the current session,
including macros and modules.")


(defparameter *current-frame* (attach-frame (make-frame) *global-environment*)
  "The current environment for the compiler.

This frame holds the shallowest definitions currently in scope,
and is attached to the frames of the surrounding scopes back
to *GLOBAL-ENVIRONMENT* and *CORE-ENVIRONMENT*.")


(defun clear-global-environment ()
  "Clear the global environment.

This forgets everything apart from core Verilisp."
  (setf *global-environment* (attach-frame (make-frame) *core-environment*))
  (setf *current-frame* (attach-frame (make-frame) *global-environment*)))


(defun current-frame ()
  "Return the current frame of the environment."
  *current-frame*)


(defmacro in-frame (f &body body)
  "Run BODY in an environment consisting solely of F."
  `(let ((*current-frame* ,f))
     ,@body))


(defmacro in-core-environment (&body body)
  "Run BODY in the core environment.

This should only be used during system loading."
  `(in-frame *core-environment*
     ,@body))


(defmacro in-global-environment (&body body)
  "Run BODY in the global environment."
  `(in-frame *global-environment*
     ,@body))


(defmacro with-frame (f &body body)
  "Attach F to the current environment for BODY."
  `(let ((*current-frame* (attach-frame ,f *current-frame*)))
     (unwind-protect
	  (progn
	    ;; run the body in the extended environment
	    ,@body)

       ;; detach the attached frame and restore the environment
       (progn
	 (detach-frame *current-frame*)))))


(defmacro with-new-frame (&body body)
  "Run BODY in an environment extended with an empty frame onto the current environment."
  `(with-frame (make-frame)
     ,@body))


(defmacro with-detached-frame (lenv &body body)
  "Run BODY in the parent of the current frame.

The shallowest frame is detached and bound to LENV. Essentially this
removes all the entries in the local frame for the duration of BODY
while making it available for access explicitly if desired. The
shoowest frame is re-attached at the end of BODY."
  (with-gensyms (penv)
    `(let* ((,penv (parent-frame *current-frame*))
	    (,lenv (detach-frame *current-frame*))
	    (*current-frame* ,penv))

       (unwind-protect
	    (progn
	      ,@body)

	 ;; re-attach the detached frame
	 (attach-frame ,lenv *current-frame*)))))


(defun declare-variable (n props)
  "Declare a new variable N with properties PROPS in the global environment."
  (declare-environment-variable n props (current-frame)))


(defun variable-declared-p (n)
  "Test whether variable N is declared in the global environment."
  (variable-declared-in-environment-p n (current-frame)))


(defun ensure-variable-declared (n)
  "Signal an UNKNOWN-VARIABLE error is N is not declared."
  (unless (variable-declared-p n)
    (error 'unknown-variable :variables n
			     :hint "Make sure variable is in scope")))


(defun variables-declared ()
  "Return the variables declared in the current global environment."
  (get-environment-names (current-frame)))


(defun variables-declared-in-frame (f)
  "Return the variables declared in frame F."
  (get-frame-names f))


(defun variables-declared-in-current-frame ()
  "Return the variables declared in only the shallowest frame of the environment."
  (variables-declared-in-frame (current-frame)))


(defun variable-declared-in-current-frame-p (n)
  "Test that N is declared in the current frame."
  (variable-declared-in-frame-p n (current-frame)))


(defun ensure-variable-declared-in-current-frame (n)
  "Test that N is declared in the current frame.

An UNKNOWN-VARIABLE error is signalled if not."
  (unless (variable-declared-in-current-frame-p n)
    (error 'unknown-variable :variable n
			     :hint "Is this variable declared in another frame, not the local one?")))


(defun variable-properties (n)
  "Return the property list of variable N in the environment."
  (get-environment-properties n (current-frame)))


(defun variable-property (n p &key default)
  "Return the value of property P of variable N in the environment."
  (get-environment-property n p (current-frame) :default default))


(defun set-variable-property (n p v)
  "Set the value of property P of variable N in the environment to V."
  (set-environment-property n p v (current-frame)))


(defun set-variable-property-unless-set (n p v)
  "Set the value of property P of N to V unless is already has a value.

This is used when setting defaults."
  (unless (variable-property n p :default nil)
    (set-variable-property n p v)))


(defun set-variable-properties (n props)
  "Set the values of properties PROPS of variable N in the global environment.

PROPS should be an alist mapping property names to their values."
  (dolist (p props)
    (set-variable-property n (car p) (cadr p))))


(defun set-variable-properties-unless-set (n props)
  "Set all unset properties in PROPS of N.

This is used for setting defaults."
  (dolist (p props)
    (set-variable-property-unless-set n (car p) (cadr p))))


(defun forget-variable (n)
  "Forget N from the current environment."
  (forget-environment-variable n (get-frame-declaring n (current-frame))))


;;---------- Common properties ----------

(defun get-type (n)
  "Return the type of N."
  (variable-property n 'type))


(defun get-representation (n)
  "Return the representation of N."
  (variable-property n 'as))


(defun get-initial-value (n &key default)
  "Return the initial value of N, with optional DEFAULT."
  (variable-property n 'initial-value :default default))


(defun get-direction (n)
  "Return the direction of N."
  (variable-property n 'direction))


;;; ---------- Form context ----------

(defparameter *current-form-queue* nil
  "The current form being evaluated.

This is a stack of forms being evaluated, used to contextualise
conditions and change synthesis based on a form's position in
the larger program.")


(defmacro with-current-form (form &body body)
  "Execute BODY within the current FORM.

Any conditions reported in BODY will be pointed as FORM as the current
form."
  `(let ((*current-form-queue* (cons ,form *current-form-queue*)))
     ,@body))


(defmacro with-current-form-queue (formq &body body)
  "Run BODY with FORMQ as the form queue."
   `(let ((*current-form-queue* ,formq))
     ,@body))


;; form accessors

(defun current-form ()
  "Return the current form."
  (car *current-form-queue*))


(defun form-head (form)
  "Return the head of the current form.

This is safe for atomic and list forms."
  (if (atom form)
	form
	(car form)))


(defun current-form-head ()
  "Return the head of the current form.

This function is safe for atom forms or list forms."
  (form-head (current-form)))


(defun containing-form (&optional form)
  "Return the form containing the current form.

If FORM is provided then return the shallowest containing form
that is that form."
  (labels ((find-form (l)
	     (cond ((null l)
		    nil)

		   ((eql (caar l) form)
		    (car l))

		   (t
		    (find-form (cdr l))))))

    (if form
	;; search for the shallowest containing form with the given tag
	(find-form (cdr *current-form-queue*))

	;; extract the immediately containing form
	(cadr *current-form-queue*))))


(defun containing-containing-form ()
  "Return the form containing the containing form."
  (if (> (length *current-form-queue*) 2)
      (caddr *current-form-queue*)))


;; form classifiers
;; We pass in a queue (list) of forms because we may need more context than
;; just the form we're interested in.

(defun integer-form-p (&optional (form (current-form)))
  "Test whether the current form is an integer literal."
  (integerp form))


(defun literal-form-p (&optional (form (current-form)))
  "Test whether the current formis a literal.

At present only integer literals are available in Verilisp."
  (integer-form-p form))


(defun variable-form-p (&optional (form (current-form)))
  "Test whether the current form is a variable reference.

Variables are symbol identifiers that are in scope."
  (let ((h (form-head form)))
    (and (symbolp h)
	 (variable-declared-p h))))


(defun element-form-p (&optional (form (current-form)))
  "Test whether the current form is an element of a larger value.

Currently covers array elements and bit extractions."
  (member (form-head form) '(aref bref)))


(defun operator-form-p (&optional (form (current-form)))
  "Test whether the current form is an operator."
  (member (form-head form) '(+ - *
			     << >>
			     = /= < <= > >=
			     make-bitfields)))


(defun assignment-form-p (&optional (form (current-form)))
  "Test whether the current form is an assignment."
  (member (form-head form) '(setq setf)))


(defun conditional-form-p (&optional (form (current-form)))
  "Test whether the current form is a conditional form."
  (member (form-head form) '(if case)))


(defun type-operator-form-p (&optional (form (current-form)))
  "Test whether the current form is a type operator."
  (member (form-head form) '(coerce the)))


(defun let-form-p (&optional (form (current-form)))
  "Test whether the current form is a LET block."
  (member (form-head form) '(let let*)))


(defun block-form-p (&optional (form (current-form)))
  "Test whether the current form is a block-introducing form."
  (eql (form-head form) '@))


(defun tagbody-form-p (&optional (form (current-form)))
  "Test whether the current form is a TAGBODY."
  (eql (form-head form) 'tagbody))


(defun module-form-p (&optional (form (current-form)))
  "Test whether the current form is a module form."
  (eql (form-head form) 'module))


;; context classifiers

(defun in-context-p (cl)
  "Test whether there is some form in the form queue matchiong CL.

CL should be a function of no variables, operating over the current
form queue. Typically tis will be a form classifier, or a function
built from them.

Return the first matching form, or NIL."
  (block found-context
    (let ((context (cdr *current-form-queue*)))
      (maplist (lambda (formq)
		 (with-current-form-queue formq
		   (when (funcall cl)
		     (return-from found-context (current-form)))))
	       context)

      ;; if we get here, we've not found a matching form
      nil)))


(defun in-top-level-context-p ()
  "Test whether the current context is top-level."
  (null (containing-form)))


(defun in-module-context-p ()
  "Test whether the current context is the body of a module."
  (not (in-block-context-p)))


(defun in-block-context-p ()
  "Test whether the current context is within an @-block."
  (in-context-p #'block-form-p))


(defun in-combinatorial-block-context-p ()
  "Test whether the current context is a combinatorial block.

Combinatorial blocks have sensitivities that depend on the values
of signals, not on edges (which are synchronous blocks)."
  (in-context-p (lambda ()
		  (and (block-form-p)
		       (destructuring-bind (sensitivities &rest body)
			   (cdr (current-form))
			 (not (edge-trigger-p sensitivities)))))))


(defun in-synchronous-block-context-p ()
  "Test whether the current context is a synchronous block.

Synchronous blocks have sensitivities that depend on signal edges."
  (in-context-p (lambda ()
		  (and (block-form-p)
		       (destructuring-bind (sensitivities &rest body)
			   (cdr (current-form))
			 (edge-trigger-p sensitivities))))))


(defun in-assignment-context-p ()
  "Test if we're in a SETF or SETQ context."
  (in-context-p #'assignment-form-p))


(defun in-expression-context-p ()
  "Test if we're in an expression context, either directly or in an assignment."
  (or (operator-form-p (containing-form))
      (assignment-form-p (containing-form))))


(defun in-state-machine-context-p ()
  "Test whether the current context is within a TAGBODY form."
  (in-context-p #'tagbody-form-p))
