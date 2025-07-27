;; The compiler context
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


;; ---------- Environment ----------

(defparameter *global-environment* (empty-environment)
  "The global environment for the compiler.")


(defparameter *current-frame* (attach-frame (make-frame) *global-environment*)
  "The current frame (environment) for the compiler.")


(defun clear-global-environment ()
  "Clear the global environment.

This should, in principle, never be needed because the environment
should be handled correctly using WITH-NEW-FRAME. However...."
  (setf *global-environment* (empty-environment))
  (setf *current-frame* (attach-frame (make-frame) *global-environment*)))


(defun current-frame ()
  "Return the current frame of the environment."
  *current-frame*)


(defmacro in-frame (f &body body)
  "Run BODY in an environment consisting solely of F."
  `(let ((*current-frame* f))
     ,@body))


(defmacro in-global-environment (&body body)
  "Run BODY in the global environment."
  `(in-frame *global-environment*))


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


(defun variables-declared-in-current-frame ()
  "Return the variables declared in only the shallowest frame of the environment."
  (get-frame-names (current-frame)))


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


(defun declare-macro (m f)
  "Declare M as a macro with body F in the current environment."
  (declare-variable m `((name ,m)
			(initial-value ,f) (as macro))))


(defun macro-declared-p (m)
  "Test whether M is declared as a macro in the global environment."
  (and (variable-declared-p m)
       (eql (get-representation m) 'macro)))


;;---------- Common properties ----------

(defun get-type (n)
  "Return the type of N.

This is the assigned type if there is one, or the
inferred type if not."
  (or (variable-property n 'type :default nil)
      (variable-property n 'inferred-type)))


(defun get-representation (n)
  "Return the representation of N."
  (variable-property n 'as))


(defun get-initial-value (n)
  "Return the initial value of N."
  (variable-property n 'initial-value :default 0))


(defun get-direction (n)
  "Return the direction of N."
  (variable-property n 'direction))


;; ---------- Form context ----------

(defparameter *current-form-queue* nil
  "The current form being evaluated.

This is a stack of forms being evaluated, used to contextualise
conditions and change synthesis based on a form's position in
the larger program.")


(defmacro with-current-form (form &body body)
  "Execute BODY within the current FORM.

Any conditions reported in BODY will be pointed as FORM as the current
form."
  `(unwind-protect
	(progn
	  (push ,form *current-form-queue*)
	  ,@body)

     (pop *current-form-queue*)))


;; form accessors

(defun current-form ()
  "Return the current form."
  (car *current-form-queue*))


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
;; We pass in a queue (list) of forms because we ma need more context than
;; just the form we're interested in.

(defun operator-form-p (&optional (formq *current-form-queue*))
  "Test whether the head of FORMQ is an operator."
  (member (caar formq) '(+ - *
			 << >>
			 = /= < <= > >=
			 aref bref)))


(defun assignment-form-p (&optional (formq *current-form-queue*))
  "Test wheterthe head of FORMQ is an assignment."
  (member (caar formq) '(setq setf)))


(defun conditional-form-p (&optional (formq *current-form-queue*))
  "Test wheter the head of FORMQ is a conditional form."
  (member (caar formq) '(if case)))


(defun let-form-p (&optional (formq *current-form-queue*))
  "Test whether the head of FORMQ is a LET block."
  (eql (caar formq) 'let))


(defun block-form-p (&optional (formq *current-form-queue*))
  "Test whether the head of FORMQ is a block-introducing form."
  (eql (caar formq) '@))


(defun tagbody-form-p (&optional (formq *current-form-queue*))
  "Test whether the head of FORMQ is a TAGBODY."
  (eql (caar formq) 'tagbody))


(defun decl-form-p (&optional (formq *current-form-queue*))
  "Test whether the head of FORMQ is a declaration of a LET block.

A decl consists of a symbol representing a declared variable, and
occurs immediately within a LET form."
  (and (symbolp (caar formq))
       (variable-declared-p (caar formq))
       (let-form-p (cdr formq))))


(defun module-form-p (&optional (formq *current-form-queue*))
  "Test whether the head of FORMQ is a module form."
  (eql (caar formq) 'module))


;; context classifiers

(defun in-top-level-context-p ()
  "Test whether the current context is top-level."
  (null (containing-form)))


(defun in-module-context-p ()
  "Test whether the current context is the body of a module."
  (not (in-block-context-p)))


(defun in-block-context-p ()
  "Test whether the current context is within an @-block."
  (find-if-list #'block-form-p (cdr *current-form-queue*)))


(defun in-setf-context-p ()
  "Test if we're in a SETF or SETQ context."
  (find-if-list #'assignment-form-p *current-form-queue*))


(defun in-expression-context-p ()
  "Test if we're in an expression context."
  (and (or (operator-form-p)
	   (conditional-form-p))
       (find-if-list (lambda (formq)
		       (or (assignment-form-p formq)
			   (decl-form-p formq)))
		     (cdr *current-form-queue*))))


(defun in-state-machine-context-p ()
  "Test whether the current context is within a TAGBODY form."
  (find-if-list #'tagbody-form-p (cdr *current-form-queue*)))
