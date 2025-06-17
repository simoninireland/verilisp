;; State machine construction
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


;; ---------- Compiler state ----------

(defparameter *state-machines* nil
  "Stack of state machines.")


(defmacro with-state-machine ((state-variable states) &body body)
  "Enter a machine with the given STATES."
  `(unwind-protect
	(progn
	  (push (list ,state-variable
		      (initial-state-label ,states)
		      (state-labels ,states))
		*state-machines*)
	  ,@body)

     (pop *state-machines*)))


(defun state-machine-context-p ()
  "Test whether we're in a state machine."
  (> (length *state-machines*) 0))


(defun ensure-state-machine-context (fun)
  "Test whether FUN appears within a state machine.

A SYNTAX-ERROR error is signalled if not."
  (unless (state-machine-context-p)
    (error 'syntax-error :form fun
			 :hint "Make sure GO appears inside a TAGBODY")))


(defun current-state-machine-state-variable ()
  "Return the current state variable."
  (car (car *state-machines*)))


(defun current-state-machine-initial-state ()
  "Return the initial state of the current state machine."
  (cadr (car *state-machines*)))


(defun current-state-machine-state-labels ()
  "Return the state labels of the current state machine."
  (caddr (car *state-machines*)))


;; ---------- TAGBODY ----------

(defun state-labels (states)
  "Extract the labels of STATES."
  (mapcar #'car states))


(defun state-bodies (states)
  "Extract the bodies of STATES."
  (mapcar #'cdr states))


(defun initial-state-label (states)
  "Return the label of the first state in STATES."
  (caar states))


(defun ensure-unique-state-labels (states)
  "Ensure that the labels on STATES are unique.

A DUPLICATE-STATE error is signalled if there are duplicates."
  (let ((state-labels (state-labels states)))
    (unless (set-p state-labels)
      (error 'duplicate-state :states state-labels
			      :hint "Ensure the labels are unique within a TAGBODY"))))


;; This representation of states has the advantage that it
;; exactly matches the clauses of CASE, which is how we
;; implement the state machines, so they can be used directly.

(defun extract-tagbody-states (forms)
  "Turn a list of state labels and executable FORMS into a list of states.

Each state has is a list headed with the state tag and followed by
the body of that state. It is legal for the first state in the TAGBODY
to be unlabelled, in which case a label is synthesised for it. The
states are returned in lexical order."
  (flet ((extract-states (states form)
	   (if (symbolp form)
	       ;; form is a state label, create a new state
	       (cons (list form) states)

	       ;; form is a part of the body of a state, append to the current state
	       (let ((state (car states)))
		 (cons (append state (list form)) (cdr states))))))

    (let ((states (reverse (remove-nulls (foldr #'extract-states forms '(()))))))
      ;; it is possible that the first state is unlabelled
      (if (symbolp (caar states))
	  states

	  ;; first state is unlabelled, add a label
	  (let ((first-state-label (gensym)))
	    (cons (cons first-state-label (car states))
		  (cdr states)))))))


(defun state-label-p (label)
  "Test that LABEL is a valid state label."
  (and (variable-declared-p label)
       (eql (variable-property label :role :default nil) :state-label)))


(defun ensure-state-label (label)
  "Ensure that LABEL is a valid state label.

An UNKNOWN-STATE error is signalled for an unrecognised state."
  (unless (state-label-p label)
    (error 'unknown-state :state label
			  :hint "Make sure the label is valid in the current state machine.")))


(defun compile-state-machine (state-variable states)
  "Return the compiled form of the machine for STATES.

The current state is stored in STATE-VARIABLE, which should be a
unique variable name."
  (let ((decls (let ((i 0))
		 (mapcar (lambda (label)
			   (prog1
			       `(,label ,i :as :constant :role :state-label)
			     (incf i)))
			 (state-labels states))))

	(new-states (mapcar (lambda (state)
			      (let ((label (car state))
				    (body (cdr state)))
				(cons label (mapcar #'expand-macros body))))
			    states)))

    ;; This compiled form works because we know we're going to float
    ;; the let blocks later, meaning that the state variable won't be
    ;; reset at each turn of the machine. If we stopped doing that for
    ;; any reason we'd need something different.
    `(let ,decls
       (let ((,state-variable ,(initial-state-label states) :role :state-variable))
	 (case ,state-variable
	   ,@new-states)))))


(defmacro tagbody/vl (&body body)
  "Compile a state machine consisting of STATES."
  (let ((states (extract-tagbody-states body)))
    (ensure-unique-state-labels states)

    (with-gensyms (state-variable)
      (with-state-machine (state-variable states)
	(compile-state-machine state-variable states)))))


;; ---------- GO ----------

(defmacro go/vl (label)
  "Change state to LABEL."

  ;; can only see GO inside a lexically-containing TAGBODY
  (ensure-state-machine-context 'go)

  ;; must be a legal state of the curent machine
  ;; (this will change when we handle nested machines)
  (unless (member label (current-state-machine-state-labels))
    (error 'unknown-state :state label
			  :hint "Label must be declared in the current TAGBODY"))

  (let ((state-variable (current-state-machine-state-variable)))
    `(setf ,state-variable ,label)))
