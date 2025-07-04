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


(defun nested-state-machine-context-p ()
  "Test whether we're in a nested state machine."
  (> (length *state-machines*) 1))


(defun ensure-state-machine-context (fun)
  "Test whether FUN appears within a state machine.

A SYNTAX-ERROR error is signalled if not."
  (unless (state-machine-context-p)
    (error 'syntax-error :form fun
			 :hint (format nil "Make sure ~a appears inside a TAGBODY" fun))))


(defun current-state-machine ()
  "Return the current state machine's data structure."
  (car *state-machines*))


(defun state-machine-state-variable (&optional (machine (current-state-machine)))
  "Return the state variable of MACHINE.

Defaults to the current state machine."
  (car machine))


(defun state-machine-initial-state (&optional (machine (current-state-machine)))
  "Return the initial state of MACHINE.

Defaults to the current state machine."
  (cadr machine))


(defun state-machine-state-labels (&optional (machine (current-state-machine)))
  "Return the state labels of MACHINE.

Defaults to the current state machine."
  (caddr machine))


(defun state-machine-all-state-labels ()
  "Return the state labels in this and any surrounding state machines."
  (foldr #'union (mapcar #'caddr *state-machines*) '()))


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

A DUPLICATE-STATE error is signalled if there are duplicates, either
in STATES or with the states of surrounding machines."
  (let ((sls (state-labels states)))
    ;; check all labels in the crrent machine are unique
    (unless (set-p sls)
      (error 'duplicate-state :states sls
			      :hint "Ensure the labels are unique within a TAGBODY"))

    ;; check for uniqueness against containing machines
    (when (some (lambda (labels)
		  (not (null (intersection sls labels))))
		(mapcar #'state-labels (cdr *state-machines*)))
      (error 'duplicate-state :states sls
			      :hint "Ensure the labels don't clash with those in a surrounding TAGBODY"))))


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


(defun state-label-p (label &optional machine)
  "Test that LABEL is a valid state label.

If MACHINE is supplied, the label is tested against that
machine's labels; otherwise the label may be in the current machine, or
in any surrounding machine."
  (if machine
      ;; test against specific machine
      (not (null (member label (state-machine-state-labels machine))))

      ;; test against current and all surrounding machines
      (not (null (member label (state-machine-all-state-labels))))))


(defun ensure-state-label (label)
  "Ensure that LABEL is a valid state label in the current context.

An UNKNOWN-STATE error is signalled for an unrecognised state."
  (unless (state-label-p label)
    (error 'unknown-state :state label
			  :hint "Make sure the label is valid in the current state machine.")))


(defun compile-state-machine (state-variable states)
  "Return the compiled form of the machine for STATES.

The current state is stored in STATE-VARIABLE, which should be a
unique variable name."
  (let* ((decls (let ((i 0))
		  (mapcar (lambda (label)
			    (prog1
				`(,label ,i :as :constant)
			      (incf i)))
			  (state-labels states))))

	 ;; prepend a state change to fall-through to the following state,
	 ;; rotating to the initial state if we fall out of the bottom
	 (states-with-fall-through (mapcar (lambda (state next)
					     (cons (car state)
						   (cons `(go ,next)
							 (cdr state))))
					   states
					   (rotate (state-labels states) -1)))

	 (new-states (mapcar (lambda (state)
			       (let ((label (car state))
				     (body (cdr state)))
				 (cons label (mapcar #'expand-macros body))))
			     states-with-fall-through)))

    ;; This compiled form works because we know we're going to float
    ;; the let blocks later, meaning that the state variable won't be
    ;; reset at each turn of the machine. If we stopped doing that for
    ;; any reason we'd need something different.
    `(let ,decls
       (let ((,state-variable ,(initial-state-label states)))
	 (case ,state-variable
	   ,@new-states)))))


(defmacro/vl tagbody (&body body)
  "Compile a state machine consisting of STATES."
  (let ((states (extract-tagbody-states body)))
    (ensure-unique-state-labels states)

    (with-gensyms (state-variable)
      (with-state-machine (state-variable states)
	(compile-state-machine state-variable states)))))


;; ---------- GO ----------

(defmacro/vl go (label)
  "Change state to LABEL."
  (ensure-state-machine-context 'go)
  (ensure-state-label label)

  (let ((code '()))
    (dolist (machine *state-machines*)
      (if (state-label-p label machine)
	  ;; jump to a local state, do the jump and return
	  (let ((state-variable (state-machine-state-variable machine)))
	    (if (null code)
		(setq code (list `(setf ,state-variable ,label)))
		(appendf code (list `(setf ,state-variable ,label))))

	    ;; wrap multiple jumps in a PROGN
	    (return (if (= (length code) 1)
			(car code)
			`(progn ,@code))))

	  ;; jump to a surrounding state, reset this state and continue
	  (let ((state-variable (state-machine-state-variable machine))
		(initial-state (state-machine-initial-state machine)))
	    (if (null code)
		(setq code (list `(setf ,state-variable ,initial-state)))
		(appendf code (list `(setf ,state-variable ,initial-state)))))))))
