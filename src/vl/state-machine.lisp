;;;; State machine construction
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
(declaim (optimize debug))


;;; ---------- State representation ----------

(defclass state ()
  ((label
    :documentation "The state label."
    :initarg :label
    :initform nil
    :reader label)
   (body
    :documentation "The forms in the body of the state."
    :initarg :body
    :initform nil
    :accessor body)
   (successor
    :documentation "The state this state will fall-through to (if it does)."
    :initform nil
    :accessor successor-state)
   (synthetic-p
    :documentation "Flag indicating that the state is synthetic."
    :initform nil
    :reader synthetic-p))
  (:documentation "Abstract state in a state machine."))


(defmethod initialize-instance :after ((s state) &key label &allow-other-keys)
  (unless label
    ;; no label given, construct one and mark it as synthetic
    (setf (slot-value s 'label) (gensym))
    (setf (slot-value s 'synthetic-p) t)))


(defun append-to-body (state form)
  "Append FORM to the body of STATE."
  (appendf (body state) (list form)))


(defun falls-through-p (state)
  "Test whether STATE has a successor state.

States without successor states exit through explicit GO forms; those
with successors will have a GO appended to them."
  (not (null (successor-state state))))


;;; ---------- Jump targets ----------

(defvar *jump-targets* nil
  "Set of jump targets in the current machine synthesis.

A jump target is a state label that is the target of a GO expression.")


(defun clear-jump-targets ()
  "Clear the list of jump targets."
  (setq *jump-targets* nil))


(defun mark-state-label-as-jump-target (label)
  "Mark LABEL as the target for a GO."
  (setq *jump-targets* (union *jump-targets* (list label))))


(defun jump-target-p (label)
  "Test whether LABEL is ever jumped to explicitly."
  (member label  *jump-targets*))


;;; ---------- TAGBODY ----------

(defun state-label-p (form)
  "Test whether FORM is a new state label.

This simply tests whether FORM is a symbol."
  (symbolp form))


(defun extract-states (forms)
  "Extract the states from FORMS.

Return a list of lists, each element being a state label and the
corresponding state body."
  (flet ((extract-state (states form)
	   (if (state-label-p form)
	       ;; new state
	       (cons (list form) states)

	       (progn
		 ;; check whether we have an unlabelled initial state
		 (when (null states)
		   ;; construct a new state
		   (setq states (list (list (gensym)))))

		 ;; add form to current state
		 (let* ((current-state (car states))
			(current-body (cdr current-state)))
		   (if (null current-body)
		       (setf (cdr current-state) (list form))
		       (setf (cdr (last current-body)) (list form)))
		   states)))))

    (reverse (foldr #'extract-state forms '()))))


(defpassmethod compute-type (tagbody &rest body)
  (declare (optimize debug))

  (let* ((states (extract-states body))
	 (state-labels (mapcar #'car states))
	 (state-bodies (mapcar #'cdr states)))

    (with-new-frame
      ;; add labels to frame
      (dolist (n state-labels)
	(declare-variable n '((as label)
			      (ignorable t))))

      ;; compute-type the bodies
      (dolist (b state-bodies)
	(compute-type (with-implicit-progn b)))

      ;; tagbody doesn't return a value (yet)
      t)))


(defpassmethod read-variables (tagbody &rest body)
  (foldr (lambda (deps form)
	   (if (state-label-p form)
	       deps
	       (union deps (read-variables form))))
	 body '()))


(defpassmethod compute-dependencies (tagbody &rest body)
  (let* ((states (extract-states body))
	 (state-bodies (mapcar #'cdr states)))

    ;; compute-type the bodies
    (dolist (b state-bodies)
      (compute-dependencies (with-implicit-progn b)))))


;;; ---------- State machine transformation ----------

;;; We elaborate TAGBODY-style state machies in CASE-style machines that
;;; can be directly synthesised. This involes a lot of processing, to identify
;;; states and the appropriate transitions.
;;;
;;; States may also be synthesised if required to ensure dataflow consistency.

;;; TODO: Rewrite this function as a pass (will require more machinery in the DSL)

(defgeneric parse-tagbody-forms-sexp (fun args forms current-state exit-state)
  (:documentation "Parse FUN applied to ARGS.

Methods on this function should construct a state machine for
FUN applied to ARGS, integrate it into CURRENT-STATE, and then
proceed to parse FORMS (typically by calling PARSE-TAGBODY-FORMS).

EXIT-STATE is the 'fall-through' state that the form should transition
to when it exits.

Returns a list consisting of a list of the states created, with
the entry state first.")
  (:method (fun args forms current-state exit-state)
    ;; "normal" form, add to body of current state
    (appendf (body current-state) (list (cons fun args)))
    (parse-tagbody-forms forms
			 current-state
			 exit-state)))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'tagbody)) args forms current-state exit-state)
  ;; nested machine, start a new state for this machine
  (let* ((trailing-states (if forms
			      (parse-tagbody-forms forms
						   nil
						   exit-state)))
	 (trailing-state (if (null trailing-states)
			     exit-state
			     (car trailing-states)))
	 (nested-states (parse-tagbody-forms args
					     current-state
					     trailing-state)))

    (append nested-states
	    (if trailing-states
		trailing-states))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'progn)) args forms current-state exit-state)
  (let* ((trailing-states (if forms
			      (parse-tagbody-forms forms
						   nil exit-state)))
	 (trailing-state (if trailing-states
			     (car trailing-states)
			     exit-state))
	 (nested-states (parse-tagbody-forms args
					     current-state
					     trailing-state)))

    (append nested-states
	    (if trailing-states
		trailing-states))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'if)) args forms current-state exit-state)
  (declare (optimize debug))

  (destructuring-bind (condition then-branch &rest else-branch)
      args

    (let* ((trailing-states (parse-tagbody-forms forms
						 nil exit-state))
	   (trailing-state (if trailing-states
			       (car trailing-states)
			       exit-state))

	   (then-states (parse-tagbody-forms (list then-branch)
					     nil trailing-state))
	   (then-state (car then-states))

	   (else-states (if else-branch
			    (parse-tagbody-forms else-branch
						 nil trailing-state)))
	   (else-state (if else-states
			   (car else-states))))

      ;; compile the conditional, with optimisations
      (cond

	    (t
	     ;; default creates new states for both arms
	     (let ((cform (if else-states
			      ;; two arms
			      (prog1
				  `(if ,condition
				       (go ,(label then-state))
				       (go ,(label else-state)))
				(mark-state-label-as-jump-target (label then-state))
				(mark-state-label-as-jump-target (label else-state)))

			      ;; one arm, jump to the trailing state on false
			      (prog1
				  `(if ,condition
				       (go ,(label then-state))
				       (go ,(label trailing-state)))
				(mark-state-label-as-jump-target (label then-state))
				(mark-state-label-as-jump-target (label trailing-state))))))

	       ;; add condition to current state
	       (append-to-body current-state cform)

	       ;; return all the states created
	       (append then-states
		       (if else-states
			   else-states)
		       (if trailing-states
			   trailing-states))))))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'case)) args forms current-state exit-state)
  (destructuring-bind (condition &rest cases)
      args

    ;; TODO Optimise this like IF

    (let* ((trailing-states (parse-tagbody-forms forms nil exit-state))
	   (trailing-state (if trailing-states
			       (car trailing-states)
			       exit-state))
	   (arms (mapcar (lambda (form)
			   (destructuring-bind (val &rest arm-body)
			       form
			     (list val (parse-tagbody-forms arm-body nil trailing-state))))
			 cases))
	   (arm-states (apply #'append (mapcar #'cadr arms)))
	   (new-arms (mapcar (lambda (arm)
			       (destructuring-bind (val nested-states)
				   arm
				 (let* ((nested-state (car nested-states))
					(nested-label (label nested-state)))
				   `(,val (go ,nested-label)))))
			     arms)))

      ;; add new case to current state
      (let ((cform `(case ,condition
		      ,@new-arms)))
	(append-to-body current-state cform))

      (append arm-states
	      trailing-states))))


(defun chew-unreachable-code (current-state forms)
  "Chew-up any unreachable code in CURRENT-STATE from FORMS.

Forms are deleted until either FORMS is exhausted or we hit
a state marker. An UNREACHABLE-CODE warning is signalled if code
is skipped."
  (when (not (or (null forms)
		 (state-label-p (car forms))))
    ;; next form does not start a new state, and so is unreachable
    ;; report against the offending (unreachable) form, not the current form
    (with-current-form (car forms)
      (warn 'unreachable-code :label (label current-state)
			      :hint "Check the logic"))

    ;; skip to the next state marker
    (do ()
	((or (null forms)
	     (state-label-p (car forms)))
	 forms)
      (setq forms (cdr forms))))

  ;; return the remaining forms (if any)
  forms)


(defmethod parse-tagbody-forms-sexp ((fun (eql 'go)) args forms current-state exit-state)
  (let ((label (car args)))
    ;; add form to current state
    (append-to-body current-state (cons fun args))
    (mark-state-label-as-jump-target label)

    ;; current state is ended and doesn't fall-through
    (setf (successor-state current-state) nil)

    ;; skip any unreachable code on this path, and continue parsing from there
    (if-let ((newforms (chew-unreachable-code current-state forms)))
      (parse-tagbody-forms newforms
			   nil
			   exit-state))))


(defun count-tagbody-forms (forms)
  "Return the number of states in FORMS."
  (let ((states 0))

    ;; count the state markers in the forms
    (dolist (form forms)
      (when (state-label-p form)
	(incf states)))

    ;; add one if the initial state wasn't labelled
    (unless (state-label-p (car forms))
      (incf states))

    states))


(defun parse-tagbody-forms (forms current-state exit-state)
  "Parse FORMS as the body of a TAGBODY, returning a state machine.

The FORMS are built into CURRENT-STATE until there is a state
change. If a state falls-through, it lands in EXIT-STATE.

Return a list of of states created, initial state first."
  (declare (optimize debug))

  (if (null forms)
      ;; no more forms to process
      (progn
	(if current-state
	    (when exit-state
		;; we have a current state
		(setf (successor-state current-state) exit-state) nil)

	    (when exit-state
		;; we don't have a current state, create one
		(let ((jump-state (make-instance 'state)))
		  (setf (successor-state jump-state) exit-state)
		  (list jump-state)))))

      ;; forms to do
      (destructuring-bind (form &rest rest)
	  forms

	(with-current-form form
	  (if (state-label-p form)
	      ;; new state marker
	      (let ((new-state (make-instance 'state :label form)))
		(when current-state
		  ;; link current state to new state
		  (setf (successor-state current-state) new-state))

		;; use this new state as the current state going forward
		(cons new-state
		      (parse-tagbody-forms rest new-state exit-state)))

	      ;; executable form
	      (if (null current-state)
		  ;; no current state, create one and re-parse
		  (let ((initial-state (make-instance 'state)))
		    (cons initial-state
			  (parse-tagbody-forms forms initial-state exit-state)))

		  (if (listp form)
		      ;; handle sexp
		      (destructuring-bind (fun &rest args)
			  form
			(parse-tagbody-forms-sexp fun args
						  rest
						  current-state exit-state))

		      (progn
			;; singleton form that isn't a state marker
			(append-to-body current-state form)
			(parse-tagbody-forms rest current-state exit-state)))))))))


(defun build-state-machine (forms)
  "Parse FORMS as the body of a TAGBODY, building a state machine.

The resulting machine is necessarily 'top-level', not contained in
another machine. It has a passivating state added to the end, which it
will remain in if it ever gets to that point. This allows machines to
be built that run once and then stop. Use explicit GO forms, or
looping macros like FOREVER, to keep the machine running."
  (declare (optimize debug))

  ;; clear the jump targets table
  (clear-jump-targets)

  ;; parse with a final passivating state
  (let* ((passive-state (make-instance 'state))
	 (states (parse-tagbody-forms forms nil passive-state)))

    ;; link all states to their successors where necessary
    (dolist (state states)
      (if (falls-through-p state)
	  (append-to-body state `(go ,(label (successor-state state))))))

    ;; return all the states, passivating state last
    (append states (list passive-state))))


;;; TODO: Do this before linking states above

(defun merge-state-machine-empty-states (machine)
  "Return an alist mapping states in MACHINE to only the necessary states.

A state is unnecessary if it consists purely of a GO to another state."
  (declare (optimize debug))

  (labels ((mergeable-state (m)
	     (let ((b (body m)))
	       (if (and (synthetic-p m)			      ; a synthetic state
			(not (null b))			      ; ... with a body not empty
			(= (length b) 1)                      ; ... containing a single form
			(eql (caar b) 'go)                    ; ... which is a GO
			(not (eql (cadr (car b)) (label m)))) ; ... and isn't back to the same state

		   ;; mergeable, return its target state
		   (destructuring-bind (fun target)
		       (car b)
		     target))))

	   (merge-states (states m)
	     (if-let ((target (mergeable-state m)))
	       (progn
		 ;; state is mergeable
		 ;; if target is merged itself, use the merged state
		 (if-let ((m (assoc target states)))
		   (setq target (cadr m)))

		 ;; record the merge
		 (cons (list (label m) target) states))

	       ;; state is real, keep it
	       states)))

    (foldr #'merge-states machine '())))


(defun get-label-from-merged-states (label merged-states)
  "Return the label that should be used for LABEL under MERGED-STATES.

Return LABEL if the the state is not merged."
  (if-let ((m (assoc label merged-states)))
    ;; state is merged with another, return that state
    (get-label-from-merged-states (cadr m) merged-states)

    ;; state is un-merged, return it
    label))


(defun synthesise-state-machine (args)
  "Return the code for ARGS as a state machine."
  (declare (optimize debug))

  (let* ((machine (build-state-machine args))
	 (merged-states (merge-state-machine-empty-states machine))
	 (state-labels (remove-if (lambda (l)
				    (assoc l merged-states))
				  (mapcar #'label machine)))
	 (states (remove-if (lambda (m)
			      (not (member (label m)
					   state-labels)))
			    machine))
	 (label-decls (mapcar (lambda (label index)
				`(,label ,index))
			      state-labels
			      (iota (length state-labels))))
	 (declaration `(declare (as constant ,@state-labels)
				(ignorable ,@state-labels)))
	 (clauses (foldr (lambda (form state)
			   (let ((l (label state))
				 (b (body state)))
			     (append form `((,l
					     ,@b)))))
			 states '())))

    ;; warn about the number of states inferred if different from that specified
    (let ((given (count-tagbody-forms args))
	  (inferred (length clauses)))
      (when (/= given inferred)
	(warn 'resources-created
	      :description (format nil "TAGBODY expanded to ~a states (from ~a in the original source code)"
				   inferred given))))

    (with-gensyms (state-variable)
      ;; record the current state machine variable
      (declare-variable 'tagbody-state-variable `((type unsigned-byte)
						  (initial-value ,state-variable)))

      ;; record the merge table
      (declare-variable 'tagbody-merged-states `((type cons)
						 (initial-value ,merged-states)))

      ;; synthesise the machine
      (add-frames `(let ,label-decls
		     ,declaration

		     (let ((,state-variable ,(car state-labels)))
		       (declare (type (unsigned-byte ,(bits-for-integer (length state-labels))) ,state-variable)
				(as register ,state-variable))

		       (case ,state-variable
			 ,@clauses)))))))


(defpassmethod elaborate-state-machines (tagbody &rest body)
  (declare (optimize debug))

  (destructuring-bind (newbody newenv)
      (float-let-blocks (with-implicit-tagbody body))

    (destructuring-bind (newfun &rest newargs)
	newbody

      (unless (eql newfun 'tagbody)
	(error "Incorrectly floated LET blocks in TAGBODY"))

      ;; we hold on to the NEWENV frame because it contains all the information
      ;; we've already extracted about the variables -- and these were the only
      ;; ones in scope when the code was analysed, with others beng created
      ;; by SYNTHESISE-STATE-MACHINE. which adds the necessary types and
      ;; representations directly
      ;;
      ;; We put the state machine synthesis into its own frame so that
      ;; it can declare the state variable and merge table metavariables
      ;; for use in its own further transformation.

      (with-new-frame
	(let* ((synth (synthesise-state-machine newargs))
	       (p (if newenv
		      ;; float the locally-declared varables around the state machine
		      (let ((newdecls (add-local-frame-to-decls
				       (mapcar (lambda (np)
						 (destructuring-bind (n props)
						     np
						   (list n
							 (get-environment-property n 'initial-value newenv :default 0))))
					       (decls newenv))
				       newenv)))
			`(let ,newdecls
			   ,synth))

		      ;; no locally-declared variables in body
		      synth)))

	  (elaborate-state-machines p))))))


;;; ---------- GO ----------

(defpassmethod compute-type (go label)
  (declare (optimize debug))

  (let* ((merged-states (if (variable-declared-p 'tagbody-merged-states)
			    (get-initial-value 'tagbody-merged-states)))

	 ;; re-write label if we have a merge table
	 (state-label (if merged-states
			  (get-label-from-merged-states label merged-states)
			  label)))

    ;; ensure label is in scope
    (unless (variable-declared-p state-label)
      (error 'unknown-state :label label))

    ;; GO doesn't really have a type
    t))


(defpassmethod compute-dependencies (go label)
  nil)


(defpassmethod read-variables (go label)
  '())


(defpassmethod elaborate-state-machines (go label)
  (let* ((merged-states (get-initial-value 'tagbody-merged-states))
	 (state-label (get-label-from-merged-states label merged-states))
	 (state-variable (get-initial-value 'tagbody-state-variable)))

    `(setq ,state-variable ,state-label)))
