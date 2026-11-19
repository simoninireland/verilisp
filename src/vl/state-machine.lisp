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
    :documentation "The forms in the body of the state.

The body is an implicit PROGN, and is essentially a basic block that can be
run without interfering with other basic blocks."
    :initarg :body
    :initform nil
    :accessor body)
   (successor
    :documentation "The state this state will fall-through to (if it does)."
    :initform nil
    :accessor successor-state)
   (synthetic-p
    :documentation "Flag indicating that the state is synthetic.

A synthetic state is one that's been created by the compiler as part of
state machine elaboration."
    :initform nil
    :reader synthetic-p)
   (jump-target-p
    :documentation "Flag indicating if this state is the target of an explicit jump.

Jump targets are handled differently during elaboration. In particular, they aren't
coalesced into other states so that the jump remains viable."
    :initform nil
    :reader jump-target-p))
  (:documentation "Abstract state in a state machine.

A state is essentially some code that runs in a single clock tick. The states
are threaded together to build the timed logic of the overall machine. The
elaboration of a machine can result in states being coalesced and/or deleted
from the machine."))


(defmethod initialize-instance :after ((s state) &key label in initial-state &allow-other-keys)
  (unless label
    ;; no label given, construct one and mark it as synthetic
    (setf (slot-value s 'label) (gensym))
    (setf (slot-value s 'synthetic-p) t))

  (when in
    ;; automatically add to a state machine, also marking
    ;; it as initial if INITIAL-STATE is non-NIL
    (add-state in s :initial-state initial-state)))


(defun empty-state-p (state)
  "Test whether STATE has an empty body."
  (null (body state)))


(defun append-to-body (state form)
  "Append FORM to the body of STATE."
  (appendf (body state) (list form)))


(defun falls-through-p (state)
  "Test whether STATE has a successor state.

States without successor states exit through explicit GO forms; those
with successors will have a GO appended to them."
  (not (null (successor-state state))))


(defun mark-as-jump-target (state)
  "Mark STATE as an explicit jump target."
  (setf (slot-value state 'jump-target-p) t))


(defclass state-machine ()
  ((initial-state
    :documentation "The initial state of the machine."
    :initform nil
    :reader initial-state)
   (states-alist
    :documentation "A mapping of labels to states."
    :initform nil
    :reader states-alist)
   (jump-targets
    :documentation "A list of states that are jump targets.

States can't be marked as jump targets in the construction pass
because they might be forward references to as-yet-undeclared states.

MARK-LABEL-AS-JUMP-TARGET marks a state's label as a jump target.

RESOLVE-JUMP-TARGETS marks the individual states by calling
MARK-AS-JUMP-TARGET for each marked state (and also detects any
unknown states)."
    :initform nil
    :reader jump-targets)
   (bypassed-states-alist
    :documentation "An alist mapping removed states to their replacement states.

A bypassed state is one that's been either deleted or collapsed into
another one.

STATE-MACHINE-STATES and related functions work over the un-bypassed
states, so to all intents and purposes the bypassed states have been
deleted. RESOLVE-LABEL recurses through the bypass table to return the
label of the \"real\" state remaining after bypassing."
    :initform nil
    :reader bypassed-states-alist))
  (:documentation "A state machine."))


(defun add-state (sm state &key initial-state)
  "Add STATE to machine SM.

The state is not automatically marked as initial even it it's the
first: the initial state needs to be set explicitly using
MARK-AS-INITIAL-STATE."
  (appendf (slot-value sm 'states-alist) (list (list (label state) state)))

  (when initial-state
    (mark-as-initial-state sm state)))


(defun bypass-state (sm state &optional new-state)
  "Bypass STATE in machine SM.

If NEW-STATE is provided, all references to STATE will be replaced
by NEW-STATE. (NEW-STATE itself may also be bypassed.)

If NEW-STATE is omitted then STATE is marked as deleted."
  (let ((new-label (bypassed-to sm (label state))))
    (when (and new-label
	       new-state
	       (not (eql new-label (label new-state))))
      (error 'compiler-error :hint (format nil "Trying to bypass an already-bypassed state ~a"
					   (label state))))

    ;; if the state is a jump target, so is the new state
    (when (and (jump-target-p state)
	       new-state)
      (mark-as-jump-target new-state)
      (mark-state-label-as-jump-target sm (label new-state)))

    ;; append the bypassed state to the bypass list
    (appendf (slot-value sm 'bypassed-states-alist)
	     (list (list (label state)
			 (or (and new-state
				  (label new-state))
			     'deleted))))))


(defun bypassed-state-labels (sm)
  "Return the labels of all bypassed states in SM."
  (alist-keys (bypassed-states-alist sm)))


(defun bypassed-states (sm)
  "Return the bypassed states of machine SM."
  (mapcar (curry #'state-machine-state-labelled sm) (bypassed-state-labels sm)))


(defun bypassed-state-p (sm state)
  "Test whether STATE is bypassed in machine SM."
  (not (null (assoc (label state) (bypassed-states-alist sm)))))


(defun bypassed-to (sm label)
  "Return the state that LABEL is bypassed to.

This only checks the immediate bypass value: to resolve the label
fully, use RESOLVE-LABEL.

Return NIL if LABEL is not bypassed."
  (if-let ((a (assoc label (bypassed-states-alist sm))))
    ;; state is bypassed, return the label it's bypassed to
    (cadr a)))


(defun resolve-label (sm label)
  "Return the label associated with LABEL in machine SM.

This traverses the bypass table to find the actual label of a
the state being targeted."
  (if-let ((new-label (bypassed-to sm label)))
    ;; has the state beein deleted?
    (if (eql new-label 'deleted)
	(error 'compiler-error :hint (format nil "State ~a has been deleted" label))

	;; label is bypassed, recurse to check that label
	(progn
	  (log:trace "Resolved ~a to ~a" label new-label)
	  (resolve-label sm new-label)))

    ;; label isn't bypassed, return it
    label))


(defun mark-as-initial-state (sm state)
  "Mark state STATE as the initial state of state machine SM."
  (setf (slot-value sm 'initial-state) state))


(defun mark-state-label-as-jump-target (sm label)
  "Mark the state with LABEL in machine SM as a jump target."
  (setf (slot-value sm 'jump-targets)
	(union (slot-value sm 'jump-targets)
	       (list label))))


(defun state-machine-state-labelled (sm label)
  "Return the state of machine SM labelled LABEL.

An UNKNOWN-STATE error is signalled if a state isn't defined."
  (if-let ((a (assoc (resolve-label sm label) (slot-value sm 'states-alist))))
    (cadr a)

    (error 'unknown-state :label label :hint "Make sure state is in scope")))


(defun resolve-jump-targets (sm)
  "Resolve jump targets in machine SM.

An UNKNOWN-STATE error is signalled if a target state isn't defined."
  (declare (optimize debug))

  (let ((states (state-machine-states sm)))
    (dolist (label (jump-targets sm))
      (let ((state (state-machine-state-labelled sm label)))
	(mark-as-jump-target state)))))


;; The following functions all treat bypassed states as though they've been deleted
;; They do not resolve labels

(defun state-machine-states (sm)
  "Return the states of machine SM.

Only the non-bypassed states are returned."
  (mapcar (curry #'state-machine-state-labelled sm)
	  (state-machine-state-labels sm)))


(defun state-machine-state-labels (sm)
  "Return the state labels of machine SM.

Only the labels of non-bypassed states are returned. The labels are
presented in the same order as the states returned by
STATE-MACHINE-STATES."
  (set-difference (alist-keys (states-alist sm))
		  (alist-keys (bypassed-states-alist sm))))


(defun state-machine-jump-target-states (sm)
  "Return the states of machine SM that are explicit jump targets."
  (filter (state-machine-states sm) #'jump-target-p))


(defun state-machine-synthetic-states (sm)
  "Return the states of machine SM that are synthetic."
  (filter (state-machine-states sm) #'synthetic-p))


(defun state-machine-fall-through-states (sm)
  "Return the states of machine SM that fall-through."
  (filter (state-machine-states sm) #'falls-through-p))


(defun state-machine-fall-through-target-states (sm)
  "Return the states in machine SM that are fall-through targets."
  (let ((fts (state-machine-fall-through-states sm)))
    (union-all (mapcar (compose #'list #'successor-state) fts))))


(defun state-machine-unused-states (sm)
  "Return the states of machine SM that are unused.

A state is unused if it is neither a successor to another state,
nor a jump target, nor initial."
  (let ((fts (state-machine-fall-through-target-states sm))
	(jts (state-machine-jump-target-states sm))
	(initial-state (initial-state sm)))
    (remove-if (lambda (state)
		 (or (member state fts)
		     (member state jts)
		     (= state initial-state)))
	       (state-machine-states sm))))


;;; ---------- State machine construction ----------

;;; We elaborate TAGBODY-style state machines in CASE-style machines that
;;; can be directly synthesised. This involes a lot of processing, to identify
;;; states and the appropriate transitions.
;;;
;;; States may also be synthesised if required to ensure dataflow consistency.
;;;
;;; TODO: We'll need to transform any TAGBODY forms in argument
;;; positions before doing this, otrherwise we miss weird uses. That
;;; should be part of a wider transformation that makes all arguments
;;; simple expressions.

;;; TODO: Rewrite this function as a pass (will require more machinery in the DSL)

(defgeneric parse-tagbody-forms-sexp (fun args forms sm current-state exit-state)
  (:documentation "Parse FUN applied to ARGS into state machine SM.

Methods on this function should construct a state machine for
FUN applied to ARGS, integrate it into CURRENT-STATE, and then
proceed to parse FORMS (typically by calling PARSE-TAGBODY-FORMS).

EXIT-STATE is the 'fall-through' state that the form should transition
to when it exits.

Return the entry state created.")
  (:method (fun args forms sm current-state exit-state)
    ;; "normal" form, add to body of current state
    (appendf (body current-state) (list (cons fun args)))

    ;; continue in current state
    (parse-tagbody-forms forms
			 sm current-state exit-state)))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'tagbody)) args forms sm current-state exit-state)
  ;; nested machine, start a new state for this machine
  (let* ((trailing-state (or (if forms
				 (parse-tagbody-forms forms
						      sm nil exit-state))
			     exit-state))
	 (body-state (parse-tagbody-forms args
					  sm current-state trailing-state)))

    body-state))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'progn)) args forms sm current-state exit-state)
  (let* ((trailing-state (or (if forms
				 (parse-tagbody-forms forms
						      sm nil exit-state))
			     exit-state))
	 (body-state (parse-tagbody-forms args
					  sm current-state trailing-state)))
    body-state))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'if)) args forms sm current-state exit-state)
  (declare (optimize debug))

  (destructuring-bind (condition then-branch &rest else-branch)
      args

    (let* ((trailing-state (or (parse-tagbody-forms forms
						     sm nil exit-state)
				exit-state))

	   (then-state (parse-tagbody-forms (list then-branch)
					    sm nil trailing-state))

	   (else-state (if else-branch
			   (parse-tagbody-forms else-branch
						sm nil trailing-state))))

      ;; compile the conditional
      (when then-state
	(mark-state-label-as-jump-target sm (label then-state)))
      (let ((cform (if else-state
		       ;; two arms
		       `(if ,condition
			    (go ,(label then-state))
			    (go ,(label else-state)))

		       ;; one arm, jump to the trailing state on false
		       `(if ,condition
			    (go ,(label then-state))
			    (go ,(label trailing-state))))))

	(if (and current-state
		 (empty-state-p current-state))
	    ;; current state is empty, use it
	    (progn
	      (unless else-state
		(setf (successor-state current-state) trailing-state))
	      (append-to-body current-state cform)
	      current-state)

	    ;; current state has content, create a new state for the IF form
	    (let ((new-state (make-instance 'state :in sm)))
	      (when current-state
		(setf (successor-state current-state) new-state))

	      (unless else-state
		(setf (successor-state new-state) trailing-state))
	      (append-to-body new-state cform)
	      new-state))))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'case)) args forms sm current-state exit-state)
  (declare (optimize debug))

  (destructuring-bind (condition &rest cases)
      args

    (let* ((trailing-state (or (parse-tagbody-forms forms sm nil exit-state)
			       exit-state))
	   (arms (mapcar (lambda (form)
			   (declare (optimize debug))
			   (destructuring-bind (val &rest arm-body)
			       form

			     (let ((s (parse-tagbody-forms (list (with-implicit-progn
								   arm-body))
							   sm nil trailing-state)))
			       (list val s))))
			 cases))
	   (new-arms (mapcar (lambda (arm)
			       (destructuring-bind (val arm-state)
				   arm
				 (let ((arm-label (label arm-state)))
				   `(,val (go ,arm-label)))))
			     arms))
	   (cform `(case ,condition
		     ,@new-arms)))

      ;; mark all the arm states as jump targets
      ;; (mapc (lambda (arm)
      ;;	      (destructuring-bind (val arm-state)
      ;;		  arm
      ;;		(declare (ignore val))

      ;;		(mark-state-label-as-jump-target sm (label arm-state))))
      ;;	    arms)

      (if (and current-state
	       (empty-state-p current-state))
	  ;; current state is empty, use it
	  (progn
	    (append-to-body current-state cform)
	    (setf (successor-state current-state) trailing-state)
	    current-state)

	  ;; current state has content, create a new state for the CASE form
	  (let ((new-state (make-instance 'state :in sm)))
	    (when current-state
	      (setf (successor-state current-state) new-state))

	    (append-to-body new-state cform)
	    (setf (successor-state new-state) trailing-state)
	    new-state)))))


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


(defmethod parse-tagbody-forms-sexp ((fun (eql 'go)) args forms sm current-state exit-state)
  (let ((label (car args)))
    ;; add form to current state
    (append-to-body current-state (cons fun args))
    (mark-state-label-as-jump-target sm label)

    ;; current state is ended and doesn't fall-through
    (setf (successor-state current-state) nil)

    ;; skip any unreachable code on this path, and continue parsing from there
    (if-let ((newforms (chew-unreachable-code current-state forms)))
      (parse-tagbody-forms newforms
			   sm nil exit-state))))


(defun parse-tagbody-forms (forms sm current-state exit-state)
  "Parse FORMS as the body of a TAGBODY into state machine SM.

The FORMS are built into CURRENT-STATE until there is a state
change. If a state falls-through, it lands in EXIT-STATE.

Return the entry state created"
  (declare (optimize debug))

  (if (null forms)
      ;; no more forms to process
      (progn
	(if current-state
	    (progn
	      (when exit-state
		;; we have a current state
		(setf (successor-state current-state) exit-state)))

	    (when exit-state
	      ;; we don't have a current state, create one
	      (let ((jump-state (make-instance 'state :in sm)))
		(setf (successor-state jump-state) exit-state)
		(setq current-state jump-state)))))

      ;; forms to do
      (destructuring-bind (form &rest rest)
	  forms

	(with-current-form form
	  (if (state-label-p form)
	      ;; new state marker
	      (let ((new-state (make-instance 'state :label form :in sm)))
		(when current-state
		  ;; link current state to new state
		  (setf (successor-state current-state) new-state))

		;; use this new state as the current state going forward
		(setq current-state new-state)
		(parse-tagbody-forms rest sm new-state exit-state))

	      ;; executable form
	      (progn
		(unless current-state
		  ;; no curent state, create one to hold the form
		  (setf current-state (make-instance 'state :in sm)))

		(if (listp form)
		    ;; handle sexp
		    (destructuring-bind (fun &rest args)
			form
		      (parse-tagbody-forms-sexp fun args
						rest
						sm current-state exit-state))

		    (progn
		      ;; singleton form that isn't a state marker
		      (append-to-body current-state form)
		      (parse-tagbody-forms rest sm current-state exit-state))))))

	current-state)))


(defun build-state-machine (forms)
  "Parse FORMS as the body of a TAGBODY, building a state machine.

The resulting machine is necessarily 'top-level', not contained in
another machine. It has a passivating state added to the end, which it
will remain in if it ever gets to that point. This allows machines to
be built that run once and then stop, and makes the semantics of
machines-within-machiens cleaner. Use explicit GO forms, or looping
macros like FOREVER, to keep the machine running."
  (declare (optimize debug))

  (let* ((sm (make-instance 'state-machine))
	 (initial-state (make-instance 'state :in sm :initial-state t))
	 (final-state (make-instance 'state :in sm)))

    ;; parse with a final passivating state
    (parse-tagbody-forms forms sm initial-state final-state)

    ;; make the final state jump to itself
    (let ((final-label (label final-state)))
      (append-to-body final-state `(go ,final-label)))

    ;; resolve the jump targets
    (resolve-jump-targets sm)

    ;; return the machine
    sm))


;;; ---------- State machine improvement ----------

(defun state-from-go (sm form)
  "Extract the state of machine SM corresponding to the jump in FORM.

FORM should be a GO form."
  (let ((label (cadr form)))
    (state-machine-state-labelled sm label)))


(defun remove-simple-jump-states (sm)
  "Remove any states of machine SM that are empty or jump-throughs."
  (labels ((jump-state-p (state)
	     ;; state just contains a jump to somewhere else
	     (and (= (length (body state)) 1)
		  (eql (caar (body state)) 'go)
		  (not (eql (jump-state-target-state state) state))))

	   (jump-state-target-state (state)
	     ;; return the target of the jump of a jump state
	     (state-from-go sm (car (body state))))

	   (empty-state-with-successor-p (state)
	     ;; state is empty with a successor
	     (and (null (body state))
		  (not (null (successor-state state)))))

	   (rewrite-jump-through-state (state)
	     (declare (optimize debug))

	     (cond ((jump-state-p state)
		    ;; body is just a GO form
		    (let ((target-state (jump-state-target-state state)))
		      (log:debug "Bypassed jump state ~a to jump directly to ~a"
				 (label state)
				 (label target-state))
		      (bypass-state sm state target-state)))

		   ((empty-state-with-successor-p state)
		    ;; body is empty, and there's a successor
		    (let ((target-state (successor-state state)))
		      (log:debug "Bypassed empty state ~a to successor ~a"
				 (label state)
				 (label target-state))

		      ;; successor is now a jump target if the state itself is
		      (when (jump-target-p state)
			(mark-state-label-as-jump-target sm (label target-state))
			(mark-as-jump-target target-state))

		      (bypass-state sm state target-state))))))

    (log:debug "Removing empty and jump-through states")
    (dolist (state (state-machine-states sm))
      (rewrite-jump-through-state state))))


(defun collapse-simple-if-states (sm)
  "Collapse any states in machine SM that are simple enough to fold into the branches of an IF."
  (labels ((rewrite-simple-if-branch (state)
	     (if (or (synthetic-p state)
		     (not (jump-target-p state)))
		 ;; return the state's body as a single form, wrapped
		 ;; in a PROGN if needed
		 (with-implicit-progn
		     (body state))))

	   (rewrite-simple-if-state (state)
	     (declare (optimize debug))

	     (let ((body (body state)))
	       (when (and (= (length body) 1)
			  (eql (caar body) 'if))

		 (let ((if-form (car body)))
		   (let* ((true-branch (state-from-go sm (elt if-form 2)))
			  (false-branch (if (= (length if-form) 4)
					    (state-from-go sm (elt if-form 3))))
			  (same-target-p (and false-branch
					      (not (null (successor-state true-branch)))
					      (eql (successor-state true-branch)
						   (successor-state false-branch)))))

		     (if-let ((new-branch (rewrite-simple-if-branch true-branch)))
		       ;; branch can be collapsed
		       (progn
			 (log:debug "Collapsed then branch ~a (~a) into conditional ~a"
				    (body true-branch)
				    (label true-branch)
				    (label state))

			 ;; rewrite the IF to include the branch's body as the then form
			 (setf (elt if-form 2) new-branch)

			 ;; bypass the state
			 (bypass-state sm true-branch))

		       ;; otherwise retain the true branch and mark it as a jump target-state
		       (progn
			 (mark-state-label-as-jump-target sm (label true-branch))
			 (mark-as-jump-target true-branch)))

		     (when false-branch
		       (if-let ((new-branch (rewrite-simple-if-branch false-branch)))
			 ;; branch can be collapsed
			 (progn
			   (log:debug "Collapsed else branch ~a (~a) into conditional ~a"
				      (body false-branch)
				      (label false-branch)
				      (label state))

			   ;; rewrite the IF to include the branch's body as the else form
			   (setf (elt if-form 3) new-branch)

			   ;; bypass the state
			   (bypass-state sm false-branch))

			 ;; otherwise retain the else branch and mark it as a jump target-state
			 (progn
			   (mark-state-label-as-jump-target sm (label false-branch))
			   (mark-as-jump-target false-branch))))

		     (if same-target-p
			 (progn
			   (log:debug "Set successor state to ~a in conditional ~a"
				      (label true-branch)
				      (label state))
			   (setf (successor-state state)
				 (successor-state true-branch)))

			 (progn
			   (log:debug "Cleared successor state of conditional ~a (~a)"
				      (body state)
				      (label state))
			   (setf (successor-state state) nil)))))))))

    (log:debug "Collapsing IF arms")
    (let ((states (state-machine-states sm)))
      (mapc (lambda (state)
	      (rewrite-simple-if-state state))
	    states))))


(defun collapse-simple-case-states (sm)
  "Collapse any states in machine SM that are simple enough to fold into the branches of a CASE."
  (labels ((rewrite-simple-case-branch (state)
	     (if (not (jump-target-p state))
		 (body state)))

	   (rewrite-simple-case-state (state)
	     (declare (optimize debug))

	     (let ((body (body state)))
	       (if (and (= (length body) 1)
			(eql (caar body) 'case))
		   (let ((case-form (car body)))

		     (destructuring-bind (fun condition &rest clauses)
			 case-form
		       (declare (ignore fun condition))

		       (let* ((state-clauses (mapcar (lambda (clause)
						       (destructuring-bind (val body)
							   clause
							 (list val (state-from-go sm body))))
						     clauses))
			      (new-clauses (mapcar (lambda (clause)
						     (destructuring-bind (val branch)
							 clause
						       (if-let ((new-branch (rewrite-simple-case-branch branch)))
							 (progn
							   (bypass-state sm branch)
							   (cons val new-branch))

							 (list val `(go ,(label branch))))))
						   state-clauses))
			      (possible-target (successor-state (cadr (car state-clauses))))
			      (same-target-p (every (lambda (s)
						      (eql (successor-state (cadr s))
							   possible-target))
						    (cdr state-clauses))))

			 (when same-target-p
			   ;; drop-through to the same trailing state
			   (setf (successor-state state) possible-target))

			 ;; update the CASE for with the new clauses
			 (setf (cddr case-form) new-clauses))))))))

    (log:debug "Collapsing CASE arms")
    (let ((states (state-machine-states sm)))
      (mapc (lambda (state)
	      (rewrite-simple-case-state state))
	    states))))


(defun collapse-successor-states (sm)
  "Collapse any successor states that can be combined."
  (labels ((collapsable-p (state)
	     ;; state falls-through to a non-jump-target state
	     (and (falls-through-p state)
		  (not (jump-target-p (successor-state state)))))

	   (rewrite-successor-state (state)
	     (when (collapsable-p state)
	       (let ((fallthrough (successor-state state)))
		 (log:debug "Collapsing successor state ~a (~a) into state ~a"
			    (body fallthrough)
			    (label fallthrough)
			    (label state))
		 ;; append the whole body of the successor state
		 ;; (append-to-body state (with-implicit-progn
		 ;;			   (body fallthrough)))
		 (mapc (curry #'append-to-body state) (body fallthrough))

		 ;; set our successor to be the successor's successor (if it has one)
		 (setf (successor-state state) (successor-state fallthrough))

		 ;; bypass (delete) the now-redundant successor
		 (bypass-state sm fallthrough)))))

    (log:debug "Collapsing successor states")
    (dolist (state (state-machine-states sm))
      (rewrite-successor-state state))))


(defun remove-unreachable-synthetic-states (sm)
  "Remove unreachable synthetic states from SM.

This is basically intended to remove a passivating state that's never reached."
  (labels ((unreachable-p (state)
	     (and (synthetic-p state)
		  (not (jump-target-p state))
		  (= (length (body state)) 1)
		  (eql (caar (body state)) 'go)
		  (eql (cadar (body state)) (label state))))

	   (remove-unreachable-state (state)
	     (when (unreachable-p state)
	       (log:debug "Deleting state ~a" (label state))

	       (bypass-state sm state))))

    (log:debug "Removing unreachable synthetic states")
    (let ((states (state-machine-states sm)))
      (mapc (lambda (state)
	      (remove-unreachable-state state))
	    states))))

;; TODO: Remove self-assignments for states

;; TODO: Make this step optional

;; TODO: Allow non-removal of explicitly-named states, even if they're redundant,
;; so that we don't affect timing where it's critical

;; TODO: Maybe also have a mode that doesn't introduce states and just uses
;; the explicit ones in the source code, for timing-sensitive bits?

(defun improve-state-machine (sm)
  "Improve the machine SM.

This attempts to minimise the numbr of states in the machine and
reduce the number of jumps. The resulting machine is neither provably
minimal nor provably optimal, hence the function name. It should
however behave exactly as the original apart from having improved
timing behaviour."
  ;; remove empty and jump-through states
  (remove-simple-jump-states sm)

  ;; collapse successors
  (collapse-successor-states sm)

  ;; simplify IF and CASE forms
  (collapse-simple-if-states sm)
  ;(collapse-simple-case-states sm)

  ;; remove any unreachable synthetic states
  ;(remove-unreachable-synthetic-states sm)

  ;; return the (hopefully) improved machine
  sm)


;;; ---------- State machine synthesis ----------

(defun synthesise-state-machine (sm)
  "Generate the code for machine SM.

Return the name of the state variable and the body of the constructed machine."
  (declare (optimize debug))

  (let* ((states (state-machine-states sm))
	 (state-labels (state-machine-state-labels sm))
	 (resolved-labels (mapcar (curry #'resolve-label sm) state-labels))
	 (initial-state-label (resolve-label sm (label (initial-state sm)))))

    (if (= (length states) 1)
	;; this is a combinatorial block, so turn it into a PROGN with
	;; no state variable
	(list nil
	      `(progn
		 ,@(body (car states))))

	;; otherwise it's a full machine

	;; TODO: This may need to be more global, for example using symbols
	;; that get compiled to integers?

	(progn
	  ;; link all states to their successors where necessary
	  (dolist (fallthrough (state-machine-fall-through-states sm))
	    (let ((successor-label (resolve-label sm (label (successor-state fallthrough)))))
	      (append-to-body fallthrough `(go ,successor-label))))

	  ;; construct the machine
	  (let* ((label-decls (mapcar (lambda (label index)
					`(,label ,index))
				      resolved-labels
				      (iota (length resolved-labels))))
		 (declaration `(declare (as constant ,@resolved-labels)
					(ignorable ,@resolved-labels)))
		 (clauses (mapcar (lambda (label state)
				    `(,label
				      ,@(body state)))
				  resolved-labels
				  states)))

	    (with-gensyms (state-variable)
	      (let ((q (add-frames `(let ,label-decls
				      ,declaration

				      ;; state variable
				      (let ((,state-variable ,initial-state-label))
					(declare (type (unsigned-byte ,(bits-for-integer (length state-labels))) ,state-variable)
						 (as register ,state-variable)
						 (synthetic ,state-variable)
						 (assigned-at-declaration ,state-variable))

					(case ,state-variable
					  ,@clauses))))))

		;; return the state variable and the machine over it
		(list state-variable q))))))))


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


(defpassmethod elaborate-state-machines (tagbody &rest body)
  (declare (optimize debug))

  (destructuring-bind (newbody newenv)
      (float-let-blocks (with-implicit-tagbody
			  body))

    (destructuring-bind (newfun &rest newargs)
	newbody

      (unless (eql newfun 'tagbody)
	(error 'compiler-error "Incorrectly floated LET blocks in TAGBODY"))

      ;; we hold on to the NEWENV frame because it contains all the
      ;; information we've already extracted about the variables --
      ;; and these were the only ones in scope when the code was
      ;; analysed, with others being created by
      ;; SYNTHESISE-STATE-MACHINE, which adds the necessary frames,
      ;; types, and representations directly

      (log:debug "State machine has ~a states as written"
		 (length (extract-states newargs)))

      (let ((sm (build-state-machine newargs)))
	(log:debug "State machine initially expands to ~a states"
		   (length (state-machine-states sm)))
	(improve-state-machine sm)
	(log:debug "Improving the state machine leaves ~a states "
		   (length (state-machine-states sm)))

	(destructuring-bind (state-variable synth)
	    (synthesise-state-machine sm)

	  (if (null state-variable)
	      ;; state machine has collapsed to a single state, return it
	      (progn
		(log:debug "State machine has collapsed to a combinatorial block")

		;; elaborate to get rid of any GO forms (which will be
		;; optimised away because there's no state machine)
		(elaborate-state-machines synth))

	      ;; wrap a new environment around the synthesised code
	      ;; to hold the state and state machine variables
	      (let ((p (add-frames `(let ((tagbody-state-machine ,sm)
					  (tagbody-state-variable ,state-variable))
				      ,synth))))

		;; elaborate the GO forms in the state machine
		(let ((q (elaborate-state-machines p)))

		  ;; remove the outermost environment (containing the
		  ;; state and state machine variables) that shouldn't
		  ;; be propagated to later passes and wrap the variable
		  ;; environment around the now-elaborated body
		  (if (and newenv
			   (not (null (get-frame-names newenv))))
		      ;; wrap the floated variables around the state machine
		      `(let ,newenv
			 ,@(cddr q))

		      ;; no locally-declared variables in body
		      (with-implicit-progn
			  (cddr q)))))))))))


;;; ---------- GO ----------

(defpassmethod compute-type (go label)
  ;; ensure label is in scope
  (unless (variable-declared-p label)
    (error 'unknown-state :label label :hint "Ensure the state label is in scope"))

  ;; GO doesn't really have a type
  t)


(defpassmethod compute-dependencies (go label)
  nil)


(defpassmethod read-variables (go label)
  '())


(defpassmethod elaborate-state-machines (go label)
  (if-let ((state-variable (get-initial-value 'tagbody-state-variable :default nil))
	   (sm (get-initial-value 'tagbody-state-machine)))
    ;; we have a state machine
    (let ((real-label (resolve-label sm label)))
      `(setq ,state-variable ,real-label))

    ;; the state machine has been optimised away, remove the GO
    (log:debug "Elided GO to state ~a" label)))
