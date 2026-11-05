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
    :reader synthetic-p)
   (jump-target-p
    :documentation "Flag indicating if this state is the target of an explicit jump."
    :initform nil
    :reader jump-target-p))
  (:documentation "Abstract state in a state machine."))


(defmethod initialize-instance :after ((s state) &key label in initial-state &allow-other-keys)
  (unless label
    ;; no label given, construct one and mark it as synthetic
    (setf (slot-value s 'label) (gensym))
    (setf (slot-value s 'synthetic-p) t))

  (when in
    ;; automatically add to a state machine, also marking
    ;; it as initial if INITIAL-STATE is non-NIL
    (add-state in s :initial-state initial-state)))


(defun append-to-body (state form)
  "Append FORM to the body of STATE."
  (appendf (body state) (list form)))


(defun falls-through-p (state)
  "Test whether STATE has a successor state.

States without successor states exit through explicit GO forms; those
with successors will have a GO appended to them."
  (not (null (successor-state state))))


(defun empty-state-p (state)
  "Test whether STATE has an empty body."
  (null (body state)))


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
    :initform nil)
   (jump-targets
    :documentation "A list of states that are jump targets.

States can't be marked as jump targets in the construction pass because
they might be forward references to as-yet-undeclared states.
MARK-LABEL-AS-JUMP-TARGET marks a state's label as a jump target,
and RESOLVE-JUMP-TARGETS martks the individual states (and detects
any unknown states)."
    :initform nil
    :reader jump-targets)
   (bypass-states-alist
    :documentation "An alist mapping removed states to their replacement states."
    :initform nil
    :reader bypass-states-alist))
  (:documentation "A state machine."))


(defun add-state (sm state &key initial-state)
  "Add STATE to machine SM.

The state is not automatically marked as initial even it it's the
first: the initial state needs to be set explicitly using
MARK-AS-INITIAL-STATE or by setting INITIAL-STATE to non-NIL."
  (appendf (slot-value sm 'states-alist) (list (list (label state) state)))

  (when initial-state
    (mark-as-initial-state sm state)))


(defun bypass-state (sm state &optional new-state)
  "Bypass STATE in machine SM.

If NEW-STATE is provided, all references to STATE will be replaced
by NEW-STATE. (NEW-STATE itself may also be bypassed.)"
  (appendf (slot-value sm 'bypass-states-alist)
	   (list (list (label state)
		       (if new-state
			   (label new-state))))))


(defun bypassed-states (sm)
  "Return the bypassed states of machine SM."
  (alist-keys (bypass-states-alist sm)))


(defun resolve-label (sm label)
  "Return the label associated with LABEL in machine SM.

This traverses the bypass table to find the actual label of a
the state being targeted."
  (if-let ((a (assoc label (bypass-states-alist sm))))
    (let ((new-label (cadr a)))
      (if (null new-label)
	  ;; we've deleted the state not simply bypassed it
	  (error 'unknown-state :label label)

	  (format t "Bypassed state ~a to ~a" label new-label)
	  (resolve-label sm new-label)))

    label))


(defun delete-state (sm state)
  "Delete STATE from machine SM.

If STATE is the initial state of SM, that is removed too."
  ;; (setf (slot-value sm 'states-alist)
  ;;	(remove-if (lambda (ls)
  ;;		     (eql (cadr ls) state))
  ;;		   (slot-value sm 'states-alist)))
  ;; (if (eql (initial-state sm) state)
  ;;     (setf (slot-value sm 'initial-state) nil))
  (bypass-state state))


(defun mark-as-initial-state (sm state)
  "Mark state STATE as the initial state of state machine SM."
  (unless (null (initial-state sm))
    (error "Re-setting initial state"))

  (setf (slot-value sm 'initial-state) state))


(defun mark-state-label-as-jump-target (sm label)
  "Mark the state with LABEL in machine SM as a jump target."
  (setf (slot-value sm 'jump-targets) (union (slot-value sm 'jump-targets) (list label))))


(defun state-machine-state-labelled (sm label)
  "Return the state of machine SM labelled LABEL.

An UNKNOWN-STATE error is signalled if a state isn't defined."
  (if-let ((a (assoc label (slot-value sm 'states-alist))))
    (cadr a)

    (error 'unknown-state :label label :hint "Make sure state is in scope")))


(defun resolve-jump-targets (sm)
  "Resolve jump targets in machine SM.

An UNKNOWN-STATE error is signalled if a target state isn't defined."
  (let ((states (state-machine-states sm)))
    (dolist (label (jump-targets sm))
      (let ((state (state-machine-state-labelled sm label)))
	(mark-as-jump-target state)))))


(defun state-machine-states (sm)
  "Return the states of machine SM."
  (let ((bps (bypassed-states sm)))
    (remove-if (lambda (state)
		 (member (label state) bps))
	       (delistify (alist-values (slot-value sm 'states-alist))))))


(defun state-machine-state-labels (sm)
  "Return the state labels of machine SM."
  (set-difference (alist-keys (slot-value sm 'states-alist))
		  (bypassed-states sm)))


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

    ;; continue in a new state
    ;; (let ((new-state (make-instance 'state :in sm)))
    ;;   (setf (successor-state current-state) new-state)
    ;;   (parse-tagbody-forms forms
    ;;			   sm new-state exit-state))

    ;; continue in te current state
    (parse-tagbody-forms forms
			 sm current-state exit-state)))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'tagbody)) args forms sm current-state exit-state)
  ;; nested machine, start a new state for this machine
  (let* ((trailing-state (or (if forms
				 (parse-tagbody-forms forms
						      smnil exit-state))
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

	   (then-state (or (parse-tagbody-forms (list then-branch)
						sm nil trailing-state)))

	   (else-state (if else-branch
			   (parse-tagbody-forms else-branch
						sm nil trailing-state))))

      ;; compile the conditional, currently without optimisations
      (let ((cform (if else-state
		       ;; two arms
		       (prog1
			   `(if ,condition
				(go ,(label then-state))
				(go ,(label else-state))))

		       ;; one arm, jump to the trailing state on false
		       (prog1
			   `(if ,condition
				(go ,(label then-state))
				(go ,(label trailing-state)))))))

	(let ((new-state (make-instance 'state :in sm)))
	  (when current-state
	    (setf (successor-state current-state) new-state))
	  (append-to-body new-state cform)

	  new-state)))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'case)) args forms sm current-state exit-state)
  (declare (optimize debug))

  (destructuring-bind (condition &rest cases)
      args

    (let* ((trailing-state (or (parse-tagbody-forms forms sm nil exit-state)
			       exit-state))
	   (arms (mapcar (lambda (form)
			   (destructuring-bind (val &rest arm-body)
			       form
			     (list val (parse-tagbody-forms arm-body sm nil trailing-state))))
			 cases))
	   (new-arms (mapcar (lambda (arm)
			       (destructuring-bind (val arm-state)
				   arm
				 (let ((arm-label (label arm-state)))
				   `(,val (go ,arm-label)))))
			     arms))
	   (cform `(case ,condition
		     ,new-arms)))

      (let ((new-state (make-instance 'state :in sm)))
	(when current-state
	  (setf (successor-state current-state) new-state))
	(append-to-body new-state cform)

	new-state))))


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
be built that run once and then stop. Use explicit GO forms, or
looping macros like FOREVER, to keep the machine running."
  (declare (optimize debug))

  (let* ((sm (make-instance 'state-machine))
	 (initial-state (make-instance 'state :in sm :initial-state t))
	 (final-state (make-instance 'state :in sm)))

    ;; parse with a final passivating state
    (parse-tagbody-forms forms sm initial-state final-state)

    ;; make the final state jump to the initial state
    (let ((initial-label (label initial-state)))
      (append-to-body final-state `(go ,initial-label))
      (mark-state-label-as-jump-target sm initial-label))

    ;; resolve the jump targets
    (resolve-jump-targets sm)

    ;; return the machine
    sm))


;;; ---------- State machine improvement ----------

(defun remove-simple-jump-states (sm)
  "Remove any states of machine SM that are just jump-throughs."
  (flet ((rewrite-jump-through-state (state)
	   (cond ((and (= (length (body state)) 1)
		       (eql (caar (body state)) 'go))
		  ;; body is just a GO form
		  (let* ((target-state-label (cadar (body state)))
			 (target-state (state-machine-state-labelled sm target-state-label)))
		    (bypass-state sm state target-state)))

		 ((and (null (body state))
		       (not (null (successor-state state))))
		  ;; body is empty, and there's a successor
		  (let ((target-state (successor-state state)))
		    (bypass-state sm state target-state))))))

    (let ((states (state-machine-states sm)))
      (mapc #'rewrite-jump-through-state states))))


;; TODO: Make this step optional
;; TODO: Allow non-removal of explicitly-named states, even if they're redundant,
;; so that we don't affect timing where it's critical

(defun improve-state-machine (sm)
  "Improve the machine SM.

This attempts to minimise the numbr of states in the machine and reduce
the number of jumps. The resulting machine is neither provably minimal nor
the provably optimal, hence the function name. It should however behave
exactly as the original apart from timing behaviour."
  ;; remove empty states
  (remove-simple-jump-states sm)

  ;; return the (hopefully) improved machine
  sm)


;;; ---------- State machine synthesis ----------

(defun synthesise-state-machine (sm)
  "Return the code for machine SM"
  (declare (optimize debug))

  ;; link all states to their successors where necessary
  (dolist (state (state-machine-fall-through-states sm))
    (append-to-body state `(go ,(label (successor-state state)))))

  (let* ((state-labels (state-machine-state-labels sm))
	 (states (state-machine-states sm))

	 ;; TODO: This may need to be more global, for example using symbols
	 ;; that get compiled to integers?
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

    ;; synthesise the machine
    (with-gensyms (state-variable)
      ;; we put the state machine synthesis into its own frame so that
      ;; it can declare the state variable and state machine variables
      ;; for use in its own further transformation.
      (add-frames `(let ((tagbody-state-machine ,sm)
			 (tagbody0state-variable ,state-variable))

		     ;; labels
		     (let ,label-decls
		       ,declaration

		       ;; state variable
		       (let ((,state-variable ,(car state-labels)))
			 (declare (type (unsigned-byte ,(bits-for-integer (length state-labels))) ,state-variable)
				  (as register ,state-variable)
				  (synthetic ,state-variable)
				  (assigned-at-declaration ,state-variable))

			 (case ,state-variable
			   ,@clauses))))))))


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
      (float-let-blocks (with-implicit-tagbody body))

    (destructuring-bind (newfun &rest newargs)
	newbody

      (unless (eql newfun 'tagbody)
	(error "Incorrectly floated LET blocks in TAGBODY"))

      ;; we hold on to the NEWENV frame because it contains all the information
      ;; we've already extracted about the variables -- and these were the only
      ;; ones in scope when the code was analysed, with others being created
      ;; by SYNTHESISE-STATE-MACHINE, which adds the necessary types and
      ;; representations directly

      (let* ((sm (build-state-machine newargs)))
	(improve-state-machine sm)

	(let* ((synth (synthesise-state-machine sm))
	       (p (if newenv
		      ;; float the locally-declared variables around the state machine
		      `(let ,newenv
			 ,synth)

		      ;; no locally-declared variables in body
		      synth))))

	(elaborate-state-machines p)))))


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
  (let* ((state-variable (get-initial-value 'tagbody-state-variable))
	 (sm (get-initial-value 'tagbody-state-machine))
	 (real-label (resolve-label sm label)))
    `(setq ,state-variable ,real-label)))
