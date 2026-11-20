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

;; Base state

(defclass state ()
  ((label
    :documentation "The state label."
    :initarg :label
    :initform nil
    :reader label)
   (successor-state-label
    :documentation "The label of the state this state will fall-through to (if it does)."
    :initarg :successor
    :initform nil
    :accessor successor-state-label)
   (synthetic-p
    :documentation "Flag indicating that the state is synthetic.

A synthetic state is one that's been created by the compiler as part of
state machine elaboration."
    :initform nil
    :reader synthetic-p))
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


(defgeneric body (state)
  (:documentation "Generate the code for STATE.

All the special forms provide a BODY method to construct code for
the state. This means that machines can be constructed as built,
and the improvements do exactly that: render a machine more
efficient.

Return a list of forms.")
  (:method (state)
    (error 'compiler-error :hint "Unexpected state type")))



;; Special forms

(defclass progn-state (state)
  ((progn-body
    :documentation "The forms in the body of the state.

The body is an implicit PROGN, and is essentially a basic block that can be
run without interfering with other basic blocks."
    :initarg :body
    :initform nil
    :accessor progn-body))
  (:documentation "A basic block, executed like a PROGN."))


(defmethod body ((state progn-state))
  (let ((c (progn-body state)))
    `(,@c
      ,@(if-let ((ssl (successor-state-label state)))
	(list `(go ,ssl))))))


(defun empty-state-p (state)
  "Test whether STATE has an empty body."
  (null (progn-body state)))


(defun append-to-body (state form)
  "Append FORM to the body of STATE."
  (appendf (progn-body state) (list form)))


(defclass if-state (state)
  ((condition
    :documentation "The condition expression."
    :initarg :condition
    :reader if-condition)
   (then-state-label
    :documentation "The label of the \"then\" state chain."
    :initarg :then-state-label
    :accessor then-state-label)
   (else-state-label
    :documentation "The label of the \"else\" state chain."
    :initarg :else-state-label
    :initform nil
    :accessor else-state-label))
  (:documentation "A two-armed conditional."))


(defmethod body ((state if-state))
  `((if ,(if-condition state)
	(go ,(then-state-label state))
	,@(if-let ((esl (else-state-label state)))
	    (list `(go ,esl))))
    ,@(if-let ((ssl (successor-state-label state)))
	(list `(go ,ssl)))))


(defclass case-state (state)
  ((condition
    :documentation "The condition expression."
    :initarg :condition
    :reader case-condition)
   (clauses
    :documentation "The list of  clauses.

Each clause is a list consisting of a value and a state label."
    :initarg :clauses
    :accessor clauses))
  (:documentation "A multi-armed guarded conditional."))


(defmethod body ((state case-state))
  (let ((arms (mapcar (lambda (clause)
			(destructuring-bind (val label)
			    clause
			  `(,val (go ,label))))
		      (clauses state))))
    `((case ,(case-condition state)
	 ,@arms)
      ,@(if-let ((ssl (successor-state-label state)))
	  (list `(go ,ssl))))))


(defclass go-state (state)
  ((jump-target-state-label
    :documentation "The label of the state being jumped to."
    :initarg :jump-target-state-label
    :accessor jump-target-state-label))
  (:documentation "An explicit jump."))


(defmethod body ((state go-state))
  `((go ,(jump-target-state-label state))))


;;; ---------- State machine representation ----------

(defclass state-machine ()
  ((initial-state-label
    :documentation "The label of the initial state of the machine."
    :initform nil
    :accessor initial-state-label)
   (states-alist
    :documentation "A mapping of labels to states."
    :initform nil
    :accessor states-alist))
  (:documentation "A state machine."))


(defun add-state (sm state &key initial-state)
  "Add STATE to machine SM.

The state is not automatically marked as initial even it it's the
first: the initial state needs to be set explicitly using
MARK-AS-INITIAL-STATE."
  (appendf (slot-value sm 'states-alist) (list (list (label state) state)))

  (when initial-state
    (mark-as-initial-state sm state)))


(defun mark-as-initial-state (sm state)
  "Mark state STATE as the initial state of state machine SM."
  (setf (initial-state-label sm) (label state)))


(defun state-machine-states (sm)
  "Return the states of machine SM."
  (mapcar (curry #'state-machine-state-labelled sm)
	  (state-machine-state-labels sm)))


(defun state-machine-state-labels (sm)
  "Return the state labels of machine SM."
  (alist-keys (states-alist sm)))


(defun state-machine-state-labelled (sm label)
  "Return the state of machine SM labelled LABEL.

An UNKNOWN-STATE error is signalled if a state isn't defined."
  (if-let ((a (assoc label (slot-value sm 'states-alist))))
    (cadr a)

    (error 'unknown-state :label label :hint "Make sure state is in scope")))


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
    ;; "normal" form, add to body of current state (which will be a PROGN-STATE)
    (append-to-body current-state (cons fun args))

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
						sm nil trailing-state)))
	   (new-state (if else-state
			    ;; two arms
			    (make-instance 'if-state :in sm
						     :condition condition
						     :then-state-label (label then-state)
						     :else-state-label (label else-state)
						     :successor (if trailing-state
								    (label trailing-state)))

			    ;; one arm
			    (make-instance 'if-state :in sm
						     :condition condition
						     :then-state-label (label then-state)
						     :succesor (if trailing-state
								   (label trailing-state))))))

      ;; link the current state (if there is one) to the new state
      (when current-state
	(setf (successor-state-label current-state) (label new-state)))

      new-state)))


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
			       (list val (label s)))))
			 cases))
	   (new-state (make-instance 'case-state :in sm
						 :condition condition
						 :clauses arms
						 :successor (if trailing-state
								(label trailing-state)))))

      ;; link the current state (if there is one) to the new state
      (when current-state
	(setf (successor-state-label current-state) (label new-state)))

      new-state)))


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
  (let* ((label (car args))
	 (new-state (make-instance 'go-state :in sm
					     :jump-target-state-label label)))

    ;; link the current state (if there is one) to the new state
    (when current-state
      (setf (successor-state-label current-state) (label new-state)))

    ;; skip any unreachable code on this path, and continue parsing from there
    (if-let ((newforms (chew-unreachable-code current-state forms)))
      (parse-tagbody-forms newforms
			   sm nil exit-state))

    new-state))


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
		(setf (successor-state-label current-state) (label exit-state))))

	    (when exit-state
	      ;; we don't have a current state, create one
	      (let ((jump-state (make-instance 'progn-state :in sm)))
		(setf (successor-state-label jump-state) (label exit-state))
		(setq current-state jump-state)))))

      ;; forms to do
      (destructuring-bind (form &rest rest)
	  forms

	(with-current-form form
	  (if (state-label-p form)
	      ;; new state marker
	      (let ((new-state (make-instance 'progn-state :label form :in sm)))
		(when current-state
		  ;; link current state to new state
		  (setf (successor-state-label current-state) (label new-state)))

		;; use this new state as the current state going forward
		(setq current-state new-state)
		(parse-tagbody-forms rest sm new-state exit-state))

	      ;; executable form
	      (progn
		(unless current-state
		  ;; no curent state, create one to hold the form
		  (setf current-state (make-instance 'progn-state :in sm)))

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
machines-within-machines cleaner. Use explicit GO forms, or looping
macros like FOREVER, to keep the machine running."
  (declare (optimize debug))

  (let* ((sm (make-instance 'state-machine))
	 (initial-state (make-instance 'progn-state :in sm :initial-state t))
	 (final-state (make-instance 'go-state :in sm)))

    ;; parse with a final passivating state
    (parse-tagbody-forms forms sm initial-state final-state)

    ;; make the final state jump to itself
    (let ((final-label (label final-state)))
      (setf (jump-target-state-label final-state) final-label))

    ;; return the machine
    sm))


;;; ---------- State machine improvement ----------

(defun remove-empty-states (sm)
  "Remove any empty states.

Empty states can be created in several ways, some of which are legal,
especially in generated code. So we allow them to be created by
BUILD-STATE-MACHINE an then remove them."
  (declare (optimize debug))

  (let ((states (state-machine-states sm)))
    (dolist (state states)

      (when (and (typep state 'progn-state)
		 (empty-state-p state))

	(let ((state-label (label state)))
	  ;; replace any predecessor state's successor label to our successor label
	  ;; (there can be at most one such predecessor state)
	  (if-let ((predecessor-state (some (lambda (s)
					      (if (eql (successor-state-label s)
						       state-label)
						  s))
					    states)))
	    (setf (successor-state-label predecessor-state) (successor-state-label state)))

	  ;; replace any jumps to target our successor state
	  (if-let ((jump-states (filter states
					(lambda (s)
					  (and (typep s 'go-state)
					       (eql (jump-target-state-label s)
						    state-label))))))
	    (dolist (s jump-states)
	      (setf (jump-target-state-label s) (successor-state-label state))))

	  ;; replace any IF arms to target our successor state
	  (if-let ((if-states (filter states
				      (lambda (s)
					(and (typep s 'if-state)
					     (eql (then-state-label s)
						  state-label))))))
	    (dolist (s if-states)
	      (setf (then-state-label s) (successor-state-label state))))
	  (if-let ((if-states (filter states
				      (lambda (s)
					(and (typep s 'if-state)
					     (eql (else-state-label s)
						  state-label))))))
	    (dolist (s if-states)
	      (setf (else-state-label s) (successor-state-label state))))

	  ;; replace any CASE clauses to target our successor state
	  ;; TODO: We could extract the cluases in one pass
	  (if-let ((case-states (filter states
					(lambda (s)
					  (and (typep s 'case-state)
					       (some (lambda (clause)
						       (eql (cadr clause) state-label))
						     (clauses s)))))))
	    (dolist (s case-states)
	      (dolist (clause (clauses s))
		(when (eql (cadr clause) state-label)
		  (setf (cadr clause) (successor-state-label state))))))

	  ;; re-write the initial state of the machine if it's this one
	  (if (eql (initial-state-label sm) state-label)
	      (setf (initial-state-label sm) (successor-state-label state)))

	  ;; delete the state from the machine
	  (setf (states-alist sm)
		(remove-if (lambda (a)
			     (eql (car a) state-label))
			   (states-alist sm))))))))


(defun remove-go-through-states (sm)
  "Remove any jump targets that are to GO states.

If a GO state is a jump target, we can replace the state with a jump directly
to that jump target's target."
  (let ((states (state-machine-states sm)))
    (let* ((go-states-to-go-states (filter states
					   (lambda (s)
					     (and (typep s 'go-state)
						  (typep (state-machine-state-labelled sm
										       (jump-target-state-label s))
							 'go-state)))))
	   (through-state-labels (mapcar #'jump-target-state-label go-states-to-go-states)))

      ;; change the target of each state to jump through
      (dolist (s go-states-to-go-states)
	(let ((through-state (state-machine-state-labelled sm
							   (jump-target-state-label s))))
	  (setf (jump-target-state-label s)
		(jump-target-state-label through-state))))

      ;; remove each through state from the machine
      (setf (states-alist sm)
	    (remove-if (lambda (a)
			 (member (car a) through-state-labels))
		       (states-alist sm))))))


;; TODO: Allow non-removal of explicitly-named states, even if they're redundant,
;; so that we don't affect timing where it's critical

;; TODO: Maybe also have a mode that doesn't introduce states and just uses
;; the explicit ones in the source code, for timing-sensitive bits?

(defun improve-state-machine (sm)
  "Improve the machine SM.

This attempts to minimise the number of states in the machine and
reduce the number of jumps. The resulting machine is neither provably
minimal nor provably optimal, hence the function name. It should
however behave exactly as the original apart from having improved
timing behaviour."
  ;; remove any empty states
  (remove-empty-states sm)

  ;; remove GO states that are just further GOs
  (remove-go-through-states sm)

  ;; return the (hopefully) improved machine
  sm)


;;; ---------- State machine synthesis ----------

(defun synthesise-state-machine (sm)
  "Generate the code for machine SM.

Return the name of the state variable and the body of the constructed machine."
  (declare (optimize debug))

  (let* ((states (state-machine-states sm))
	 (state-labels (state-machine-state-labels sm))
	 (initial-state-label (initial-state-label sm)))

    (if (= (length states) 1)
	;; this is a combinatorial block, so turn it into a PROGN with
	;; no state variable
	(list nil
	      `(progn
		 ,@(progn-body (car states))))

	;; otherwise it's a full machine

	;; TODO: This may need to be more global, for example using symbols
	;; that get compiled to integers?

	(let* ((label-decls (mapcar (lambda (label index)
				      `(,label ,index))
				    state-labels
				    (iota (length state-labels))))
	       (declaration `(declare (as constant ,@state-labels)
				      (ignorable ,@state-labels)))
	       (clauses (mapcar (lambda (label state)
				  `(,label
				    ,@(body state)))
				state-labels
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
	      (list state-variable q)))))))


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
    `(setq ,state-variable ,label)

    ;; the state machine has been optimised away, remove the GO
    (log:debug "Elided GO to state ~a" label)))
