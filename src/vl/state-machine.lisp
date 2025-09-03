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

(in-package :verilisp/core)
(declaim (optimize debug))


;; ---------- State representation ----------

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
   (synthetic-p
    :documentation "Flag whether the state is synthetic."
    :initform nil
    :reader synthetic-p))
  (:documentation "Abstract state in a state machine."))


(defmethod initialize-instance :after ((s state) &key label &allow-other-keys)
  (unless label
    (setf (slot-value s 'label) (gensym))
    (setf (slot-value s 'synthetic-p) t)))


;; ---------- TAGBODY ----------

(defun state-label-p (form)
  "Test whether FORM is a new state label.

This simply tests whether FORM is a symbol."
  (symbolp form))


(defun extract-states (forms)
  "Extract the states from FORMS.

Return a list of lists, each element being a state label and the state body."
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


(defmethod compute-type-sexp ((fun (eql 'tagbody)) args)
  (declare (optimize debug))

  (let* ((states (extract-states args))
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


(defmethod read-variables-sexp ((fun (eql 'tagbody)) args)
  (foldr (lambda (deps form)
	   (if (state-label-p form)
	       deps
	       (union deps (read-variables form))))
	 args '()))


(defmethod compute-dependencies-sexp ((fun (eql 'tagbody)) args)
  (let* ((states (extract-states args))
	 (state-bodies (mapcar #'cdr states)))

    ;; compute-type the bodies
    (dolist (b state-bodies)
      (compute-dependencies (with-implicit-progn b)))))


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
	   (trailing-label (if trailing-state
			       (label trailing-state)))
	   (then-states (parse-tagbody-forms (list then-branch)
					     nil trailing-state))
	   (then-state (car then-states))
	   (then-label (label then-state))
	   (else-states (if else-branch
			    (parse-tagbody-forms else-branch
						 nil trailing-state)))
	   (else-state (if else-states
			   (car else-states)))
	   (else-label (if else-states
			   (label else-state))))

      ;; compile the conditional, with optimisations
      (cond ((and (= (length then-states) 1)
		  (or (null else-states)
		      (= (length else-states) 1)))
	     ;; then arm is a single state, else is either a single state
	     ;; or missing, so we can coalesce both diretly into this state
	     ;; rather than creating new intermediate states

	     ;; recompile the arms to fall-through
	     (let* ((then-states (parse-tagbody-forms (list then-branch)
						      nil nil))
		    (then-state (car then-states))
		    (else-states (if else-branch
				     (parse-tagbody-forms else-branch
							  nil nil)))
		    (else-state (if else-states
				    (car else-states))))

	       (let ((cform (if else-states
				;; two arms
				`(if ,condition
				     (progn
				       ,@(body then-state))
				     (progn
				       ,@(body else-state)))

				;; one arm
				`(if ,condition
				     (progn
				       ,@(body then-state))))))

		 ;; add the condition to the current state
		 (appendf (body current-state) (list cform))

		 ;; continue into the trailing states, if any
		 (when trailing-state
		   (if (synthetic-p trailing-state)
		       (progn
			 ;; trailing state is synthetic, remove it and
			 ;; fold its body into the current state
			 (appendf (body current-state) (body trailing-state))

			 ;; don't then synthesise this merged state
			 (cdr trailing-states))

		       (progn
			 ;; trailing state isn't synthetic, add a drop-through
			 ;; jump to it
			 (appendf (body current-state) `((go ,trailing-label)))

			 ;; retain the state in the machine
			 trailing-states))))))

	    ((or (null else-states)
		 (= (length else-states) 1))
	     ;; then arm is a full state machine but else is either a
	     ;; single state or missing, so we can build it into this state
	     ;; but we need to keep the trailing states as a state for the
	     ;; then arm to jump back to
	     (let ((cform (if else-states
			      ;; two arms
			      `(if ,condition
				   (go ,then-label)
				   (progn
				     ,@(body else-state)))

			      ;; one arm
			      `(if ,condition
				   (go ,then-label)
				   (go ,trailing-label)))))

	       ;; add the condition to the current state
	       (appendf (body current-state) (list cform))

	       ;; return all the states created
	       (append then-states
		       (if trailing-states
			   trailing-states))))

	    ((= (length then-states) 1)
	     ;; then arm is a single state but the else arm is a full
	     ;; state machine, so integrate the then arm but keep the
	     ;; trailing states as a state for the else arm to jump to
	     (let ((cform `(if ,condition
			       (progn
				 ,@(body then-state))
			       (go ,else-label))))

	       ;; add the condition to the current state
	       (appendf (body current-state) (list cform))

	       ;; return all the states created
	       (append else-states
		       (if trailing-states
			   trailing-states))))

	    (t
	     ;; default creates new states for both arms
	     (let ((cform (if else-states
			      ;; two arms
			      `(if ,condition
				   (go ,then-label)
				   (go ,else-label))

			      ;; one arm, jump to the trailing state on false
			      `(if ,condition
				   (go ,then-label)
				   (go ,trailing-label)))))

	       ;; add condition to current state
	       (appendf (body current-state) (list cform))

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
	(appendf (body current-state) (list cform)))

      (append arm-states
	      trailing-states))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'go)) args forms current-state exit-state)
  (let ((label (car args)))
    ;; add form to current state
    (appendf (body current-state) (list (cons fun args)))

    ;; check whether there is unreachable code on this path
    (when (not (or (null forms)
		   (state-label-p (car forms))))
      ;; next form does not start a new state, and so is unreachable
      ;; report against the offending (unreachable) form, not the GO form
      (with-current-form (car forms)
	(warn 'unreachable-code :label (label current-state)
				:hint "Check the logic"))

      ;; skip to the next state marker
      (do ()
	  ((or (null forms)
	       (state-label-p (car forms)))
	   forms)
	(setq forms (cdr forms))))

    (if (not (null forms))
	(parse-tagbody-forms forms
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
		;; we have a current state, append a jump to the exit state
		(appendf (body current-state) (list `(go ,(label exit-state))))
		nil)

	    (when exit-state
		;; we don't have a current state, create one to hold the jump
		;; (which may then be optimised away later)
		(let ((jump-state (make-instance 'state)))
		  (appendf (body jump-state) (list `(go ,(label exit-state))))
		  (list jump-state)))))

      ;; forms to do
      (destructuring-bind (form &rest rest)
	  forms

	(with-current-form form
	  (if (state-label-p form)
	      ;; new state marker
	      (let ((new-state (make-instance 'state :label form)))
		(if current-state
		    ;; link current state to new state
		    (appendf (body current-state) (list `(go ,(label new-state)))))

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
			(appendf (body current-state) (list form))
			(parse-tagbody-forms rest current-state exit-state)))))))))


(defun build-state-machine (forms)
  "Parse FORMS as the body of a TAGBODY, building a state machine.

The resulting machine is necessarily 'top-level', not contained in
another machine. It has a passivating state added to the end, which it
will remain in if it ever gets to that point. This allows machines to
be built that run once and then stop. Use explicit GO forms, or
looping macros like FOREVER, to keep the machine running."
  (declare (optimize debug))

  (let* ((passive-state (make-instance 'state))
	 (states (parse-tagbody-forms forms nil passive-state)))

    ;; return all the states, passivating state last
    (append states (list passive-state))))


(defun merge-state-machine-empty-states (machine)
  "Return an alist mapping states in MACHINE to only the necessary states.

A state is unnecessary if it consists purely of a GO to another state."
  (declare (optimize debug))

  (labels ((mergeable-state (m)
	     (let ((b (body m)))
	       (if (and (not (null b))                          ; body not empty
			(= (length b) 1)                        ; single form
			(eql (caar b) 'go)                      ; ... which is a GO
			(not (eql (cadr (car b)) (label m))))   ; ... and isn't back to the same state

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


(defmethod transform-sexp ((fun (eql 'tagbody)) args)
  (declare (optimize debug))

  (destructuring-bind (newbody newenv)
      (float-let-blocks (with-implicit-tagbody args))

    (destructuring-bind (newfun &rest newargs)
	newbody

      (unless (eql newfun 'tagbody)
	(error "Incorrectly floated LET blocks in TAGBODY"))

      ;; we hold on to the NEWENV frame because it contains all the information
      ;; we've already extracted about the variables -- and these were the only
      ;; ones in scope when the code was analysed, with others beng created
      ;; by SYNTHESISE-STATE-MACHINE. which adds the necessary types and
      ;; representations directly

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

	  (transform p))))))


;; ---------- GO ----------

(defmethod compute-type-sexp ((fun (eql 'go)) args)
  (declare (optimize debug))

  (let* ((label (car args))
	 (merged-states (if (variable-declared-p 'tagbody-merged-states)
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


(defmethod compute-dependencies-sexp ((fun (eql 'go)) args))


(defmethod read-variables-sexp ((fun (eql 'go)) args)
  '())


(defmethod transform-sexp ((fun (eql 'go)) args)
  (let* ((merged-states (get-initial-value 'tagbody-merged-states))
	 (state-label (get-label-from-merged-states (car args) merged-states))
	 (state-variable (get-initial-value 'tagbody-state-variable)))

    `(setq ,state-variable ,state-label)))
