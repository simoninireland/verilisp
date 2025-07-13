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
    :accessor body))
  (:documentation "Abstract state in a state machine."))


(defmethod initialize-instance :after ((s state) &key label &allow-other-keys)
  (unless label
    (setf (slot-value s 'label) (gensym))))


;; ---------- TAGBODY ----------

(defun state-marker-p (form)
  "Test whether FORM is a new state marker.

This simply tests whether FORM is a symbol."
  (symbolp form))


(defun extract-states (forms)
  "Extract the states from FORMS.

Return a list of lists, each element being a state label and the state body."
  (flet ((extract-state (states form)
	   (if (state-marker-p form)
	       ;; new state
	       (cons (list form) states)

	       (progn
		 ;; check whether we have an unlabelled initial state
		 (when (null states)
		   ;; construct a new state
		   (setq states (list (list (gensym)))))

		 ;; add form to current state
		 (let* ((current-state (car states))
			(current-body (cadr current-state)))
		   (if (null current-body)
		       (setf (cdr current-state) (list (list form)))
		       (setf (cdr current-body) (list form)))
		   states)))))

    (reverse (foldr #'extract-state forms '()))))


(defmethod typecheck-sexp ((fun (eql 'tagbody)) args)
  (let* ((states (extract-states args))
	 (state-labels (mapcar #'car states))
	 (state-bodies (mapcar #'cadr states)))

    (with-new-frame
      ;; add labels to frame
      (dolist (n state-labels)
	(declare-variable n '((as label)
			      (ignorable t))))

      ;; typecheck the bodies
      (dolist (b state-bodies)
	(typecheck (with-implicit-progn b)))

      ;; tagbody doesn't return a value (yet)
      nil)))


(defmethod dependencies-sexp ((fun (eql 'tagbody)) args)
  (foldr (lambda (deps form)
	   (if (symbolp form)
	       deps
	       (union deps (dependencies form))))
	 args'()))


(defmethod read-written-variables-sexp ((fun (eql 'tagbody)) args)
  (foldr (lambda (deps form)
	   (if (symbolp form)
	       deps
	       (union2 deps (read-written-variables form))))
	 args '(() ())))


(defgeneric parse-tagbody-forms-sexp (fun args forms current-state exit-state)
  (:documentation "Parse FUN applied to ARGS.

Methods on this function should construct a state machine for
FUN applied to ARGS, integrate it into CURRENT-STATE, and then
proceed to parse FORMS (typically by calling PARSE-TAGBODY-FORMS).

EXIT-STATE is the 'fall-through' state that the form may use.

Returns a list consisting of a list of the states created, with
the entry state first, and a boolean indicating whether the
form fell-through and should therefore continue to EXIT-STATE.")
  (:method (fun args forms current-state exit-state)
    ;; "normal" form, add to body of current state
    (appendf (body current-state) (list (cons fun args)))
    (parse-tagbody-forms forms current-state exit-state)))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'tagbody)) args forms current-state exit-state)
  ;; nested machine, start a new state for this machine
  (let* ((trailing-states (parse-tagbody-forms forms nil))
	 (trailing-state (if trailing-states
			     (car trailing-states)
			     exit-state))
	 (nested-body args)
	 (nested-states (parse-tagbody-forms nested-body nil trailing-state)))

    (append (list current-state)
	    nested-states
	    trailing-states)))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'if)) args forms current-state exit-state)
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
	   (then-label (label then-state))
	   (else-states (if else-branch
			    (parse-tagbody-forms else-branch
						 nil trailing-state)))
	   (else-state (if else-states
			   (car else-states)))
	   (else-label (if else-states
			   (label else-state))))

      ;; add the condition to the current state
      (let ((cform (if else-states
		       ;; two arms
		       `(if ,condition
			    (go ,then-label)
			    (go ,else-label))

		       ;; one arm, jump to the trailing state on false
		       `(if ,condition
			    (go ,then-label)
			    (go ,(label trailing-state))))))
	(appendf (body current-state) (list cform)))

      (append (list current-state)
	      then-states
	      else-states
	      trailing-states))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'case)) args forms current-state exit-state)
  (destructuring-bind (condition &rest cases)
      args

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

      (append (list current-state)
	      arm-states
	      trailing-states))))


(defmethod parse-tagbody-forms-sexp ((fun (eql 'go)) args forms current-state exit-state)
  (let ((label (car args)))
    ;; add form to current state
    (appendf (body current-state) (list (cons fun args)))

    ;; check whether there is unreachable code on this path
    (if (not (or (null forms)
		 (state-marker-p (car forms))))
	(progn
	  ;; next form does not start a new state
	  (warn 'unreachable-code :hint "Check the logic")

	  ;; skip to the next state marker
	  (do ()
	      ((or (null forms)
		   (state-marker-p (car forms)))
	       forms)
	    (setq forms (cdr forms)))))

    (let ((trailing-states (if (not (null forms))
			       (parse-tagbody-forms forms
						    (make-instance 'state)
						    exit-state))))
      (if trailing-states
	  (cons current-state
		trailing-states)
	  (list current-state)))))


(defun parse-tagbody-forms (forms current-state exit-state)
  "Parse FORMS as the body of a TAGBODY, returning a state machine.

The FORMS are built into CURRENT-STATE until there is a state
change. If a state falls-through, it lands in EXIT-STATE.

Return a list of states created, initial state (of the path) first."
  (declare (optimize debug))

  (if (null forms)
      ;; finished this path
      (when current-state
	(when exit-state
	  ;; add transition to exit state
	  (appendf (body current-state) (list `(go ,(label exit-state)))))

	(list current-state))

      ;; path continues with more forms
      (let ((form (car forms)))
	(if (state-marker-p form)
	    ;; new state
	    (let ((trailing-states (parse-tagbody-forms (cdr forms)
							(make-instance 'state :label form)
							exit-state)))
	      (if current-state
		  (when trailing-states
		    ;; add jump to next state to current state
		    (appendf (body current-state) (list `(go ,(label (car trailing-states)))))

		    ;; prepend current state
		    (cons current-state
			  trailing-states))

		  ;; initial state, nothing to prepend
		  trailing-states))

	    ;; otherwise, part of the current state's body
	    (progn
	      (if (null current-state)
		  ;; this is the body of an unlabelled initial state, so
		  ;; create the state to hold it
		  (setq current-state (make-instance 'state)))

	      ;; parse form
	      (if (listp form)
		  (destructuring-bind (fun &rest args)
		      form
		    (parse-tagbody-forms-sexp fun args
					      (cdr forms)
					      current-state exit-state))

		  (progn
		    (appendf (body current-state) (list form))
		    (parse-tagbody-forms (cdr forms)
					 current-state exit-state))))))))


(defun build-state-machine (forms)
  "Parse FORMS as the body of a TAGBODY, building a state machine."
  (declare (optimize debug))

  (let* ((passive-state (make-instance 'state))
	 (states (parse-tagbody-forms forms nil passive-state)))

    ;; return all the states, passivating state last
    (append states (list passive-state))))


(defmethod dependencies-sexp ((fun (eql 'tagbody)) args)
  (foldr (lambda (deps form)
	   (if (symbolp form)
	       deps
	       (union deps (dependencies form))))
	 args'()))


(defmethod read-written-variables-sexp ((fun (eql 'tagbody)) args)
  (foldr (lambda (deps form)
	   (if (symbolp form)
	       deps
	       (union2 deps (read-written-variables form))))
	 args'(() ())))


;; ---------- GO ----------

(defmethod typecheck-sexp ((fun (eql 'go)) args)
  (let ((label (car args)))
    ;; ensure label is in scope
    (unless (variable-declared-p label)
      (error 'unknown-state :state label))
    (unless (eql (get-representation label) 'label)
      (error 'unknown-state :state label))
    (break)
    ;; GO doesn't really have a type
    t))


(defmethod dependencies-sexp ((fun (eql 'go)) args)
  nil)


(defmethod read-written-variables-sexp ((fun (eql 'go)) args)
  '(() ()))
