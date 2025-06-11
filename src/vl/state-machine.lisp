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


(defun typecheck-sm-state (state)
  "Typecheck a single machine STATE description."
  (destructuring-bind (name &rest body)
      state

    ;; states must be integers -- should generalise to symbols too
    (ensure-subtype name 'unsigned-byte)

    ;; the type of the state is the type of the last form
    (typecheck `(progn ,@body))))


(defmethod typecheck-sexp ((fun (eql 'state-machine) args))
  (let ((tys (mapcar #'typecheck-sm-state args)))
    (last tys)))


(defmethod synthesise-sexp ((fun (eql 'state-machine)) args)
  (with-gensyms (state)
    (let ((sm `(@ (posedge clk)
		  (case ,state
		    ,@args)
		  )))

      (synthesise sm))))


(defmethod synthesise-sexp ((fun (eql 'next-state)) args)

  )




(defgeneric construct-state-machine (form)
  (:documentation "Construct a state machine from FORM.")
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (construct-state-machine-sexp fun args))))


(defmethod construct-state-machine ((form integer))
  form)


(defmethod construct-state-machine ((form symbol))
  form)


(defgeneric construct-state-machine-sexp (fun args)
  (:documentation "Construct a state machine for FUN applied to ARGS.

The default leaves the form unchanged.")
  (:method (fun args)
    `(,fun ,@args)))


(defmethod construct-state-machine-sexp ((fun (eql 'progn)) args)
  (let ((i 0)
	(states '())
	(current-state '())
	(updated-in-current-state '()))

    (flet ((split-into-states (form)
	     (let* ((updated (dependencies form))
		    (all-dependencies (foldr #'union
					     (mapcar (rcurry #'variable-property :dependencies)
						     updated)
					     '())))
	       (when (not (null (intersection all-dependencies updated-in-current-state)))
		 ;; we're doing updates that depend on previous updated values,
		 ;; construct a new state
		 (appendf current-state (list `(next-state ,(1+ i))))
		 (appendf states (list (cons i current-state)))
		 (incf i)
		 (setq current-state '())
		 (setq updated-in-current-state '()))

	       ;; we're doing an independent update, append to this state
	       (appendf current-state (list form))
	       (appendf updated-in-current-state updated))))

      (mapc #'split-into-states args)

      (appendf states (list (cons i current-state)))
      states

      )


    )

  )


(with-new-frame
  (declare-variable 'a '((:type (unsigned-byte 8))
			 (:initial-value 1)))
  (declare-variable 'b '((:type (unsigned-byte 8))
			 (:initial-value 21)))

  (dependencies '(progn (setq a 10) (setq b (+ a 12)) (setq a 12)))
  (construct-state-machine '(progn (setq a 10) (setq b (+ a 12)) (setq a 12))))
