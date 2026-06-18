;;;; Macro definition macros
;;;;
;;;; Copyright (C) 2024--20256 Simon Dobson
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

;;; Macro definition. We can't simply inherit Lisp's macros because they
;;; can't be guaranteed to generate valid Verilisp, so we re-create the
;;; macros we want explicitly.
;;;
;;; Some macros are core, defined in the core environment and so
;;; always available; others are defined in the global environment;
;;; and others are "local" to other macros.


;;; ---------- Environment management----------

(defun declare-macro (m f)
  "Declare M as a macro with body F in the current environment."
  (declare-variable m `((name ,m)
			(initial-value ,f) (as macro))))


(defun macro-declared-p (m)
  "Test whether M is declared as a macro in the global environment."
  (and (variable-declared-p m)
       (eql (get-representation m) 'macro)))


(defun declare-symbol-macro (m f)
  "Declare M as a symbol macro with body F in the current environment."
  (declare-variable m `((name ,m)
			(initial-value ,f) (as symbol-macro))))


(defun symbol-macro-declared-p (m)
  "Test whether M is declared as a symbol macro in the global environment."
  (and (variable-declared-p m)
       (eql (get-representation m) 'symbol-macro)))


;;; ---------- Declaration ----------

(defun translate-lambda-list (l)
  "Translate a macro-style lambda-list L to a function-style lambda-list.

Specifically this changes &body entries into &rest, which are equivalent
but are laid-out differently."
  (mapcar (lambda (e)
	    (cond ((eql e '&body)
		   '&rest)

		  (t
		   e)))
	  l))


(defmacro defmacro/vl (name lambda-list &body body)
  "Declare NAME with LAMBDA-LIST as a Verilisp macro.

NAME is declared in the global environment, and so is
available anywhere in a Verilisp program."

  ;; test whether the macro already exists
  (in-global-environment
    (when (variable-declared-p name)
      ;; variable exists, delete it to allow re-definition
      (warn 'duplicate-macro :name name)
      (forget-variable name)))

  (with-gensyms (tll)
    `(in-global-environment
       (declare-macro ',name (lambda (&rest ,tll)
			       (destructuring-bind ,(translate-lambda-list lambda-list)
				   ,tll
				 ,@body))))))


(defmacro defcoremacro/vl (name lambda-list &body body)
  "Declare NAME with LAMBDA-LIST as a macro in core Verilisp.

This should only be used during system loading to populate the core
environment with core macros. As such re-declaring a core macro gives
rise to an error, not a warning."
  (with-gensyms (tll)
    `(in-core-environment
       (declare-macro ',name (lambda (&rest ,tll)
			       (destructuring-bind ,(translate-lambda-list lambda-list)
				   ,tll
				 ,@body))))))



;;; TODO: I think we can get rid of these and replace them with MACROLET and
;;; SYMBOL-MACROLET in Verilisp code

(defmacro macrolet/vl (decls &body body)
  "Declare the macros in DECLS within BODY.

The macros in DECLS are in scope for BODY, and nowhere else.

A MACROLET/VL form should appear only within a DECLAREMACRO/VL form."
  (let ((dms (mapcar (lambda (m)
		       (destructuring-bind (name lambda-list &rest body)
			   m
			 (with-gensyms (tll)
			   `(declare-macro ',name (lambda (&rest ,tll)
						    (destructuring-bind ,(translate-lambda-list lambda-list)
							,tll
						      ,@body))))))
		     decls)))

    `(progn
       ;; declare embedded macros
       ,@dms

       ;; interpolate the body
       ,@body)))


(defmacro symbol-macrolet/vl (decls &body body)
  "Declare the symbol macros in DECLS within BODY.

The symbol macros in DECLS are in scope for BODY, and nowhere else.

A SYMBOL-MACROLET/VL form should appear only within a DECLAREMACRO/VL form."
  (let ((dms (mapcar (lambda (m)
		       (destructuring-bind (name form)
			   m
			 (with-gensyms (n)
			   `(declare-symbol-macro ',name (lambda (,n)
							   ,form)))))
		     decls)))

    `(progn
       ;; declare embedded macros
       ,@dms

       ;; interpolate the body
       ,@body)))


;;; SYMBOL-MACROLET can appear within Verilisp code as well.

(defmethod into-arguments-macros ((fun (eql 'symbol-macrolet)) args &key pass-name option extra)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (with-new-frame
      (dolist (m decls)
	(destructuring-bind (name form)
	    m
	  (declare-symbol-macro name (lambda (f)
				       (declare (ignore f))
				       form))))

      (apply pass-name (cons (with-implicit-progn body) extra)))))
