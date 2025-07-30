;; Macro definition macros
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
available anywhere in a Verilsp program."

  ;; test whether the macro already exists
  (when (variable-declared-in-environment-p name *global-environment*)
    ;; variable exists, delete it to allow re-definition
    (warn 'duplicate-macro :name name)
    (forget-environment-variable name *global-environment*))

  (with-gensyms (tll)
    `(with-frame *global-environment*
       (declare-macro ',name (lambda (&rest ,tll)
			       (destructuring-bind ,(translate-lambda-list lambda-list)
				   ,tll
				 ,@body))))))


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
