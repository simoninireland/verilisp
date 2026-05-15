;;;; DSL builder conditions
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

(in-package :verilisp/dsl)


(define-condition dsl-error (error)
  ((hint
    :documentation "A hint as to how to fix the condition."
    :initarg :hint
    :initform "No suggestion."
    :reader hint))
  (:report (lambda (c str)
	     (format str "DSL error: ~s" (hint c))))
  (:documentation "Error signalled by errors within the DSL-defining macros."))


(define-condition unknown-form (dsl-error)
  ((form
    :documentation "The form."
    :initarg :form
    :initform (current-form)
    :reader form))
  (:report (lambda (c str)
	     (format str "Unknown form ~a" (form c))))
  (:documentation "Condition signalled when an unknown form is encountered.

This usually means that a pass has encountered a form that shouldn't be
present. Often this means that a macro hasn't been expanded or a form hasn't
been transformed away."))
