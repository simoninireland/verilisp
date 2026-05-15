;;;; Helper macros for writing DSL functions
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


;;; ---------- Errors that underlie Verilisp ----------

(defmacro with-vl-errors-not-synthesisable (&body body)
  "Run BODY within a handler that translates non-Verilisp errors.

Non-error conditions are passed through; non-Verilisp-specific errors
are reported as NOT-SYNTHESISABLE errors."
  `(handler-bind ((vl-error (lambda (c)
			      (error c)))

		  (error (lambda (c)
			   (error 'not-synthesisable :underlying-condition c))))

     ,@body))


;;; ---------- Implicit forms ----------

(defmacro with-implicit-progn (body)
  "Return the forms in BODY as an implicit PROGN form."
  `(cons 'progn ,body))


(defmacro with-implicit-tagbody (body)
  "Return the forms in BODY as an implicit TAGBODY form."
  `(cons 'tagbody ,body))
