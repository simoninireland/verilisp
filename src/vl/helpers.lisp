;; Helper macros for writing DSL functions
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


;; ---------- Errors that underlie Verilisp ----------

(defmacro with-vl-errors-not-synthesisable (&body body)
  "Run BODY within a handler that translates non-Verilisp errors.

Non-error conditions are passed through; non-Verilisp-specific errors
are reported as NOT-SYNTHESISABLE errors."
  `(handler-bind ((vl-error (lambda (c)
			      (error c)))

		  (error (lambda (c)
			   (error 'not-synthesisable :underlying-condition c))))

     ,@body))


;; ---------- Implicit forms ----------

(defmacro with-implicit-progn (body)
  "Return the forms in BODY as an implicit PROGN form."
  `(cons 'progn ,body))


(defmacro with-implicit-tagbody (body)
  "Return the forms in BODY as an implicit TAGBODY form."
  `(cons 'tagbody ,body))


;; ---------- Unknown forms in Verilisp code ----------

(defun failed-form (condition)
  "Return the form that can't be handled based on CONDITION.

If CONDITION does not indicate such a failure, return nil.

There is no standard way to retrieve this information, so this
function is implementation-dependent."

  #+sbcl
  (if (subtypep (type-of condition) 'sb-pcl::no-applicable-method-error)
      (car (slot-value condition 'sb-pcl::args))

      ;; condition is not caused by an unknown form
      nil)

  ;; can't handle other Lisp implementations for now
  #-sbcl
  nil)


(defmacro with-unknown-forms (&body body)
  "Run BODY in an environment that traps errors due to unknown forms.

Any unknown forms are reported as UNKNOWN-FORM exceptions. The
actual way these forms are captured is unfortunately implementation-specific."
  `(handler-bind ((error #'(lambda (c)
			     (declare (optimize debug))

			     (if-let ((form (failed-form c)))
			       ;; we encountered an unknown form, signal it as such
			       (error 'unknown-form :form form)

			       ;; propagate the condition
			       (error c)))))
     ,@body))


;; ---------- Continuing compilation after an error ----------

(defun recover-on-error-report (str)
  "Report the recovery action to STR."
  (format str "Recover from ~a" (current-form)))


(defmacro with-recover-on-error (recovery &body body)
  "Run the BODY forms, offering a restart that runs the RECOVERY form on error.

The recovery action is triggered by calling RECOVER, which invokes
the RECOVER restart installed by this macro.

The recovery form should do whatever is necessary to best continue
compilation. The handler may decide not to synthesise code after such
an error has been signalled; alternatively it may treat some such
errors as warnings and still synthesise code."
  `(restart-case
       (progn
	 ,@body)
     (recover ()
       :report recover-on-error-report
       ,recovery)))


(defmacro recover ()
  "Run the recovery action.

A RECOVER restart must be available."
  `(invoke-restart 'recover))
