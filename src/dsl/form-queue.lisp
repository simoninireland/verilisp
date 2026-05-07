;;;; The form queue
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

;;; The form queue is maintained by passes as they descend and ascend
;;; the code tree. This means that a pass can always identify where
;;; it is in the larger scheme of the code.


;;; ---------- Form queue ----------

(defparameter *current-form-queue* nil
  "The current form being evaluated.

This is a stack of forms being evaluated, used to contextualise
conditions and change synthesis based on a form's position in
the larger program.")


(defmacro with-current-form (form &body body)
  "Execute BODY within the current FORM.

Any conditions reported in BODY will be pointed as FORM as the current
form."
  `(let ((*current-form-queue* (cons ,form *current-form-queue*)))
     ,@body))


(defmacro with-current-form-queue (formq &body body)
  "Run BODY with FORMQ as the form queue."
   `(let ((*current-form-queue* ,formq))
     ,@body))


;;; Form queue accessors

(defun current-form ()
  "Return the current form."
  (car *current-form-queue*))


(defun form-head (form)
  "Return the head of the current form.

This is safe for atomic and list forms."
  (if (atom form)
	form
	(car form)))


(defun current-form-head ()
  "Return the head of the current form.

This function is safe for atom forms or list forms."
  (form-head (current-form)))


(defun containing-form (&optional form)
  "Return the form containing the current form.

If FORM is provided then return the shallowest containing form
that is that form."
  (labels ((find-form (l)
	     (cond ((null l)
		    nil)

		   ((eql (caar l) form)
		    (car l))

		   (t
		    (find-form (cdr l))))))

    (if form
	;; search for the shallowest containing form with the given tag
	(find-form (cdr *current-form-queue*))

	;; extract the immediately containing form
	(cadr *current-form-queue*))))


(defun containing-containing-form ()
  "Return the form containing the containing form."
  (if (> (length *current-form-queue*) 2)
      (caddr *current-form-queue*)))


(defun in-context-p (cl)
  "Test whether there is some form in the form queue matching CL.

CL should be a function of no variables that operates on the
current form. The test starts working from the *containing* form.

Returns the first matching form, or NIL."
  (block found-context
    (let ((context (cdr *current-form-queue*)))
      (maplist (lambda (formq)
		 (with-current-form-queue formq
		   (when (funcall cl)
		     (return-from found-context (current-form)))))
	       context)

      ;; if we get here, we've not found a matching form
      nil)))
