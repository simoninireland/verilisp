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


(defmacro defmacro/vl (name lambda-list &body body)
  "Declare NAME with LAMBDA-LIST as a Verilisp macro.

NAME is declared in the global environment, and so is
available anywhere in a Verilsp program."
  (with-gensyms (external-name)
    `(progn
       ;; define the macro under a new name
       (defmacro ,external-name ,lambda-list
	 ,@body)

       ;; declare macro into Verilisp's global environment
       (with-frame *global-environment*
	 (declare-macro ',name ',external-name)))))


(defmacro importmacro/vl (name)
  "Import Lisp macro NAME globally into Verilisp."
  `(with-frame *global-environment*
     (declare-macro ',name)))


(defmacro macrolet/vl (decls &body body)
  "Declare the macros in DECLS within BODY.

The macros in DECLS are in scope for BODY, and nowhere else.

A MACROLET/VL form should appear only within a DECLAREMACRO/VL form."
  (let ((dms (mapcar (lambda (m)
		       (destructuring-bind (name lambda-list &rest body)
			   m
			 (with-gensyms (f external-name)
			   `(let ((,f (make-frame)))
			      (with-frame ,f

				;; declare macro as usual
				(defmacro ,external-name ,lambda-list
				  ,@body))

			      ;; bind macro into parent's local frame
			      (declare-macro ',name ',external-name)))))
		     decls)))

    `(progn
       ;; declare the local macros
       ,@dms

       ;; continue with the body
       ,@body)))
