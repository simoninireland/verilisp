;; Macro definition
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

;; We re-implement some macros to avoid possible issues with the standard
;; versions generating non-synthesisable Lisp. The names of these macros
;; are suffixed /vl, and we alias them using ADD-MACRO in the loader.

(in-package :verilisp/core)
(declaim (optimize debug))


;; ---------- Macro environment  ----------

(defvar *macro-environment* (empty-environment)
  "The frame containing all the macros available in Verilisp.

The frame is initially detatched. It should be attached to the global
environment before calling EXPAND-MACROS-IN-ENVIRONMENT (and can be
detached again afterwards).")


(defun forget-macro/vl (name)
  "Forget the macro NAME."
  (forget-environment-variable name *macro-environment*))


;; ---------- Macro definition macros ----------

(defmacro defmacro/vl (name args &body body)
  "Declare NAME with ARGS as a Verilisp macro."
  (with-gensyms (external-name)
    `(progn
       (defmacro ,external-name ,args
	 ,@body)

       (with-frame *macro-environment*
	 (declare-macro ',name ',external-name)))))


(defmacro importmacro/vl (name)
  "Import Lisp macro NAME into Verilisp."
  `(with-frame *macro-environment*
     (declare-macro ',name)))
