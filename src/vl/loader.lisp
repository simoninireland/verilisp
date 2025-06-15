;; Loader macros and helpers
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


;; ---------- Macros we allow in Verilisp ----------

(defvar *macro-environment* (empty-environment)
  "The frame containing all the macros available in Verilisp.

The frame is initially detatched. It should be attached to the global
environment before calling EXPAND-MACROS-IN-ENVIRONMENT (and can be
detached again afterwards).")


;; declare all macros available by default
(with-frame *macro-environment*
  ;; conditionals
  (declare-macro 'cond)
  (declare-macro 'when 'when/vl)
  (declare-macro 'unless 'unless/vl)

  ;; representation-specific lets
  (declare-macro 'let-wires)
  (declare-macro 'let-registers)
  (declare-macro 'let-constants)

  ;; places
  (declare-macro 'incf 'incf/vl)
  (declare-macro 'decf 'decf/vl)

  ;; tests and maths
  (declare-macro '0=)
  (declare-macro '0/=)
  (declare-macro '1+ '1+/vl)
  (declare-macro '1- '1-/vl)
  (declare-macro '2* '2*)

  ;; variable introduction and aliasing
  (declare-macro 'with-bitfields)

  ;; state machines
  (declare-macro 'state-machine)
  (declare-macro 'next 'next/vl)
  (declare-macro 'exit 'exit/vl))


;; ---------- Module registry ----------

(defvar *module-list* nil
  "The module registry.

Modules added here are queued for synthesis.")


(defvar *module-interfaces* '()
  "Mapping of known modules to their interface types.

This variable contains all the modules that can be imported. It will
contain the types of all the modules in *MODULE-LIST* that have been
defined in this session, plus any externlly-defined modules made
available for import.")


(defun clear-module-registry ()
  "Clear the module registry of all imported and declared modules."
  (setq *module-list* '()
	*module-interfaces* '()))


(defun known-module-interface-p (modname)
  "Test whether MODNAME is known as a module interface."
  (not (null (assoc modname *module-interfaces*))))


(defun add-module-interface (modname intf)
  "Add module MODNAME with given INTF."
  (when (known-module-interface-p modname)
    (error 'duplicate-module :module modname))

  (appendf *module-interfaces* (list (list modname intf))))


(defun get-module-interface (modname)
  "Return the module interface type for MODNAME."
  (if-let ((m (assoc modname *module-interfaces*)))
    (cadr m)

    (error 'unknown-module :module modname
			   :hint "Make sure the module has been defined or imported")))


(defun synthesising-module-p (modname)
  "Test whether MODNAME is queued for synthesis."
  (not (null (assoc modname *module-list*))))


(defun add-module-for-synthesis (modname module)
  "Queue the module MODNAME with code MODULE for synthesis."
  (when (synthesising-module-p modname)
    (error 'duplicate-module :module modname))

  (appendf *module-list* (list (list modname module))))


(defun get-module (modname)
  "Return the module code for MODNAME.

This will typically have been set by DEFMODULE and so will have been
type-checked, macro-expanded, and possibly had other passes applied."
  (if-let ((m (assoc modname *module-list*)))
    (cadr m)

    (error 'unknown-module :module modname
			   :hint "Make sure the module has been declared")))


(defun get-modules-for-synthesis ()
  "Return an alist consisting of module names and their declarations."
  *module-list*)


;; ---------- Module declaration ----------

(defun elaborate-module (form)
  "Elaborate FORM as a module.

This runs all the relevant compiler nanopasses, returning a list
consisting of the module interface type and the fully-elaborated
module ready for synthesis."
  ;; expand macrs
  (let ((expanded (expand-macros-in-environment form *macro-environment*)))

    ;; typecheck
    (let ((intf (typecheck expanded)))
      ;; add dependencies
      (dependencies expanded)

      ;; simplify
      (let* ((floated (car (float-let-blocks expanded)))
	     (simplified (simplify-progn floated)))

	(list intf simplified)))))


(defmacro defmodule (modname decls &body body)
  "Declare a module MODNAME with given DECLS and BODY.

The module is loaded, annotated, macro-expanded, type-checked
(meaning that it will be at least minimally syntactically correct
afterwards -- although possibly still not finally synthesisable),
and then be processed ready for synthesis (which might cause further
warnings or errors).

The resulting fully-elaborated module is added to *MODULE-LIST* for
synthesis. Its type is added to *MODULE-INTERFACES* for importing.
Duplicate module names will cause a DUPLICATE-MODULE error.

Return the name of the newly-defined module."
  (with-gensyms (module rc intf elaborated)
    (let ((code `(module ,modname ,decls
			 ,@body)))
      `(let* ((,module ',code)
	      (,rc (elaborate-module ,module)))

	 (destructuring-bind (,intf ,elaborated)
	     ,rc

	   ;; typecheck and elaborate the expanded module
	   ;; add type to interfaces available for import
	   (add-module-interface ',modname ,intf)

	   ;; add expanded code to modules for synthesis
	   (add-module-for-synthesis ',modname ,elaborated)

	   ',modname)))))


;; ---------- Module synthesis ----------

(defun synthesise-module (m str)
  "Synthesise module M to STR.

M can be a Verilisp form or a symbol identifying
a loaded module."
  (with-synthesis-to-stream str
    (let ((vl (if (symbolp m)
		  (get-module m)
		  m)))
      (synthesise vl))))
