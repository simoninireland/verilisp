;;;; Loader macros and helpers
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
(declaim (optimize debug))


;;; ---------- Module registry ----------

;;; Modules are all held in the global environment.

(defun declare-module (modname intf code)
  "Declare a module MODNAME with the given INTF and CODE.

The module is declared in *GLOBAL-ENVIRONMENT*. Modules can be
re-defined, overwriting previous declarations and signalling
a DUPLICATE-MODULE warning."
  (in-global-environment

    ;; test whether the module already exists
    (when (variable-declared-p modname)
      ;; variable exists, delete it to allow re-definition
      (warn 'duplicate-module :module modname)
      (forget-variable modname))

    (declare-variable modname `((type ,intf)
				(initial-value ,code)
				(as module)))))


(defun declare-imported-module (modname intf)
  "Declare an imported module MODNAME with the given INTF.

The module is declared in *GLOBAL-ENVIRONMENT*. Modules can be
re-defined, overwriting previous declarations and signalling
a DUPLICATE-MODULE warning."
  (declare-module modname intf nil))


(defun module-declared-p (modname)
  "Test whether MODNAME is declared as a module in the current environment."
  (in-global-environment
    (let ((ty (get-type modname)))
      (and (not (null ty))
	   (subtype-p ty 'module)))))


(defun get-module (modname)
  "Return the module code for MODNAME.

This will typically have been set by DEFMODULE/VL and so will have been
type-checked, macro-expanded, and had other passes applied."
  (if (module-declared-p modname)
      (in-global-environment
	(get-initial-value modname))

      (error 'unknown-module :module modname
			     :hint "Make sure the module has been declared")))


(defun get-module-interface (modname)
  "Return the module interface for MODNAME.

This will typically have been set by DEFMODULE/VL and so will have a
corresponding module body to be synthesised."
  (if (module-declared-p modname)
      (in-global-environment
	(get-type modname))

      (error 'unknown-module :module modname
			     :hint "Make sure the module has been declared")))


(defun get-modules-for-synthesis ()
  "Return an alist consisting of module names and their declarations."
  (declare (optimize debug))

  (in-global-environment
    (let ((env (filter-environment (lambda (n env)
				     (and (module-declared-p n)
					  (not (null (get-initial-value n :default nil)))))
				   (current-frame))))

      (mapcar (lambda (n)
		(list n
		      (get-module n)))
	      (get-environment-names env)))))


;;; ---------- Module declaration ----------

(defun expand/vl (form)
  "Compiler pass to expand FORM into core Verilisp.

This performs macro expansion and frame application, returning
the expanded, framed, transformed, analysed form.

This function is not usually called directly, but is called as part
of a larger compilation process."
  (declare (optimize debug))

  ;; expand macros and apply frames
  (let* ((expanded (expand-macros-in-environment form))
	 (framed (add-frames (copy-tree expanded))))

    ;; compute the dependencies between variables
    (compute-dependencies framed)

    ;; infer representations on the tree
    (infer-representation framed)

    framed))


(defun typecheck/vl (form)
  "Type-check and infer types in FORM.

Returns the overall type of FORM, which will typically be a module.

This function is not usually called directly, but is called as part
of a larger compilation process."
  (typecheck form))


(defun elaborate/vl (form)
  "Elaborate FORM as a module.

FORM should be a program in core Verilisp, with macros expanded
and frames applied, as done by EXPAND/VL.

This function runs all the relevant compiler nanopasses, returning a
list consisting of the module interface type and the fully-elaborated
module ready for synthesis.

This function is not usually called directly, but is called as part
of a larger compilation process."
  (declare (optimize debug))
q
  ;; simplify
  (let* ((transformed (elaborate-state-machines form))
	 (floated (car (float-let-blocks transformed)))
	 (simplified (simplify-progn floated)))

    simplified))


(defmacro defmodule/vl (modname decls &body body)
  "Declare a module MODNAME with given DECLS and BODY.

The module is loaded, annotated, macro-expanded, type-checked
(meaning that it will be at least minimally syntactically correct
afterwards -- although possibly still not finally synthesisable),
and then be processed ready for synthesis (which might cause further
warnings or errors).

The resulting fully-elaborated module is added to *GLOBAL-ENVIRONMENT*
and is avalable for importing and synthesis.

Declaring a module again will cause a DUPLICATE-MODULE warning, and the
old module will be overwritten.

Return the name of the newly-defined module."
  (declare (optimize debug))

  (with-gensyms (module expanded intf elaborated)
    (let ((code `(module ,modname ,decls
			 ,@body)))
      `(let* ((,module ',code)
	      (,expanded (expand/vl ,module))
	      (,intf (typecheck/vl ,expanded))
	      (,elaborated (elaborate/vl ,expanded))
	      )

	 ;; declare the module
	 (declare-module ',modname ,intf ,elaborated)

	 ',modname))))


(defmacro defmoduleinterface/vl (modname decls declarations)
  "Define a module MODNAME with the given argument DECLS.

Defining just interfaces allows modules written in Verilog to be used
within Verilisp. This lets Verilisp programs use existing IP.

Verilisp does a lot more work in analysing code than Verilog, however.
DECLARATIONS should be a DECLARE form providing information about the
DECLS, typically types and directions of arguments (which Verilisp
would normally infer from the body of the module).

Module interfaces are not scheuled for synthesis, and should be included
into the final bitstream from within the FPGA toolchain."
  ;; make sure we have a DECLARE form
  (with-current-form declarations
    (unless (and (listp declarations)
		 (eql (car declarations) 'declare))
      (error 'syntax-error :hint "Use a DECLARE form as the module body")))

  (with-gensyms (module expanded intf)
    (let ((code `(module ,modname ,decls
			 ,declarations)))
      `(let* ((,module ',code)
	      (,expanded (add-frames ,module))
	      (,intf (typecheck/vl ,expanded)))

	 ;; check that we have all the necessary information
	 ;; TBD

	 ;; declare the imported module
	 (declare-imported-module ',modname ,intf)

	 ',modname))))


;;; ---------- Forgetting ----------

(defun forget/vl (name)
  "Forget the macro or module NAME from the global environment."
  (in-global-environment
    (forget-variable name)))


;;; ---------- Module synthesis ----------

(defun synthesise/vl (m str)
  "Synthesise module M to STR.

M can be an elaborated Verilisp form or a symbol identifying
a loaded module."
  (with-synthesis-to-stream str
    (let ((vl (if (symbolp m)
		  (get-module m)
		  m)))
      (synthesise vl))))
