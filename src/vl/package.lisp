;; Package for Verilisp core
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

(in-package :common-lisp-user)

(defpackage verilisp/core
  (:documentation "The verilisp core language")
  (:use :cl :alexandria :verilisp/utils)
  (:import-from :cl-ppcre
		#:create-scanner
		#:scan
		#:regex-replace-all)
  (:import-from :str
		#:concat
		#:s-first
		#:containsp
		#:shorten
		#:words)

  (:export
   ;; types
   #:bitwidth
   #:construct-type
   #:deconstruct-type
   #:subtype-p
   #:subtype-type
   #:representable-type-p
   #:fixed-width-p
   #:unsigned-byte-p
   #:signed-byte-p
   #:lub
   #:lurb
   #:lub-type

   ;; environments
   #:*global-environment*
   #:current-frame
   #:empty-environment
   #:add-frames
   #:in-frame
   #:in-core-environment
   #:in-global-environment
   #:with-frame
   #:with-new-frame
   #:with-local-frame
   #:declare-variable
   #:variable-declared-p
   #:get-frame-names
   #:get-environment-names
   #:filter-environment
   #:get-type
   #:get-representation
   #:get-width
   #:get-initial-value

   ;; evaluating expressions in environments
   #:close-form-in-environment
   #:close-form-in-static-environment
   #:eval-in-static-environment
   #:ensure-static
   #:eval-if-static

   ;; extra Verilisp functions, macros, values, and annotations
   ;; not present in Common Lisp
   #:module
   #:module-interface
   #:<<
   #:>>
   #:asserted-p
   #:@
   #:posedge
   #:negedge
   #:bref
   #:if-let-bitfields
   #:with-bitfields
   #:make-bitfields
   #:extend-bits
   #:register
   #:wire
   #:constant
   #:parameter
   #:width
   #:as
   #:type-constraints
   #:initial-value
   #:depends-on
   #:direction
   #:in
   #:out
   #:inout
   #:read
   #:written
   #:0=
   #:0/=
   #:1+
   #:1-
   #:2*
   #:2/
   #:while
   #:until
   #:forever

   ;; passes and code functions
   #:add-frames
   #:add-local-frame-to-decls
   #:get-local-frame
   #:expand-macros
   #:expand-macros-in-environment
   #:declare-annotation
   #:compute-dependencies
   #:add-dependencies
   #:traverse-dependencies
   #:infer-representation
   #:compute-type
   #:apply-type-constraints
   #:typecheck
   #:read-variables
   #:read-variables-setf
   #:written-variables-setf
   #:mark-variable-as-read
   #:mark-variable-as-written
   #:variable-read-p
   #:variable-written-p
   #:rewrite-variables
   #:generalised-place-p
   #:transform
   #:float-let-blocks
   #:simplify-progn
   #:synthesise
   #:lispify

   ;; loader
   #:clear-global-environment
   #:get-module
   #:get-module-interface
   #:get-modules-for-synthesis
   #:defmodule/vl
   #:importmodule/vl
   #:defmacro/vl
   #:macrolet/vl
   #:expand/vl
   #:typecheck/vl
   #:elaborate/vl
   #:synthesise/vl

   ;; base conditions
   #:vl-condition
   #:vl-error
   #:vl-warning

   ;; restarts
   #:recover

   ;; errors
   #:not-synthesisable
   #:not-representable
   #:unknown-variable
   #:unknown-form
   #:syntax-error
   #:unknown-module
   #:module-mismatch
   #:unknown-state
   #:duplicate-variable
   #:duplicate-state
   #:not-importable
   #:not-static
   #:value-mismatch
   #:shape-mismatch
   #:no-local-frame

   ;; warnings
   #:unused-variable
   #:used-variable
   #:duplicate-module
   #:duplicate-macro
   #:direction-mismatch
   #:type-mismatch
   #:coercion-mismatch
   #:precision-mismatch
   #:bitfield-mismatch
   #:unreachable-code
   #:unrecognised-declaration
   #:type-inferred
   #:resources-created
   #:representation-mismatch
   ))
