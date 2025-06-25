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
  (:nicknames :vl)
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
   #:subtype-p
   #:subtype-type
   #:fixed-width-p
   #:unsigned-byte-p
   #:signed-byte-p
   #:lub
   #:lub-type

   ;; environments
   #:*global-environment*
   #:empty-environment
   #:add-frames
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

   ;; extra Verilisp functions and macros not in Common Lisp
   #:module
   #:<<
   #:>>
   #:asserted-p
   #:@
   #:posedge
   #:negedge
   #:bref
   #:with-bitfields
   #:make-bitfields
   #:extend-bits
   #:0=
   #:0/=
   #:2*
   #:let-wires
   #:let-registers
   #:let-constants

   ;; DSL functions
   #:add-frames
   #:typecheck
   #:free-variables
   #:rewrite-variables
   #:float-let-blocks
   #:simplify-progn
   #:expand-macros-in-environment
   #:simplify
   #:synthesise
   #:lispify

   ;; loader
   #:clear-module-registry
   #:get-module
   #:get-module-interface
   #:get-modules-for-synthesis
   #:defmodule/vl
   #:expand/vl
   #:elaborate/vl
   #:synthesise/vl

   ;; conditions
   #:vl-condition
   #:vl-error
   #:vl-warning
   #:recover
   #:not-synthesisable
   #:syntax-error
   #:unknown-variable
   #:unknown-module
   #:unknown-state
   #:unknown-form
   #:duplicate-variable
   #:duplicate-module
   #:duplicate-state
   #:not-importable
   #:not-static
   #:value-mismatch
   #:direction-mismatch
   #:type-mismatch
   #:coercion-mismatch
   #:precision-mismatch
   #:bitfield-mismatch
   #:shape-mismatch
   #:state-machine-mismatch
   #:type-inferred
   #:representation-mismatch
   #:no-local-frame
   ))
