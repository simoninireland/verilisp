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
   ;; fixed-width types
   #:unsigned-byte
   #:signed-byte
   #:bitwidth
   #:fixed-width-p
   #:unsigned-byte-p
   #:signed-byte-p

   ;; environments
   #:*global-environment*
   #:empty-environment
   #:add-frame
   #:with-new-frame
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
   #:state-machine
   #:next
   #:exit

   ;; DSL functions
   #:annotate
   #:typecheck
   #:free-variables
   #:rewrite-variables
   #:float-let-blocks
   #:simplify-progn
   #:expand-macros-in-environment
   #:synthesise
   #:lispify

   ;; loader
   #:clear-module-registry
   #:get-module
   #:get-module-interface
   #:get-modules-for-synthesis
   #:defmodule
   #:elaborate-module
   #:synthesise-module

   ;; conditions
   #:vl-condition
   #:vl-error
   #:vl-warning
   #:recover
   #:not-synthesisable
   #:unknown-variable
   #:unknown-module
   #:unknown-form
   #:duplicate-variable
   #:duplicate-module
   #:not-importable
   #:not-static
   #:value-mismatch
   #:direction-mismatch
   #:type-mismatch
   #:coercion-mismatch
   #:bitfield-mismatch
   #:shape-mismatch
   #:state-machine-mismatch
   #:type-inferred
   #:representation-mismatch
   ))
