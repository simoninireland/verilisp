;; User-level package definition
;;
;; Copyright (C) 2023--2025 Simon Dobson
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

(defpackage verilisp
  (:use :cl :alexandria :verilisp/core)
  (:nicknames :vl)
  (:export
   ;; extra Verilisp functions, macros, values, and
   ;; annotations not present in Common Lisp
   #:module
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
   #:width
   #:as
   #:direction
   #:in
   #:out
   #:inout
   #:0=
   #:0/=
   #:2*
   #:while
   #:until
   #:forever

   ;; top-level interface
   #:defmodule/vl
   #:defmoduleinterface/vl
   #:defmacro/vl
   #:macrolet/vl
   #:synthesise/vl
   #:forget/vl
   #:clear-global-environment
   #:get-module
   #:get-module-interface
   #:get-modules-for-synthesis

   ;; conditions
   #:vl-condition
   #:vl-error
   #:vl-warning
   #:recover
   #:not-synthesisable
   #:not-representable
   #:syntax-error
   #:unknown-variable
   #:unknown-module
   #:unknown-state
   #:unknown-form
   #:unrecognised-declaration
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
   #:resources-created
   #:type-inferred
   #:representation-mismatch
   #:no-local-frame))
