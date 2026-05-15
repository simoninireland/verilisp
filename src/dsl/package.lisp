;;;; Package for DSL builder
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

(in-package :common-lisp-user)

;;; The DSL builder is a small set of data structures, functions, and macros
;;; to simplify the construction of DSLs: it's not specific to Verilisp.
;;;
;;; The builder build passes, which process a program by structural
;;; induction. Each pass consists of generic functions that recursively
;;; process the code passed in. There are macros for creating passes,
;;; and some recursion schemata that capture the common recursive
;;; combinators for processing forms. (New schemata can be defined too.)
;;; Passes can be structured into queues that can be run as chains.
;;;
;;; The builder maintains a queue of the forms being processed, letting
;;; a pass method identify where it it in the larger scheme of the code.

(defpackage verilisp/dsl
  (:documentation "The Verilisp DSL builder")
  (:use :cl :alexandria :verilisp/utils)
  (:import-from :str
		#:concat
		#:upcase)

  (:export
   ;; form queue
   #:current-form
   #:form-head
   #:current-form-head
   #:containing-form
   #:containing-containing-form
   #:in-context-p
   #:with-current-form
   #:with-current-form-queue

   ;; pass queues
   #:define-pass-queue
   #:pass-queue-names
   #:pass-queue-p
   #:ensure-pass-queue
   #:run-pass-queue

   ;; recursion schemata
   #:define-recursion-schema
   #:recursion-schemata
   #:recursion-schema-p
   #:ensure-recursion-schema

   ;; standard schemata
   #:fail-unknown-form
   #:constant-form
   #:into-arguments
   #:over-arguments
   #:into-function-and-arguments
   #:into-arguments-all-non-nil
   #:into-arguments-union

   ;; pass definition macros
   #:defpass
   #:defpassmethod
   #:pass-names
   #:pass-p
   #:ensure-pass

   ;; recovery
   #:with-recover-on-error
   #:recover

   ;; conditions
   #:dsl-error
   #:unknown-form))
