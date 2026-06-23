;;;; System definitions
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

;;; ---------- Verilisp ----------

(asdf:defsystem "verilisp"
  :description "A very Lisp approach to hardware synthesis."
  :author "Simon Dobson <simoninireland@gmail.com"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria"
	       "str"
	       "trivia"
	       "cl-ppcre"
	       "closer-mop"
	       "slot-extra-options"
	       "read-number")
  :pathname "src/"
  :serial t
  :components (;; utilities
	       (:module "utils"
		:components ((:file "package")
			     (:file "list-utils")
			     (:file "string-utils")
			     (:file "maths-utils")))

	       ;; DSL builder
	       (:module "dsl"
		:components ((:file "package")
			     (:file "conditions")
			     (:file "form-queue")
			     (:file "pass-queue")
			     (:file "recursion-schemata")
			     (:file "dsl")
			     (:file "types")))

	       ;; core language
	       (:module "vl"
		:components ((:file "package")
			     (:file "conditions")
			     (:file "env")
			     (:file "context")
			     (:file "helpers")
			     (:file "pretty-printer")
			     (:file "passes")
			     (:file "typeops")
			     (:file "identifiers")
			     (:file "macros")
			     (:file "fixed-width")
			     (:file "literals")
			     (:file "references")
			     (:file "binders")
			     (:file "declare")
			     (:file "control-flow")
			     (:file "conditionals")
			     (:file "assignment")
			     (:file "iteration")
			     (:file "operators")
			     (:file "shortcuts")
			     (:file "comparisons")
			     (:file "bits")
			     (:file "make-bitfields")
			     (:file "with-bitfields")
			     (:file "arrays")
			     (:file "casting")
			     (:file "modules")
			     (:file "state-machine")
			     (:file "eval")
			     (:file "loader")
			     (:file "embedding")))

	       ;; public package
	       (:file "package"))
  :in-order-to ((test-op (test-op "verilisp/test"))))


(asdf:defsystem "verilisp/test"
  :description "Global tests of Verilisp."
  :depends-on ("alexandria" "verilisp" "fiveam")
  :pathname "test/"
  :serial t
  :components ((:file "package")
	       (:file "test-utils")
	       (:file "test-dsl")
	       (:file "test-vl-base")
	       (:file "test-vl-types")
	       (:file "test-vl-assignment")
	       (:file "test-vl-literals")
	       (:file "test-vl-references")
	       (:file "test-vl-bits")
	       (:file "test-vl-make-bitfields")
	       (:file "test-vl-arrays")
	       (:file "test-vl-operators")
	       (:file "test-vl-comparisons")
	       (:file "test-vl-casting")
	       (:file "test-vl-binders")
	       (:file "test-vl-control-flow")
	       (:file "test-vl-conditionals")
	       (:file "test-vl-modules")
	       (:file "test-vl-eval")
	       (:file "test-vl-macros")
	       (:file "test-vl-float-let")
	       (:file "test-vl-rewriting")
	       (:file "test-vl-cond")
	       (:file "test-vl-with-bitfields")
	       (:file "test-vl-helpers")
	       (:file "test-vl-state-machine"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))


;;; ---------- Transpiler CLI ----------

(asdf:defsystem "verilisp/cli"
  :description "Command line tool to transpile Verilisp to Verilog."
  :author "Simon Dobson <simoninireland@gmail.com"
  :license "GPL3"
  :depends-on ("alexandria" "unix-opts" "local-time" "verilisp")
  :pathname "src/cli/"
  :serial t
  :components ((:file "package")
	       (:file "verilispc"))
  :build-operation "program-op"
  :build-pathname "../../bin/verilispc"
  :entry-point "verilisp/cli:main")
