;; Language conditions
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


;; ---------- Language base condition ----------

(define-condition vl-condition ()
  ((hint
    :documentation "A hint as to how to fix the condition."
    :initarg :hint
    :initform nil
    :reader hint)
   (underlying-condition
    :documentation "Any underlying condition that was converted to this.

This allows VL-BASE-CONDITION to be used to mask other, typically
implementation-specific, conditions encountered during processing."
    :initform nil
    :initarg :underlying-condition
    :reader underlying-condition)
   (fragment
    :documentation "The code giving rise to the condition.

This is extracted automatically from the current function
form queue, or can be provided explicitly using the :FRAGMENT key."
    :initform (current-form)
    :initarg :fragment
    :reader fragment))
  (:documentation "Mixin for Verilisp language conditions.

The fragment is the code that gave rise to the condition, and is used
to add contxt to the report. A hint can be given to suggest how to fix
the issue."))


(defgeneric format-condition-context (detail c str)
  (:documentation "Format the DETAIL and other information of a condition C.

The text is written to stream STR."))


(defvar *maximum-code-fragment-length* 60
  "Length of code fragment to be reported in conditions.

This only changes the printed length: the entire fragment is retained.")


(defmethod format-condition-context (detail (c vl-condition) str)
  (format str "~a" detail)

  ;; add hint if present
  (if-let ((hint (hint c)))
    (format str " (~a)" hint))

  ;; add context if known
  (if-let ((code (fragment c)))
    (format str " Context: ~a" (shorten *maximum-code-fragment-length*
					(format nil "~a" code)
					:ellipsis "..."))))


(define-condition vl-warning (vl-condition warning)
  ()
  (:documentation "Base condition for warnings.

Warnings can be ignored by simply returning from their handler."))


(define-condition vl-error (vl-condition error)
  ()
  (:documentation "Base condition for errors.

Errors cannot be ignored like warnings, However, they will typically be
signalled from a context that exports a RECOVER restart to re-start
processing from a \"safe\" point. This lets processing continuue, but
might still cause a cascade of further errors. use WITH-RECOVER-ON-ERROR
to set up recovery actions."))


;; ---------- Synthesis ----------

(define-condition not-synthesisable (vl-error)
  ()
  (:report (lambda (c str)
	     (format-condition-context (format nil "Could not synthesise code (~s)"
					       (underlying-condition c))
				       c str)))
  (:documentation "Condition signalled when code can't be synthesised.

This usually indicates that the Lisp fragment provided is un-parsable because
it contains non-synthesisable syntax, or because of a problem with the
synthesiser's code generator."))


(define-condition not-representable (vl-error)
  ((ty
    :documentation "The type that can't be represented."
    :initarg :type
    :reader unrepresentable-type))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Cannot represent value of type ~a"
					       (unrepresentable-type c))
				       c str)))
  (:documentation "Condition signalled when a value can't be represented.

This is usually caused by unsatisfiable type constraints arising from
variable assignments."))


;; ---------- Checking ----------

(define-condition unknown-variable (vl-error)
  ((var
    :documentation "The variable(s)."
    :initarg :variable
    :initarg :variables
    :reader variables))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Unknown variable(s) ~a"
					       (variables c))
				       c str)))
  (:documentation "Condition signalled when one or more undeclared variables are encountered.

This is caused either by an undeclared variable or by use of a variable
that should be declared in the architectural environment, such as a register."))


(define-condition unknown-form (vl-error)
  ((form
    :documentation "The form."
    :initarg :form
    :reader form))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Unknown form ~a"
					       (form c))
				       c str)))
  (:documentation "Condition signalled when an unknown form is encountered.

This is usually caused by using a Lisp function that is not supported
by Verilisp, or an undefined macro."))


(define-condition syntax-error (vl-error)
  ((form
    :documentation "The form."
    :initarg :form
    :reader form))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Syntax error in ~a"
					       (form c))
				       c str)))
  (:documentation "Condition signalled when a syntax error is encountered.

Syntax errors in Verilisp are forms that are known, but that appear in
a context than can't be synthesised -- for example a GO outside a
TAGBODY."))


(define-condition unknown-module (vl-error)
  ((modname
    :documentation "The module."
    :initarg :module
    :reader module))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Unknown module ~a"
					       (module c))
				       c str)))
  (:documentation "Condition signalled when an unknown module is imported.

This means that the required module isn't available, either having not yet
been defined or not having been imported."))


(define-condition module-mismatch (vl-error)
  ((modname
    :documentation "The module."
    :initarg :module
    :reader module))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Can't instanciate ~s"
					       (module c))
				       c str)))
  (:documentation "Condition signalled when a module can't be instanciated.

This means there's a parameter mismatch in terms of unrecognised arguments
or duplicates. It isn't signalled for type errors."))


(define-condition unknown-state (vl-error)
  ((label
    :documentation "The state label."
    :initarg :label
    :reader label))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Unknown state label ~a"
					       (label c))
				       c str)))
  (:documentation "Condition signalled when an unknown state label is used as a GO target.

Only state labels that have been explicitly set up in a TAGBODY can be
used as targets for GO forms."))


(define-condition duplicate-variable (vl-error)
  ((var
    :documentation "The variable(s)."
    :initarg :variable
    :initarg :variables
    :reader variables))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Duplicate variable(s) ~a"
					       (variables c))
				       c str)))
  (:documentation "Condition signalled when a variable is re-defined in the same scope."))


(define-condition duplicate-macro (vl-warning)
  ((var
    :documentation "The macro."
    :initarg :name
    :reader name))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Re-defining macro ~a"
					       (name c))
				       c str)))
  (:documentation "Condition signalled when a macro is re-defined.

This is acceptable, and is just a warning in case it's not intended."))


(define-condition duplicate-module (vl-warning)
  ((modname
    :documentation "The module."
    :initarg :module
    :reader module))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Duplicate module ~a"
					       (module c))
				       c str)))
  (:documentation "Condition signalled when a module is re-defined."))


(define-condition duplicate-state (vl-error)
  ((states
    :documentation "The state labels."
    :initarg :states
    :reader states))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Duplicate state label in ~a"
					       (states c))
				       c str)))
  (:documentation "Condition signalled when state labels are not unique.

State labels need to be unique within a single state machine (TAGBODY form).
They do not need to be globally unique."))


(define-condition not-importable (vl-error)
  ((modname
    :documentation "The module."
    :initarg :module
    :reader module))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Module ~a can't be imported"
					       (module c))
				       c str)))
  (:documentation "Condition signalled when a module can't be imported.

This is usually caused by mismatched arguments, either an unknown argument
being provided in the import or one that's needed not being provided."))


(define-condition not-static (vl-error)
  ()
  (:report (lambda (c str)
	     (format-condition-context "Expression is not static" c str)))
  (:documentation "Condition signalled when an expression isn't static.

Some parts of a program need to be known at compile-time, and so can't
include terms that are only known at run-time."))


(define-condition value-mismatch (vl-error)
  ((expected
    :documentation "The values allowed."
    :initarg :expected
    :reader expected-values)
   (received
    :documentation "The value received."
    :initarg :got
    :reader received-value))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Expected a value that is one of ~s, got ~s"
					       (expected-values c)
					       (received-value c))
				       c str)))
  (:documentation "Condition signalled when a mis-matched value is received."))


(define-condition direction-mismatch (vl-warning)
  ((variable
    :documentation "The variable."
    :initarg :variable
    :reader variables)
   (expected
    :documentation "The direction(s) allowed."
    :initarg :expected
    :reader expected-values)
   (received
    :documentation "The direction received."
    :initarg :got
    :reader received-value))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Expected a direction  ~a for ~a, got ~a"
					       (expected-values c)
					       (variables c)
					       (received-value c))
				       c str)))
  (:documentation "Condition signalled when a mis-matched direction is received.

This is signalled when an explicit direction is declared for a module
parameter that is inconsistent with the one Verilisp infers. This is
almost certainly a logical error."))


(define-condition type-mismatch (vl-warning)
  ((expected
    :documentation "The expected type."
    :initarg :expected
    :initform "unknown"
    :reader expected-type)
   (received
    :documentation "The received type."
    :initarg :got
    :initform "unknown"
    :reader received-type))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Expected a value of type ~a, got one of type ~a"
					       (expected-type c)
					       (received-type c))
				       c str)))
  (:documentation "Condition signalled when types don't match.

The most common case is making an assignment of an expression to a
variable that is too narrow to accommodate all its possible values.
This is usually signalled as a warning, as there is a
sometimes-acceptable default action to risk the loss of precision
caused by the assignment."))


(define-condition coercion-mismatch (vl-warning)
  ((expected
    :documentation "The type we tried to coerce to."
    :initarg :expected
    :reader expected-type)
   (received
    :documentation "The received type."
    :initarg :got
    :reader received-type))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Can't coerce a value of type ~a to one of type ~a"
					       (received-type c)
					       (expected-type c))
				       c str)))
  (:documentation "Condition signalled when a coercion can't happen.

Coercion only currently works between fixed-width types."))


(define-condition precision-mismatch (vl-warning)
  ((received
    :documentation "The type we received"
    :initarg :received
    :reader received-type)
   (want
    :documentation "The type we want."
    :initarg :want
    :reader wanted-type))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Potential loss of precision coercing ~a to ~a"
					       (received-type c)
					       (wanted-type c))
				       c str)))
  (:documentation "Condition signalled when a coercion may cause loss of precision.

This happens when a value is coerced to a type with too few bits to represent
it faithfully."))


(define-condition bitfield-mismatch (vl-warning)
  ((pattern
    :documentation "The bitfield pattern."
    :initarg :pattern
    :reader pattern))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Can't match bitfield pattern ~a"
					       (pattern c))
				       c str)))
  (:documentation "Condition signalled when a bitfield pattern can't be parsed.

This is usually caused by non-consecutive uses of variables in the pattern."))


(define-condition shape-mismatch (vl-error)
  ((shape
    :documentation "The expected shape."
    :initarg :expected
    :reader shape))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Data doesn't match shape ~a"
					       (shape c))
				       c str)))
  (:documentation "Condition signalled when data has the wrong shape.

This means that the initial contents of an array are not in the right
shape for the array. Usually this can be fixed by simply reformatting
the data, and/or making sure there's the right amount of it."))


(define-condition state-machine-mismatch (vl-error)
  ((state
    :documentation "The state."
    :initarg :state
    :reader state))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Unrecognised state ~a"
					       (state c))
				       c str)))
  (:documentation "Condition signalled when an unrecognised state is encountered.

This usualy happens when a state is targeted as the next state (using the
NEXT macro) that isn't defined in the surrounding state machine."))


(define-condition unreachable-code (vl-warning)
  ()
  (:report (lambda (c str)
	     (format-condition-context "Uneachable code" c str)))
  (:documentation "Condition signalled when some code is unreachable.

This usually happens in state machines, where a GO form is followed
by code in the same state, whcih can't then be reached. This may be
a mistake in the logic of the program, requiring the unreachable code
to be moved before the GO, or to be placed in a new state that then
can be reached."))


(define-condition unrecognised-declaration (vl-warning)
  ((tag
    :documentation "The declared annotation tag."
    :initarg :tag
    :reader tag))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Unrecognised declaration annotation ~a"
					       (tag c))
				       c str)))
  (:documentation "Condition signalled when an unrecognised annotationis encountered.

This is often benign, indicating that an annotation has appeared
in a DECLARE form that isn't (yet) handled by Verilisp."))


(define-condition type-inferred (vl-warning)
  ((var
    :documentation "The variable."
    :initarg :variable
    :reader inferred-variable)
   (inferred
    :documentation "The type inferred."
    :initarg :inferred
    :reader inferred-value))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Inferred ~a to have type ~a"
					       (inferred-variable c)
					       (inferred-value c))
				       c str)))
  (:documentation "Condition signalled when a type is inferred.

This is almost always a warning, signalled when the compiler infers the
type of a variable that doesn't have an explicit type provided. If may
cause downstream errors if the inferred type is incorrect, but that will
only happen when the types are being used inconsistently."))


(define-condition state-machine-inferred (vl-warning)
  ((given
    :documentation "The number of states in the machine as specified."
    :initarg :given
    :reader given-states)
   (inferred
    :documentation "The number of states inferred."
    :initarg :inferred
    :reader inferred-states))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Inferred state machine has ~a states (~a in original code)"
					       (inferred-states c)
					       (given-states c))
				       c str)))
  (:documentation "Condition signalled when a state machine is inferred.

This is a condition that doesn't necessarily indicte a problem, but announces
how many states have been inferred for a state machine. This will never be less
than the number in the code, but will frequently be more due to constructs
introduced by loops, dependencies, and so on. This may be important where
the timing of the final system is critical, since each state requires its own
clock cycle."))


(define-condition unused-variable (vl-warning)
  ((var
    :documentation "The variable."
    :initarg :variable
    :reader unused-variable))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Variable ~a was declared but never used"
					       (unused-variable c))
				       c str)))
  (:documentation "Condition signalled when a variable is declared and not used.

This is probably a user mistake. It does no harm, but can result in
wasted silicon."))


(define-condition used-variable (vl-warning)
  ((var
    :documentation "The variable."
    :initarg :variable
    :reader used-variable))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Variable ~a was declared to be 'ignored' but was then used"
					       (used-variable c))
				       c str)))
  (:documentation "Condition signalled when a variable is annoataed to be ignored and is then used.

This is probably a user mistake. It does no harm, ut might indicate that there's something unexpected in the code."))


(define-condition representation-mismatch (vl-warning)
  ((variable
    :documentation "The variable."
    :initarg :variable
    :reader variables)
   (expected
    :documentation "The representation expected."
    :initarg :expected
    :reader expected-values)
   (received
    :documentation "The representation received."
    :initarg :got
    :reader received-value))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Expected a representation ~a for ~a, got ~a"
					       (expected-values c)
					       (variables c)
					       (received-value c))
				       c str)))
  (:documentation "Condition signalled when representations are mis-matched.

This can happen when a representation is provided for a variable that's
incompatible with how its used. It also happens when using the LET-WIRES,
LET-REGISTERS, and LET-CONSTANTS macros and providing a representation
explicitly that conflicts with the one implied by the macro."))


;; ---------- Internal errors ----------

;; These are errors arising from the incorrect functioning of the
;; Verilisp DSL itself, rather than from user code.

(define-condition no-local-frame (vl-error)
  ()
  (:report (lambda (c str)
	     (format-condition-context (format nil "No local frame")
				       c str)))
  (:documentation "Condition signalled when there is no local frame.

This indicates a coding error within Verilisp, whcih attempted to retrieve
a locally-applied frame from a construct that didn;t have one."))
