;; Variables for fields in a word
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


(defun extract-runs (pattern)
  "Extract consecutive runs of a symbol from PATTERN.

A run consists of either a sequence of one or more instances of the
same variable name, 0 or 1 bits, or the - ignore bit; or a list
consisting of a variable name, 0 or 1 bits, or the - ignore bit,
followed by the length of the run.

Return an alist mapping symbol to start and end positions,
with the rightmost position being 0."
  (declare (optimize debug))

  (labels ((extract (pat i currentrun)
	     (let ((s (car pat)))
	       (cond ((< i 0)
		      ;; end of pattern, done
		      (list currentrun))

		     ((listp s)
		      ;; new run with explicit length
		      (destructuring-bind (var len)
			  s
			(let ((end (- i len)))
			  (if (null currentrun)
			      ;; first run
			      (extract (cdr pat)
				       end
				       (list var i (1+ end)))

			      ;; new run
			      (cons currentrun
				    (extract (cdr pat)
					     end
					     (list var i (1+ end))))))))

		     ((null currentrun)
		      ;; first character, create first run
		      (extract (cdr pat)
			       (1- i)
			       (list s i i)))

		     ((equal s (car currentrun))
		      ;; same character, continuing current run
		      (extract (cdr pat)
			       (1- i)
			       (list s (cadr currentrun) i)))

		     (t
		      ;; different character, starting a new run
		      (cons currentrun
			    (extract (cdr pat)
				     (1- i)
				     (list s i i)))))))

	   (pattern-length (pat)
	     (foldr (lambda (l s)
		      (if (listp s)
			  (destructuring-bind (var len)
			      s
			    (+ l len))
			  (1+ l)))
		    pat 0)))

    (if (null pattern)
	nil

	(handler-bind
	    ((error (lambda (c)
		      (error 'syntax-error :hint "Explicit-width fields should consist of a symbol and a width "))))
	  (extract pattern (1- (pattern-length pattern)) nil)))))


(defun duplicate-keys-p (l)
  "Test whether alist L has duplicate keys."
  (if (null l)
      nil

      (let ((k (caar l)))
	(if (assoc k (cdr l))
	    t
	    (duplicate-keys-p (cdr l))))))


(defun extract-bitfields (pattern)
  "Extract bitfields from PATTERN.

Return an alist from variable name to bits (as a list).
A MISMATCHED-BITFIELD error is signalled if the same
variable appears in non-consecutive places.

The bitfields also include any 0, 1, and - symbols."
  (let ((runs (extract-runs pattern)))
    ;; check for duplicate symols /except/ 0,1, and -
    (let ((bindings (remove-if (lambda (s)
				 (member (car s) '(0 1 -)))
			       runs)))
      (when (duplicate-keys-p bindings)
	(error 'bitfield-mismatch :pattern pattern
				  :hint "Bits going into a variable have to be consecutive"))

      runs)))


(defun bitfield-fixed-bit-p (b)
  "Test whether B is a fixed bit.

Fixed bits are constant 0s or 1s."
  (member b '(0 1)))


(defun bitfield-fixed-bit-or-ignored-p (b)
  "Test whether B is a fixed bit or being ignored."
  (or (bitfield-fixed-bit-p b)
      (eql b '-)))


(defun bitfield-fixed-bit-runs (runs)
  "Extract fixed-bit runs from RUNS."
  (remove-if (lambda (run)
	       (not (bitfield-fixed-bit-p (car run))))
	     runs))


(defun bitfield-variable-runs (runs)
  "Extract variable (non-fixed-bit) runs from RUNS."
  (remove-if (lambda (run)
	       (bitfield-fixed-bit-or-ignored-p (car run)))
	     runs))


(defun run-to-decl (arg run)
  "Return the Verilisp declaration correponding to RUN over ARG."
  (destructuring-bind (s start end)
      run
    (if (= start end)
	;; single bit
	`(,s (bref ,arg ,start))

	;; several bits
	`(,s (bref ,arg ,start :end ,end)))))


(defun run-to-test (arg run)
  "Return a test of the bits of RUN against ARG."
  (destructuring-bind (s start end)
      run
    (if (= start end)
	;; single bit
	`(= (bref ,arg ,start) ,s)

	;; several bits
	(if (= s 0)
	    ;; zero bit
	    `(= (bref ,arg ,start :end ,end) 0)

	    ;; one bit
	    (let ((val (1- (ash 1 (1+ (- start end))))))
	      `(= (bref ,arg ,start :end ,end) ,val))))))


(defcoremacro/vl if-let-bitfields (pattern arg &body body)
  "Create variables matching the bitfield PATTERN applied to ARG.

The pattern consists of a list of variable names, with each
entry corresponding to a bit position. The rightmost bit is
bit zero, then bit one to its left, and so forth. If several
consecutive positions have the same variable name, the variable
takes multiple bits.

The pattern may also contain 0, 1, and - for literal 0, 1, and
ignored bits.

For example:

(if-let-bitfields (o o o a a a a 0)
    opcode

  ;; executed when patten matches
  (setq reg a)

  ;; executed when pattern doesn't match
  (setq reg 0)))

would declare variables o and a in the body, with the values extracted
from bits 7 to 5 and 4 to 1 respectively, and the lowest-order bit
being 0.

If the bitfields can be matched, the first form of BODY is
evaluated with all the variables declared in the pattern in scope and
bound to the appropriate values from the bitfield; if not, the rest of
forms in BODY is evaluated with none of the variables declared.

Variables are declared as generalised places, meaning that calls to
SETF will update the appropriate positons in ARG."

  ;; catch the common error of forgetting the value
  ;; to match against with a one-form body
  (when (< (length body) 1)
    (error 'syntax-error :form arg
			 :hint "No value to match against?"))

  ;; extract else branch if body is longer than one form
  (let* ((then-branch (car body))
	 (else-branch (if (> (length body) 1)
			  (cdr body))))

    (with-gensyms (condition)
      (let* ((runs (extract-bitfields pattern))
	     (fixed-bit-runs (bitfield-fixed-bit-runs runs))
	     (variable-runs (bitfield-variable-runs runs))

	     (tests (if fixed-bit-runs
			(mapcar (curry #'run-to-test condition) fixed-bit-runs)))
	     (decls (if variable-runs
			(mapcar (curry #'run-to-decl arg) variable-runs))))

	(if tests
	    (let ((test (if (= (length tests) 1)
			    (car tests)
			    `(and ,@tests))))

	      (if decls
		  ;; tests and declarations
		  (if else-branch
		      ;; two-armed conditional
		      `(let ((,condition ,arg))
			 (if ,test
			     ,(rewrite-variables then-branch decls)

			     (progn
			       ,@(rewrite-variables else-branch decls))))

		      ;; one-armed conditional
		      `(let ((,condition ,arg))
			 (if ,test
			     ,(rewrite-variables then-branch decls))))

		  ;; tests, no decls
		  (if else-branch
		      ;; two-armed conditional
		      `(let ((,condition ,arg))
			 (if ,test
			     ,then-branch

			     (progn
			       ,@else-branch)))

		      ;; one-armed conditional
		      `(let ((,condition ,arg))
			 (if ,test
			     ,(rewrite-variables then-branch decls))))))

	    (if decls
		;; decls, no tests
		(progn
		  (unless (null else-branch)
		    (warn 'unreachable-code :hint "Should there be fixed bits to test?"))

		  `(let ((,condition ,arg))
		     ,(rewrite-variables then-branch decls)))

		(progn
		  ;; no decls or tests
		  (warn 'unreachable-code :hint "Why is this assignment here?")
		  nil)))))))


(defcoremacro/vl with-bitfields (pattern arg &body body)
  "Create variables matching the bitfield PATTERN applied to ARG in BODY.

The patterns are as in IF-LET-BITFIELDS. If the fixed bits do not
match, BODY is not evaluated."

  ;; catch the common error of forgetting the value
  ;; to match against with a one-form body
  (when (< (length body) 1)
    (error 'syntax-error :form arg
			 :hint "No value to match against?"))

  `(if-let-bitfields ,pattern
		     ,arg
		     (progn ,@body)))
