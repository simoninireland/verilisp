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

(in-package :vl)
(declaim (optimize debug))


;; We do these with a macro that re-writes variables in the body.
;; TODO: Decide what we do if patterns don't match because of
;; incorrect 0s and 1s. My current idea is don't execute the body,
;; but that'd be a silent failure on the hardware (although it could
;; be checked in simulation)
;;
;; A better idea is to refator and have IF-LET-BITFIELDS with a failure
;; branch.
;;
;; Might also want to expand the pattern language to allow, for example,
;; (a a a (b 3) c c) as an alternative to (a a a b b b c c). The count
;; would need to be statically known.

(defun extract-runs (pattern)
  "Extract consecutive runs of a symbol from PATTERN.

Detection happens regardless of repetition.

Return an alist mapping symbol to start and end positions,
with the rightmost position being 0."
  (labels ((extract (pat i currentrun)
	     (let ((s (car pat)))
	       (cond ((< i 0)
		      (list currentrun))

		     ((null currentrun)
		      (extract (cdr pat)
			       (1- i)
			       (list s i i)))

		     ((equal s (car currentrun))
		      (extract (cdr pat)
			       (1- i)
			       (list s (cadr currentrun) i)))

		     (t
		      (cons currentrun
			    (extract (cdr pat)
				     (1- i)
				     (list s i i))))))))

    (if (null pattern)
	nil
	(extract pattern (1- (length pattern)) nil))))


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
				  :hint "Pattern has non-consecutive runs"))

      runs)))


(defun bitfield-fixed-bit-p (b)
  "Test whether B is a fixed bit.

Fixed bits are constant 0s or 1s."
  (member b '(0 1)))


(defun bitfield-contains-fixed-bits-p (runs)
  "Test whether RUNS includes fixed bits."
  (not (null (find-if (lambda (run)
			(bitfield-fixed-bit-p (car run)))
		      runs))))


(defun run-to-decl (arg run)
  "Return the Verilisp declaration correponding to RUN over ARG."
  (destructuring-bind (s start end)
      run
    (if (= start end)
	;; single bit
	`(,s (bref ,arg ,start) :width 1)

	;; several bits
	`(,s (bref ,arg ,start :end ,end) :width ,(1+ (- start end))))))


(defmacro/vl with-bitfields (pattern arg &body body)
  "Create variables matching the bitfield PATTERN applied to ARG in BODY.

The pattern consists of a list of variable names, with each
entry corresponding to a bit position. The rightmost bit is
bit zero, then bit one to its left, and so forth. If several
consecutive positions have the same variable name, the variable
takes multiple bits.

The pattern may also contain 0, 1, and - for literal 0, 1, and
ignored bits.

For example:

(with-bitfields (o o o a a a a 0)
   opcode
...)

would declare variables o and a in the body, with the values
extracted from bits 7 to 5 and 4 to 1 respectively, and
the lowest-order bit being 0.

If the fixed bits do not match, BODY is not evaluated."

  ;; catch the common error of forgetting the value
  ;; to match against with a one-form body
  (when (< (length body) 1)
    (error 'not-synthesisable :hint "No value to match against?"))

  (let ((runs (extract-bitfields pattern)))
    (if (bitfield-contains-fixed-bits-p runs)
	(error 'not-synthesisable :hint "Fixed bits not yet implemented")

	;; no fixed bits, don't synthesise tests
	(let* ((decls (mapcar (curry #'run-to-decl arg) runs))
	       (newbody (rewrite-variables `(progn ,@body) decls)))
	  newbody))))
