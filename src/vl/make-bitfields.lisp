;; Constructing words from bitfields
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


;; ---------- make-bitfields ----------

(defun typecheck-pattern (pat)
  "Typecheck bitfield pattern PAT."
  (let ((typ (typecheck pat)))
    (ensure-fixed-width typ)
    typ))


(defmethod typecheck-sexp ((fun (eql 'make-bitfields)) args)
  (destructuring-bind (&rest pats)
      args
    (let ((tys (mapcar #'typecheck-pattern pats)))
      ;; work out the bit width of the result
      (let ((w (foldr #'+
		      (mapcar #'bitwidth tys)
		      0)))
	`(unsigned-byte ,w)))))


(defmethod synthesise-sexp ((fun (eql 'make-bitfields)) args)
  (as-literal "{")
  (as-inline-forms args :sep ",")
  (as-literal "}"))


;; ---------- extend-bits ----------

(defmethod typecheck-sexp ((fun (eql 'extend-bits)) args)
  (destructuring-bind (bs width)
      args
    (let ((tyw (typecheck width))
	  (tyb (typecheck bs)))
      (ensure-fixed-width tyw)
      (ensure-fixed-width tyb)

      ;; evaluate the width, which must be statically determined
      (let ((w (ensure-static width)))
	`(unsigned-byte ,w)))))


(defun synthesise-fixed-width-constant (c width &optional (base 2))
  "Synthesise C as a constant with the given WIDTH.

The BASE used can be 2, 8, 10, or 16."
  (synthesise width)
  (as-literal "'")
  (as-literal (ecase base
		(2  "b")
		(8  "o")
		(10 "d")
		(16 "x")))
  (let ((*print-base* base))
    (synthesise c)))


(defmethod synthesise-sexp ((fun (eql 'extend-bits)) args)
  (destructuring-bind (bs width)
      args
    (as-literal "{")
    (synthesise width)
    (as-literal "{")
    (if (static-constant-p bs)
	;; value is a static constant, output it
	(let ((w (bitwidth (ensure-static bs))))
	  (synthesise-fixed-width-constant bs w))

	;; value is an expression, synthesise it
	(progn
	  (synthesise bs)))
    (as-literal "}}")))
