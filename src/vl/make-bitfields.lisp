;;;; Constructing words from bitfields
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

(in-package :verilisp/core)
(declaim (optimize debug))


;;; ---------- make-bitfields ----------

(defpassmethod compute-type (make-bitfields &rest pats)
  (let ((tys (mapcar #'compute-type pats)))
    `(and ,@tys)))


(defpassmethod apply-type-constraints (make-bitfieldsK &rest pats)
  (let ((tys (mapcar #'compute-type pats)))
    (mapc #'ensure-fixed-width tys)))


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


(defun synthesise-make-bitfields-field (f)
  "Synthesise a field F in a bitfield."
  (if (static-constant-p f)
      ;; value is a static constant, output it
      (let ((w (bits-for-integer (ensure-static f))))
	(synthesise-fixed-width-constant f w))

      ;; value is an expression, synthesise it
      (synthesise f)))


(defpassmethod simple-expression-form-p (make-bitfields &rest pats)
  (every #'simple-expression-form-p pats))


(defpassmethod synthesise (make-bitfields &rest pats)
  (as-literal "{")
  (as-inline-forms pats :sep ", " :process #'synthesise-make-bitfields-field)
  (as-literal "}"))


;;; ---------- extend-bits ----------

(defpassmethod compute-type (extend-bits bs times)
  (let ((tybs (compute-type bs))
	(n (eval-in-static-environment times)))

    `(and ,@(n-copies tybs n))))


(defpassmethod apply-type-constraints (extend-bits bs times)
  (let ((tyb (compute-type bs)))
    (ensure-fixed-width tyb)))


(defpassmethod read-variables (extend-bits bs times)
  (union-all (mapcar #'read-variables (list bs times))))


(defpassmethod simple-expression-form-p (extend-bits &rest args)
  (every #'simple-expression-form-p args))


(defpassmethod synthesise (extend-bits bs times)
  (as-literal "{")
  (synthesise times)
  (as-literal "{")
  (synthesise-make-bitfields-field bs)
  (as-literal "}}"))
