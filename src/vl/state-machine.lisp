;; State machine construction
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


(defun typecheck-sm-state (state)
  "Typecheck a single machine STATE description."
  (destructuring-bind (name &rest body)
      state

    ;; states must be integers -- should generalise to symbols too
    (ensure-subtype name 'unsigned-byte)

    ;; the type of the state is the type of the last form
    (typecheck `(progn ,@body))))


(defmethod typecheck-sexp ((fun (eql 'state-machine) args))
  (let ((tys (mapcar #'typecheck-sm-state args)))
    (last tys)))


(defmethod synthesise-sexp ((fun (eql 'state-machine)) args)
  (with-gensyms (state)
    (let ((sm `(@ (posedge clk)
		  (case ,state
		    ,@args)
		  )))

      (synthesise sm))))


(defmethod synthesise-sexp ((fun (eql 'next-state)) args)

  )
