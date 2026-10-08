;;;; Macros for defining protocols using wire-wiggling
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

;;; Wire protocols are tricky to define: it's often referred to
;;; as "wire wiggling", where the program explicitly interacts
;;; with the states of signals on wires. This macro set is intended
;;; to apply some structure to this process.
;;;
;;; A better solution would probably be to define session types so
;;; that both ends of an interaction are specified together.


;;; ---------- Signal assertion ----------

;;; In each case WIRE is assumed to be a wire -- but it doesn't matter
;;; if it's wider, it'll just affect the lowest-order bit.

(defmacro/vl assert (wire)
  "Assert a value of 1 on WIRE."
  `(setf ,wire 1))


(defmacro/vl deassert (wire)
  "Assert a value of 0 (or deassert) on WIRE."
  `(setf ,wire 0))


(defmacro/vl asserted-p (wire)
  "Test whether WIRE is asserted."
  `(0/= (logand ,wire 1)))     ; only check the lowest-order bit


(defmacro/vl with-asserted (wire &body body)
  "Run BODY with WIRE asserted.

WIRE is deasserted afterward."
  `(progn
     (assert ,wire)
     ,@body
     (deassert ,wire)))


(defmacro/vl if-asserted (wire asserted-body &rest deasserted-body)
  "If WIRE is asserted, run ASSERTED-BODY, otherwise run optional DEASSERTED-BODY.

As with normal IF, ASSERTED-BODY should be a single form while
DEASSERTED-BODY can be a list of forms."
  `(if (asserted-p ,wire)
       ,asserted-body

       ,@deasserted-body))


(defmacro/vl while-asserted (wire &body body)
  "Run BODY repeatedly for as long as WIRE is asserted.

If WIRE is not asserted initially, BODY isn't run at all."
  `(while (asserted-p ,wire)
	  ,@body))


(defmacro/vl until-asserted (wire &body body)
  "Run BODY repeatedly until WIRE is asserted.

If WIRE is asserted initially, BODY isn't run at all."
  `(while (not (asserted-p ,wire))
	  ,@body))


;;; ---------- Bitstreams ----------

;;; These macros send a value as a bitstream to a wire, one bit
;;; at each clock tick. This is the simplest multi-bit transfer
;;; protocol imaginable -- but also the lowest overhead.

(defmacro/vl send-as-bitstream (n v wire &optional bigendian)
  "Send N bits from V to WIRE, one bit per clock tick.

The bits are sent little-endian (lowest-order bit first) unless
BIGENDIAN is non-nil."
  (with-gensyms (bit)
    `(dotimes (,bit ,n)
     ` (setf ,wire (bref ,v ,(if bigendian
				 `(- (1- ,n) ,bit)
				 `,bit))))))


(defmacro/vl receive-as-bitstream (n v wire &optional bigendian)
  "Read N bits from WIRE into V, one bit per clock tick.

The bits are read little-endian (lowest-order bit first) unless
BIGENDIAN is non-nil."
  (with-gensyms (bit)
    `(progn
       (setf ,v 0)

       (dotimes (,bit ,n)
	 (setf (bref ,v ,(if bigendian
			     `(- (1- ,n) ,bit)
			     `,bit))
	       ,wire)))))
