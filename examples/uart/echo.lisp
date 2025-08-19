;; A UART echo server
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a Common Lisp DSL for hardware design
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

(use-package :alexandria)


(defmacro/vl pulse (wire)
  "Pulse the value of WIRE to 1 for one clock cycle, and then back to 0."
  (with-gensyms (then)
    `(tagbody
	(setq ,wire 1)
	,then
	(setq ,wire 0))))


(defmodule/vl echo (clk rx tx led
		    &key (delay 22))
  (declare (type bit clk rx tx led)
	   (direction out tx))  ;; should be inferred from module's direction!

  (let (rx-byte received-p receiving-p receive-error-p
	tx-byte (transmit 0) transmitting-p)

    (let ((uart (make-instance 'uart :clk clk :rst 0
				     :rx rx :rx-byte rx-byte
				     :received-p received-p :receiving-p receiving-p :receive-error-p receive-error-p
				     :tx tx :tx-byte tx-byte
				     :transmit transmit :transmitting-p transmitting-p)))

      (@ (posedge clk)
	 (cond (received-p
		(setq tx-byte rx-byte)
		;;(setq tx-byte #16r62)
		(setq transmit 1))

	       (receive-error-p
		(setq tx-byte #16r21)
		(setq transmit 1))
	       (t
		(setq transmit 0)))))))
