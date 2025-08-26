;; A UART echo using a UART module defined in Verilog and imported
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


(importmodule/vl uart (clk rst
		       rx rx_byte received is_receiving recv_error
		       tx tx_byte transmit is_transmitting)
  (declare (type bit clk rst
		     rx received is_receiving recv_error
		     tx transmit is_transmitting)
	   (type (unsigned-byte 8) rx_byte tx_byte)
	   (direction in clk rst rx tx_byte transmit)
	   (direction out tx rx_byte received is_receiving recv_error is_transmitting)))


(defmodule/vl echo (clk rx tx led
		    &key (delay 22))
  (declare (type bit clk rx tx led)
	   (direction out tx)) ;; should be inferred from module's direction!

  (let (rx-byte received-p receiving-p receive-error-p
	tx-byte (transmit 0) transmitting-p)

    (let ((uart0 (make-instance 'uart :clk clk :rst 0
				      :rx rx :rx_byte rx-byte
				      :received received-p :is_receiving receiving-p :recv_error receive-error-p
				      :tx tx :tx_byte tx-byte
				      :transmit transmit :is_transmitting transmitting-p)))

      (@ (posedge clk)
	 (cond (received-p
		(setq tx-byte rx-byte)
		(setq transmit 1))

	       (receive-error-p
		(setq tx-byte #16r21)	; "!"
		(setq transmit 1))

	       (t
		(setq transmit 0)))))))
