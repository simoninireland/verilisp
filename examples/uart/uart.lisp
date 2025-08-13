;; A UART
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

;; This is a Verilisp UART written in a "software style" but inspired
;; by the "Documented Verilog UART" from
;; https://github.com/cyrozap/osdvu/blob/master/uart.v

;; Copyright (C) 2010 Timothy Goddard (tim@goddard.net.nz)
;;               2013 Aaron Dahlen
;; Distributed under the MIT licence.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defmacro/vl with-asserted (wire &body body)
  "Perform BODY with WIRE set to 1, and the reset WIRE to 0."
  `(progn
     (setq ,wire 1)
     ,@body
     (setq ,wire 0)))


(defmacro/vl wait (time counter)
  "Wait for TIME cycles using COUNTER to count the cycles."
  `(progn
     (setf ,counter ,time)
     (while (0/= ,counter))))


(defmacro/vl one-baud (baud-rate clk-rate)
  "Return the number of CLK-RATE ticks per cycle at BAUD-RATE."
  (floor (/ clk-rate baud-rate)))


(defmodule/vl uart (clk rst
			rx rx-byte received-p receiving-p receive-error-p
			tx tx-byte transmit transmitting-p)
  (declare (type bit clk rst
		 rx received-p receiving-p receive-error-p
		 tx transmit transmitting-p)
	   (type (unsigned-byte 8) rx-byte tx-byte))

  (let ((one-baud-clk (one-baud 9600 12000000))
	(rx-clk one-baud-clk)
	(tx-clk one-baud-clk)
	rx-data
	rx-status-receiving-p rx-status-received-p rx-status-error-p
	tx-data (tx-out 1)
	tx-status-idle-p)
    (declare (as constant one-baud-clk)) ;; should be inferred

    ;; wire externally visible flags to internal status wires
    (setq received-p rx-status-received-p)
    (setq receive-error-p rx-status-error-p)
    (setq receiving-p rx-status-receiving-p)
    (setq transmitting-p (not tx-status-idle-p))

    (setq rx-byte rx-data)
    (setq tx tx-out)

    (@ (posedge clk)
       ;; clock dividers
       (if (0/= rx-clk)
	   (decf rx-clk))
       (if (0/= tx-clk)
	   (decf tx-clk))

       ;; receiving state machine
       (forever
	(setq rx-status-receiving-p 0)

	;; wait for low start bit
	(until (0= rx))

	(setq rx-status-received-p 0)
	(setq rx-status-error-p 0)
	(setq rx-status-receiving-p 1)

	;; wait half a cycle and re-check
	(wait (>> one-baud-clk 1) rx-clk)
	(if (0= rx)
	    (progn
	      ;; still in start bit, wait half a cycle
	      (wait (>> one-baud-clk 1) rx-clk)

	      ;; read all eight bits
	      (dotimes (bits 8)

		;; wait a quarter of a cycle
		(wait (>> one-baud-clk 2) rx-clk)

		;; take a sequence of samples of the next bit, to
		;; handle small amounts of clock drift
		(let ((samples 0))
		  (declare (width 4 samples))

		  (dotimes (countdown 6)
		    ;; wait an eighth of a cycle and sample
		    (wait (>> one-baud-clk 3) rx-clk)
		    (if (0/= rx)
			(incf samples)))

		  (if (> samples 3)
		      (setq rx-data (make-bitfields 1 (bref rx-data 7 :end 1)))
		      (setq rx-data (make-bitfields 0 (bref rx-data 7 :end 1))))))

	      ;; wait another half-cycle to sample the middle of the high stop bit
	      (wait (>> one-baud-clk 1) rx-clk)
	      (if (0/= rx)
		  ;; stop bit received, signal receipt
		  (setq rx-status-received-p 1)

		  ;; no stop bit, signal error
		  (progn
		    (setq rx-status-error-p 1)
		    (wait (<< one-baud-clk 3) rx-clk))))

	    ;; start bit changed unexpectedly, signal error
	    (progn
	      (setq rx-status-error-p 1)
	      (wait (<< one-baud-clk 3) rx-clk))))

       ;; transmitting state machine
       (forever
	;; wait for transmit to be strobed high
	(with-asserted tx-status-idle-p
	  (until (0/= transmit)))

	;; latch the byte to transmit
	(setq tx-data tx-byte)

	;; send start bit
	(setq tx-out 0)
	(wait one-baud-clk tx-clk)

	;; send all bits
	(dotimes (tx-bits 8)
	  (declare (width 4 tx-bits))

	  (setq tx-out (bref tx-data 0))
	  (setq tx-data (make-bitfields 0 (bref tx-data 7 :end 1)))
	  (wait one-baud-clk tx-clk))

	;; send two stop bits
	(setq tx-out 1)
	(wait (<< one-baud-clk 1) tx-clk)

	;; wait for transmit to be low (prevents duplicate characters)
	(while (0/= transmit))))))
