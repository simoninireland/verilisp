;; A USB UART
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

;; This is a transliteration into Verilisp of the "Documented Verilog UART" from
;; https://github.com/cyrozap/osdvu/blob/master/uart.v
;;
;; Copyright (C) 2010 Timothy Goddard (tim@goddard.net.nz)
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

(defmodule/vl uart (clk rst
		    rx rx-byte received-p receiving-p receive-error-p
		    tx tx-byte transmit transmitting-p
		    &key
		    (clk-divide 1302))   ;; (/ clock-rate (* baud-rate 4))
  (declare (type bit clk rst
		     rx received-p receiving-p receive-error-p
		     tx transmit transmitting-p)
	   (type (unsigned-byte 8) rx-byte tx-byte))

  (let ((rx-clk-divider clk-divide)
	(tx-clk-divider clk-divide)
	rx-out rx-data rx-countdown rx-bits-remaining
	tx-out tx-data tx-countdown tx-bits-remaining)
    (declare (type (unsigned-byte 10) rx-clk-divider tx-clk-divider)
	     (type (unsigned-byte 5) rx-countdown tx-countdown)
	     (type (unsigned-byte 3) rx-bits-remaining tx-bits-remaining))

    (setq rx-byte rx-data)
    (setq tx tx-out)

    (@ (posedge clk)
       ;; receive countdown
       (decf rx-clk-divider)
       (when (0= rx-clk-divider)
	 (setq rx-clk-divider clk-divide)
	 (decf rx-countdown))

       ;; transmit countdown
       (decf tx-clk-divider)
       (when (0= tx-clk-divider)
	 (setq tx-clk-divider clk-divide)
	 (decf tx-countdown))

       ;; receiving state machine
       (tagbody
	rx-idle
	  (setq receiving-p 0)
	  (setq received-p 0)
	  (setq receive-error-p 0)

	  ;; wait for low on rx to signal start of data
	  (while (0/= rx))

	  (setq receiving-p 1)
	  (setq rx-clk-divider clk-divide)
	  (setq rx-countdown 2)

	rx-check-start
	  (if (0= rx-countdown)
	      (if (0= rx)
		  ;; pulse still low
		  (progn
		    (setq rx-countdown 4)
		    (setq rx-bits-remaining 8)
		    (go rx-read-bits))

		  ;; pulse unexpectedly went high, error
		  (go rx-error)))

	rx-read-bits
	  (if (0= rx-countdown)
	      (progn
		(setq rx-data (make-bitfields rx (bref rx-data 7 :end 1)))
		(setq rx-countdown 4)
		(decf rx-bits-remaining)
		(if (0= rx-bits-remaining)
		    (go rx-check-stop))))

	rx-check-stop
	  (if (0= rx-countdown)
	      ;; receive should be high
	      (if rx
		  (go rx-received)
		  (go rx-error)))

	rx-delay-restart
	  (if (0= rx-countdown)
	      (go rx-idle))

	rx-error
	  (setq receive-error-p 1)
	  (setq rx-countdown 8)
	  (go rx-delay-restart)

	rx-received
	  (setq received-p 1)
	  (go rx-idle))

       ;; transmitting state machine
       (tagbody
	tx-idle
	  (setq transmitting-p 0)

	  (until (0/= transmit))

	  (setq transmitting-p 0)
	  (setq tx-data tx-byte)
	  (setq tx-clk-divider clk-divide)
	  (setq tx-countdown 4)
	  (setq tx-out 0)
	  (setq tx-bits-remaining 8)

	tx-sending
	  (if (0= tx-countdown)
	      (if (> tx-bits-remaining 0)
		  (progn
		    (decf tx-bits-remaining)
		    (setq tx-out (bref tx-data 0))
		    (setq tx-data (make-bitfields 0 (bref tx-data 7 :end 1)))
		    (setq tx-countdown 4))

		  (progn
		    (setq tx-out 1)
		    (setq tx-countdown 8)
		    (go tx-delay-restart))))

	tx-delay-restart
	  (if (0= tx-countdown)
	      (go tx-idle))))))
