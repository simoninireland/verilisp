;;;; Function with one caller
;;;;
;;;; Copyright (C) 2024--2025 Simon Dobson
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

;;; The protocol for calling the function:
;;;
;;; 1. Starts with ready=0 and finished=0
;;; 2. Caller waits for finished=0; function waits for ready=1
;;; 3. Caller assigns values to arguments, sets ready=1, and
;;;    waits for finished=1
;;; 4. Function sets finished=0 and computes
;;; 5. Function sets finished=1 and waits for ready=0
;;; 6. Caller retrieves results (if any) and sets ready=0
;;; 7. Function sets finished=0


(defmodule/vl flasher (clk
		       flashy
		       start	 ;; initial value for flasher
		       ready	 ;; input ready trigger flag
		       finished) ;; done flag
  (declare (type bit clk ready finished)
	   (type (unsigned-byte 5) flashy start)
	   (direction in clk start ready)
	   (direction out flashy finished))

  (let ((leds 0)
	fin)
    (declare (type (unsigned-byte 5) leds)
	     (type bit fin))

    (setq flashy leds)
    (setq finished fin)

    (@ (posedge clk)
       (setq fin 0)

       (forever
	;; wait for ready signal
	(until (asserted-p ready))

	;; signal busy
	(setq fin 0)

	;; flash the LEDs for the requested amount
	(dotimes (i start)
	  (setq leds i))

	;; signal finished
	(setq fin 1)
	(until (not (asserted-p ready)))
	(setq fin 0))

       )))


(defmodule/vl SOC (clk-in
		   leds-out)
  (declare (type bit clk-in)
	   (type (unsigned-byte 5) leds-out)
	   (direction in clk-in)
	   (direction out leds-out))

  (let* (clk reset leds
	 (cw (make-instance 'clockworks :clk-in clk-in
					:reset-in 0
					:clk clk
					:reset reset
					:slow 19))
	 flashes (ready 0) finished
	 (flash (make-instance 'flasher :clk clk
					:flashy leds-out
					:start flashes
					:ready ready
					:finished finished))
	 )
    (declare (type bit clk reset)
	     (type (unsigned-byte 5) leds)
	     (type bit ready finished)
	     (type (unsigned-byte 5) flashes)
     )

    (setq leds-out leds)

    (@ (posedge clk)
       (forever
	;; call the function
	(until (not (asserted-p finished)))
	(setq flashes 4)
	(setq ready 1)
	(until (asserted-p finished))
	(setq ready 0)

	;; call the function
	(until (not (asserted-p finished)))
	(setq flashes 8)
	(setq ready 1)
	(until (asserted-p finished))
	(setq ready 0)))))
