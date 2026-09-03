;;;; Function with two caller threads that need to be arbitrated
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


;;; TODO: Can't pass arrays into modules (!) -- well, not 2d arrays, which is
;;; how it interprets an array of numbers. We could address this by using only
;;; bits within a number, or expanding the array so that it's flat.

(defmodule/vl flasher (clk
		       flashy

		       ;; arguments and results
		       caller-starts  ;; initial value for flasher
		       caller-results ;; number of flashes flashed

		       ;; protocol flags
		       caller-requests	  ;; requests for service
		       caller-completions ;; caller completion
		       &key (caller-sites 1))
  (declare (type bit clk)
	   (type (unsigned-byte 5) flashy)
	   (type (unsigned-byte (* 5 caller-sites)) caller-starts caller-results)
	   (type (unsigned-byte caller-sites) caller-requests caller-completions)
	   (direction in clk caller-requests caller-starts)
	   (direction out flashy caller-results caller-completions)
	   (as register caller-completions caller-results))

  (let ((leds 0)
	ready fin
	start result)
    (declare (type (unsigned-byte 5) leds start result)
	     (type bit ready fin))

    (setq flashy leds)

    ;; arbiter
    (@ (posedge clk)
       (let ((request-index 0))
	 (declare (type (unsigned-byte 2) request-index))

	 (forever
	  ;; wait for a service request
	  (until (asserted-p (bref caller-requests request-index))
		 (setf request-index (if (>= request-index (1- caller-sites))
					 0
					 (1+ request-index))))
	  (setf (bref caller-completions request-index) 0)

	  ;; connect the caller's arguments to the function's
	  (setf start (bref caller-starts (* 5 request-index) :width 5))

	  ;; signal the function
	  (setf ready 1)

	  ;; wait for completion
	  (until (asserted-p fin))

	  ;; connect the function's results to the caller's
	  (setf (bref caller-results (* 5 request-index) :width 5) result)

	  ;; signal completion to the caller
	  (setf (bref caller-completions request-index) 1)

	  ;; free the function
	  (setf ready 0))))

    ;; function
    (@ (posedge clk)
       (setq fin 0)

       (forever
	;; wait for the arguments to be provided
	(until (asserted-p ready))

	;; signal busy
	(setq fin 0)

	;; flash the LEDs for the requested amount
	(dotimes (i start)
	  (setq leds i))

	;; signal finished
	(setq result start)
	(setq fin 1)
	(until (not (asserted-p ready)))
	(setq fin 0)))))


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
					:slow 19)))
    (declare (type bit clk reset)
	     (type (unsigned-byte 5) leds))

    (setq leds-out leds)

    (let* (;; protocol flags
	   thread-requests
	   thread-completions

	   ;; arguments and results
	   thread-starts
	   thread-results

	   ;; function instance
	   (flash (make-instance 'flasher :clk clk
					  :flashy leds
					  :caller-requests thread-requests
					  :caller-completions thread-completions
					  :caller-starts thread-starts
					  :caller-results thread-results
					  :caller-sites 2)))
      (declare (width 2 thread-requests thread-completions)
	       (width (* 2 5) thread-starts thread-results))

      ;; call site 0
      (@ (posedge clk)
	 (let (result-0)
	   (declare (type (unsigned-byte 5) result-0))

	   (forever
	    ;; insert the arguments
	    (setf (bref thread-starts 0 :width 5) 4)

	    ;; call the function
	    (setf (bref thread-requests 0) 1)

	    ;; wait for completion
	    (until (asserted-p (bref thread-completions 0))))))

      ;; call site 1
      (@ (posedge clk)
	 (let (result-1)
	   (declare (type (unsigned-byte 5) result-1))

	   (forever
	    ;; insert the arguments
	    (setf (bref thread-starts 0 :width 5) 8)

	    ;; call the function
	    (setf (bref thread-requests 0) 1)

	    ;; wait for completion
	    (until (asserted-p (bref thread-completions 0)))))))))
