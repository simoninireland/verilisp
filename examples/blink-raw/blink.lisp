;;;; A simple LED blinker
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

(defmodule/vl blink (clk leds
		     &key (bits 5) (delay 24))
  (declare (type bit clk)
	   (width bits leds))

  (let ((counter 0)
	(out 0))
    (declare (width (+ bits delay) counter)
	     (width bits out))

    (@ (posedge clk)
       (setf counter (+ counter 1))
       (setf out (>> counter delay)))

    (setf leds out)))
