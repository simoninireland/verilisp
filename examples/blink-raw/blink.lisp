;; A simple LED blinker in close-to-Verilog RTLisp
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of cl-vhdsl, a Common Lisp DSL for hardware design
;;
;; cl-vhdsl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-vhdsl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-vhdsl. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :cl-vhdsl/examples/blink-raw)


(defmodule/vl blink ((clk  :width 1 :direction :in)
		     (leds :width bits :direction :out)
		     &key (bits 5) (delay 22))

  (let ((counter 0 :width (+ bits delay))
	(out 0 :width 5))

    (@ (posedge clk)
       (setf counter (+ counter 1))
       (setf out (>> counter delay)))

    (setf leds out)))
