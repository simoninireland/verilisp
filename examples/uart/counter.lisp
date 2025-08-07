;; A counter over a UART
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


(defmacro/vl flash (n)
  "Flash the flasher N times."
  `(progn
     ;; set up and trigger the flashes
     (setq flashes ,n)
     (pulse flash)

     ;; wait for them to finish
     (until (not flashing-p))))


(defmodule/vl counter (clk rx tx led
			   &key (delay 22))
  (declare (type bit clk rx tx led))

  (let (flashlight)

    (setq led flashlight)

    (let (flash flashes flashing-p counter)
      (declare (width delay counter)
	       (width 3 flashes)
	       (type bit flashing-p))

      (let ((flasher (make-instance 'flasher :clk clk :flash flash :flashes flashes :led flashlight
					     :flashing-p flashing-p
					     :delay (- delay 2))))

	(@ (posedge clk)

	   (forever
	    ;; do some flashes
	    (flash 6)

	    ;; delay!
	    (setq counter 1)
	    (while (> counter 0)
		   (incf counter))
	    (setq counter 1)
	    (while (> counter 0)
		   (incf counter))
	    (setq counter 1)
	    (while (> counter 0)
		   (incf counter))))))))
