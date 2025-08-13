;; A flasher, for debugging
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

(defmodule/vl flasher (clk
		       flash flashes led
		       &optional flashing-p
		       &key (delay 22))
  (declare (type (unsigned-byte 3) flashes))

  (let ((doflash 0)
	(doingflash 0)
	(counter 1))
    (declare (width delay counter))

    ;; connect flash action to LED, and feedback to status
    (setq led doflash)
    (setq flashing-p doingflash)

    (@ (posedge clk)
       (forever
	;; wait until trigger
	(until (0/= flash))

	(with-asserted doingflash
	  (dotimes (i flashes)
	    (declare (type (unsigned-byte 3) i))

	    ;; flash on
	    (setq doflash 1)
	    (setq counter 1)
	    (while (> counter 0)
		   (incf counter))

	    ;; flash off
	    (setq doflash 0)
	    (setq counter 1)
	    (while (> counter 0)
		   (incf counter))))))))
