;; SPI RAM module, straight transliteration
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
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

(in-package :verilisp/core)


(defmacro/vl doubled (byte)
  (if (not (symbolp byte))
      (error "Byte to be doubled must be a variable, not ~a" byte))

  (flet ((bbyyttee (dd i)
	   (append `((bref ,byte ,i) (bref ,byte ,i))
		   dd)))

    `(make-bitfields ,@(foldr #'bbyyttee (iota 8) '()))))


(defmodule/vl spi-rom (clk
		       read-strobe addr read-data busy
		       chip-clock chip-select/0 chip-io)
  (declare (type bit clk chip-clk chip-select/0 read-strobe busy)
	   (type (unsigned-byte 32) addr read-data)
	   (type (unsigned-byte 2) chip-io)
	   (direction in clk read-strobe addr)
	   (direction out read-data busy chip-clock chip-select/0)
	   (direction inout chip-io)
	   (as register read-data))

  (let ((clock-count 0)
	(shifter 0)
	(receive/send 0)
	(chip-io-read/write 1))

    (let ((busy (/= clock-count 0))
	  (sending (and receive/send busy))
	  (receiving (and (not receive/send) busy))
	  (chip-io-out (bref shifter 39 :width 2)))
      (declare (as wire busy sending receiving))

      ;; chip clock needs to be disabled when not in use
      (setq chip-select/0 1)		; deselect
      (setq chip-clock (and (= chip-select/0 0)
			    clk))

      ;; chip output lines get either the top two bits
      ;; of the shifter, or are tristated
      (setq chip-io (if chip-io-read/write
			chip-io-out
			(copy-bit z 2)))

      ;; swizzle the shifter's data into big endian
      (setq read-data (make-bitfields (bref shifter  8 :width 8)
				      (bref shifter 15 :width 8)
				      (bref shifter 23 :width 8)
				      (bref shifter 31 :width 8)))

      (@ (negedge clk)
	 (if read-strobe
	     ;; set up
	     (progn
	       (setq chip-select/0 0)
	       (setq chip-io-read/write 1)
	       (setq receive/send 1)
	       (setq shifter (make-bitfields (doubled #16rbb)
					     (copy-bit 0 4)
					     (bref addr 19 :width 18)
					     (copy-bit 0 2)))
	       (setq clock-count 28))

	     ;; do the operation
	     (if busy
		 ;; shift shifter to SPI I/O pins
		 (progn
		   (setq shifter (make-bitfields (bref shifter 37 :end 0)
						 (if receiving
						     chip-io
						     (copy-bit 1 2))))
		   (decf clock-count)

		   ;; check for having sent the command
		   (if (and receive/send
			    (= clock-count 1))
		       ;; command complete, read the data
		       (progn
			 (setq clock-count 16)
			 (setq chip-io-read/write 0)
			 (setq read/write 0))))

		 ;; read complete, disable chip
		 (setq chip-select/0 1)))))))
