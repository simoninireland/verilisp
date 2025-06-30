;; SPI RAM module, state machine version
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



(tagbody
 init
   ;; Initial state, wait for read-strobe
   (unless read-strobe
     (go init))

 setup-write-command-and-address
   ;; Initialise shifter for writing command and address
   (setq chip-select/0 0)
   (setq chip-io-read/write 1)
   (setq receive/send 1)
   (setq shifter (make-bitfields (doubled #16rbb)
				 (copy-bit 0 4)
				 (bref addr 19 :width 18)
				 (copy-bit 0 2)))
   (setq clock-count 28)

 write-command-and-address
   ;; Write command and address
   (setq shifter (make-bitfields (bref shifter 37 :end 0)
				 (copy-bit 1 2)))
   (decf clock-count)
   (when (> clock-count 1)
     (go write-command-and-address))

 setup-read-data
   ;; Initialise shifter for reading data
   (setq clock-count 16)
   (setq chip-io-read/write 0)
   (setq read/write 0)

 read-data
   (setq shifter (make-bitfields (bref shifter 37 :end 0)
				 chip-io))
   (decf clock-count)
   (when (> clock-count 1)
     (go read-data))

 clean-up
   (setq chip-select/0 1)
   (go init))
