;; Tests of MAKE-BITFIELDS and REPEAT-BITS
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

(in-package :verilisp/test)
(in-suite verilisp/vl)


(test test-make-bitfields-literal
  "Test we can make a bitfield from literals."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(vl:make-bitfields #2r111 #2r100)))
		    '(unsigned-byte 6)))

  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(vl:make-bitfields
						  #2r111
						  (the (unsigned-byte 12) 0))))
		    '(unsigned-byte 15))))


(test test-make-bitfields-extend
  "Test we can extend bitfields."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(vl:make-bitfields
						  #2r101
						  (vl:extend-bits #2r0 5))))
		    '(unsigned-byte 8)))

  ;; repeats must be statically known, but pattern can be variable
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(let ((a 8))
						  (vl:make-bitfields (vl:extend-bits a 5)))))
		    '(unsigned-byte 20)))
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(let ((a 8))
						  (vl:make-bitfields (vl:extend-bits (+ a 9) 5)))))
		    '(unsigned-byte 25)))
  (signals (vl:not-static)
    (vl:typecheck (vl:expand/vl '(let ((a 8))
				  (vl:make-bitfields (vl:extend-bits 0 a)))))))


(test test-test-synthesise-make-bitfields
  "Test we can synthesise constructed bitfields."
  (let ((p (vl:expand/vl '(vl:make-bitfields #2r111 #2r100))))
    (vl:typecheck p)
    (is (vl:synthesise p)))

  (let ((p (vl:expand/vl '(let ((a 1)
				b)
			   (setq b (vl:make-bitfields (vl:extend-bits a 5)))))))
    (vl:typecheck p)
    (vl:synthesise p)))
