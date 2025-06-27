;; Tests of synthesisable fragment passes and synthesis
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


;; ---------- Addition and subtraction ----------

(test test-add-widths
  "Test we can determine the widths of additions."
  (is (vl:subtype-p (vl:typecheck '(+ 1 1))
		    '(unsigned-byte 2)))
  (is (vl:subtype-p (vl:typecheck '(+ 15 2))
		    '(unsigned-byte 5)))
  (is (not (vl:subtype-p (vl:typecheck '(+ 15 -2))
			 '(unsigned-byte 5))))
  (is (vl:subtype-p (vl:typecheck '(+ 15 -2))
		    '(signed-byte 6))))


(test test-width-subtractions
  "Test we can extract the widths of subtractions."
  (is (vl:subtype-p (vl:typecheck '(- 1))
		    '(signed-byte 2)))
  (is (vl:subtype-p (vl:typecheck '(- 2 1))
		    '(signed-byte 3))))


(test test-synthesise-addition-operators
  "Test we can synthesise addition and subtraction operators."
  ;; arithmetic
  (dolist (op '(+ - *))
    ;; conventional two-operand
    (is (vl:synthesise `(,op 1 2)))

    ;; Lisp-y multi-operand
    (is (vl:synthesise `(,op 1 2 3))))

  ;; unary minus
  (is (vl:synthesise `(- 1))))


;; ---------- Shifts ----------

(test test-width-shifts
  "Test we can extract the widths of shifts."
  (is (vl:subtype-p (vl:typecheck '(vl::<< 1 2))
		    '(unsigned-byte 4)))
  (is (vl:subtype-p (vl:typecheck '(vl::<< 15 15))
		    '(unsigned-byte 19)))

  (is (vl:subtype-p (vl:typecheck '(vl::>> 16 4))
		    '(unsigned-byte 5)))

  ;; wrong number of arguments
  (dolist (op '(vl::<< vl::>>))
    (signals (vl:not-synthesisable)
      (vl:typecheck `(,op 1 2 3)))
    (signals (vl:not-synthesisable)
      (vl:typecheck `(,op 3)))))


(test test-synthesise-shift-operators
  "Test we can synthesise shift operators."
  (dolist (op '(vl::<< vl::>>))
    ;; conventional two-operand
    (is (vl:synthesise `(,op 1 2)))))


;; ---------- Bitwise operators ----------

(test test-typecheck-logop
  "Test we can typecheck the logical operators."
  (is (vl:subtype-p (vl:typecheck '(logand #2r10110 #2r11110))
		    '(unsigned-byte 5)))

  (is (vl:subtype-p (vl:typecheck '(logand #2r10110 #2r1111110))
		    '(unsigned-byte 7))))
