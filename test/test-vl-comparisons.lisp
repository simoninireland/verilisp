;; Tests of synthesising logical tests and operations
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


;; ---------- Comparisons----------

(test test-typecheck-equality
  "Test we can type-check equality."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(= 1 2)))
		    '(unsigned-byte 1))))


(test test-typecheck-inequality
  "Test we can type-check inequality."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(/= 1 2)))
		    '(unsigned-byte 1))))


(test test-typecheck-gt-lt
  "Test we can type-check greater-than and less-than."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(< 1 2)))
		    '(unsigned-byte 1)))
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(> 1 2)))
		    '(unsigned-byte 1))))


(test test-typecheck-asserted
  "Test we can type-check assertedness."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(vl:asserted-p 1)))
		    '(unsigned-byte 1))))
