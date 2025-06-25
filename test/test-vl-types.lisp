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


;; ---------- Fixed-width integers ----------

(test test-unsigned-range
  "Test the range of an unsigned fixed-width integer."
  (is (typep 12 '(unsigned-byte 8)))
  (is (typep 12 '(unsigned-byte 16)))

  (is (typep 512 '(unsigned-byte 16)))
  (is (not (typep 512 '(unsigned-byte 8))))

  (is (not (typep -64 '(unsigned-byte 8)))))


(test test-signed-range
  "Test the range of a signed fixed-width integer."
  (is (typep 12 '(signed-byte 8)))
  (is (typep 12 '(signed-byte 16)))
  (is (typep 127 '(signed-byte 8)))
  (is (typep -128 '(signed-byte 8)))

  (is (not (typep 128 '(signed-byte 8))))
  (is (not (typep -129 '(signed-byte 8))))
  (is (not (typep -64 '(signed-byte 4)))))


;; ---------- Widths ----------

(test test-bitwidths-integer-types
  "Test we can extract the widths of integer types."
  (is (= (vl::bitwidth '(unsigned-byte 8)) 8))
  (is (= (vl::bitwidth '(signed-byte 8)) 8)))


(test test-bitwidths-integer-constants
  "Test we can extract bit widths of integer constants."
  (is (= (vl::bitwidth 0) 1))
  (is (= (vl::bitwidth 1) 1))
  (is (= (vl::bitwidth 2) 2))
  (is (= (vl::bitwidth 127) 7))

  (is (= (vl::bitwidth -127) 8))
  (is (= (vl::bitwidth -1) 2)))


;; ---------- Sub-typing ----------

(test test-subtype-fixed-width
  "Test the fixed-width types for sub-type relationships."

  ;; unsigned vs unsigned
  (is (vl:subtype-p 'unsigned-byte 'unsigned-byte))
  (is (vl:subtype-p 'unsigned-byte '(unsigned-byte *)))
  (is (vl:subtype-p '(unsigned-byte *) '(unsigned-byte *)))
  (is (vl:subtype-p '(unsigned-byte *) 'unsigned-byte))
  (is (vl:subtype-p '(unsigned-byte 12) 'unsigned-byte))
  (is (vl:subtype-p '(unsigned-byte 12) '(unsigned-byte 12)))
  (is (vl:subtype-p '(unsigned-byte 12) '(unsigned-byte 16)))
  (is (not (vl:subtype-p '(unsigned-byte 12) '(unsigned-byte 8))))

  ;; signed vs signed
  (is (vl:subtype-p 'signed-byte 'signed-byte))
  (is (vl:subtype-p 'signed-byte '(signed-byte *)))
  (is (vl:subtype-p '(signed-byte *) '(signed-byte *)))
  (is (vl:subtype-p '(signed-byte *) 'signed-byte))
  (is (vl:subtype-p '(signed-byte 12) 'signed-byte))
  (is (vl:subtype-p '(signed-byte 12) '(signed-byte 12)))
  (is (vl:subtype-p '(signed-byte 12) '(signed-byte 16)))
  (is (not (vl:subtype-p '(signed-byte 12) '(signed-byte 8))))

  ;; unsigned vs signed
  (is (vl:subtype-p 'unsigned-byte 'signed-byte))
  (is (vl:subtype-p '(unsigned-byte *) 'signed-byte))
  (is (vl:subtype-p '(unsigned-byte *) '(signed-byte *)))
  (is (vl:subtype-p '(unsigned-byte 12) 'signed-byte))
  (is (vl:subtype-p '(unsigned-byte 12) '(signed-byte 13)))
  (is (not (vl:subtype-p '(unsigned-byte 12) '(signed-byte 12))))
  (is (not (vl:subtype-p '(unsigned-byte 12) '(signed-byte 8))))

  ;; signed vs unsigned
  (is (not (vl:subtype-p 'signed-byte 'unsigned-byte)))

  ;; bits
  (is (vl:subtype-p 'bit 'unsigned-byte))q
  (is (vl:subtype-p 'bit '(unsigned-byte 1)))
  (is (vl:subtype-p 'bit '(unsigned-byte 8))))


(test test-type-fixed-width
  "Test the classifiers."

  ;; fixed width
  (is (vl:fixed-width-p 'unsigned-byte))
  (is (vl:fixed-width-p 'signed-byte))
  (is (vl:fixed-width-p '(unsigned-byte 8)))
  (is (vl:fixed-width-p '(signed-byte 8)))
  (is (vl:fixed-width-p 'bit))

  ;; detailed classes
  (is (vl:unsigned-byte-p 'unsigned-byte))
  (is (not (vl:unsigned-byte-p 'signed-byte)))
  (is (vl:signed-byte-p 'signed-byte))
  (is (vl:signed-byte-p 'unsigned-byte)))


(test test-type-lattice
  "Test the type lattice."

  ;; top type
  (is (vl:subtype-p 'unsigned-byte t))
  (is (vl:subtype-p '(unsigned-byte 8) t))
  (is (vl:subtype-p nil t))
  (is (vl:subtype-p t t))
  (is (not (vl:subtype-p t 'unsigned-byte)))

  ;; bottom type
  (is (vl:subtype-p nil 'unsigned-byte))
  (is (vl:subtype-p nil '(unsigned-byte 8)))
  (is (not (vl:subtype-p 'unsigned-byte nil)))
  (is (not (vl:subtype-p '(signed-byte 8) nil)))
  (is (not (vl:subtype-p t nil)))
  (is (vl:subtype-p nil nil)))


(test test-type-complex
  "Test complex type specifiers."

  ;; union types
  (is (vl:subtype-p 'unsigned-byte '(or unsigned-byte signed-byte)))
  (is (vl:subtype-p '(unsigned-byte 8) '(or unsigned-byte signed-byte)))
  (is (vl:subtype-p 'signed-byte '(or unsigned-byte signed-byte)))
  (is (vl:subtype-p '(signed-byte 8) '(or unsigned-byte signed-byte)))
  (is (vl:subtype-p '(unsigned-byte 8) '(or (unsigned-byte 16) (unsigned-byte 4))))
  (is (vl:subtype-p 'bit '(or unsigned-byte signed-byte)))
  (is (vl:subtype-p 'bit '(or (unsigned-byte 1) signed-byte)))
  (is (vl:subtype-p '(unsigned-byte 1) '(or bit signed-byte)))
  (is (not (vl:subtype-p '(unsigned-byte 12) '(or bit (signed-byte 8)))))
  (is (vl:subtype-p '(unsigned-byte 12) '(or bit (signed-byte 8) t)))

  ;; intersection types
  ;; TBD

  ;; negation types
  (is (vl:subtype-p '(unsigned-byte 16) '(not (unsigned-byte 8)))))


;; ---------- Least upper-bounds of types ----------

(test test-test-lub-fixed-width
  "Test we can form LUBs of fixed-with types."

  ;; unsigned vs unsigned
  (is (equal (vl:lub 'unsigned-byte 'unsigned-byte)
	     'unsigned-byte))
  (is (equal (vl:lub 'unsigned-byte '(unsigned-byte *))
	     'unsigned-byte))
  (is (equal (vl:lub '(unsigned-byte *) 'unsigned-byte)
	     'unsigned-byte))
  (is (equal (vl:lub '(unsigned-byte 12) '(unsigned-byte *))
	     'unsigned-byte))
  (is (equal (vl:lub '(unsigned-byte *) '(unsigned-byte 12))
	     'unsigned-byte))
  (is (equal (vl:lub '(unsigned-byte 16) '(unsigned-byte 12))
	     '(unsigned-byte 16)))

  ;; signed vs signed
  (is (equal (vl:lub 'signed-byte 'signed-byte)
	     'signed-byte))
  (is (equal (vl:lub 'signed-byte '(signed-byte *))
	     'signed-byte))
  (is (equal (vl:lub '(signed-byte *) 'signed-byte)
	     'signed-byte))
  (is (equal (vl:lub '(signed-byte 12) '(signed-byte *))
	     'signed-byte))
  (is (equal (vl:lub '(signed-byte *) '(signed-byte 12))
	     'signed-byte))
  (is (equal (vl:lub '(signed-byte 16) '(signed-byte 12))
	     '(signed-byte 16)))

  ;; signed vs unsigned
  (is (equal (vl:lub 'unsigned-byte 'signed-byte)
	     'signed-byte))
  (is (equal (vl:lub '(signed-byte 16) '(unsigned-byte 12))
	     '(signed-byte 16)))
  (is (equal (vl:lub '(unsigned-byte 16) '(signed-byte 12))
	     '(signed-byte 17)))

  ;; bits
  (is (equal (vl:lub 'bit 'unsigned-byte)
	     'unsigned-byte))
  (is (equal (vl:lub 'unsigned-byte 'bit)
	     'unsigned-byte))
  (is (equal (vl:lub '(unsigned-byte 1) 'bit)
	     '(unsigned-byte 1)))
  (is (equal (vl:lub 'bit '(unsigned-byte 1))
	     '(unsigned-byte 1))))
