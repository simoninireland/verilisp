;;;; Tests of synthesisable fragment passes and synthesis
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

(in-package :verilisp/test)
(in-suite verilisp/vl)



;;; ---------- Fixed-width integers ----------

;;; These really just check that Common Lisp behaves as it should

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


;;; ---------- Widths ----------

(test test-bitwidths-integer-types
  "Test we can extract the widths of integer types."
  (is (= (vl::bitwidth '(unsigned-byte 8)) 8))
  (is (= (vl::bitwidth '(signed-byte 8)) 8)))


(test test-bitwidths-integer-constants
  "Test we can extract bit widths of integer constants."
  (is (= (vl::bits-for-integer 0) 1))
  (is (= (vl::bits-for-integer 1) 1))
  (is (= (vl::bits-for-integer 2) 2))
  (is (= (vl::bits-for-integer 127) 7))

  (is (= (vl::bits-for-integer -127) 8))
  (is (= (vl::bits-for-integer -128) 9))
  (is (= (vl::bits-for-integer -1) 2)))


;;; ---------- Sub-typing ----------

(test test-subtype-fixed-width
  "Test the fixed-width types for sub-type relationships."

  ;; unsigned vs unsigned
  (is (vl::subtype-p 'unsigned-byte 'unsigned-byte))
  (is (vl::subtype-p 'unsigned-byte '(unsigned-byte *)))
  (is (vl::subtype-p '(unsigned-byte *) '(unsigned-byte *)))
  (is (vl::subtype-p '(unsigned-byte *) 'unsigned-byte))
  (is (vl::subtype-p '(unsigned-byte 12) 'unsigned-byte))
  (is (vl::subtype-p '(unsigned-byte 12) '(unsigned-byte 12)))
  (is (vl::subtype-p '(unsigned-byte 12) '(unsigned-byte 16)))
  (is (not (vl::subtype-p '(unsigned-byte 12) '(unsigned-byte 8))))

  ;; signed vs signed
  (is (vl::subtype-p 'signed-byte 'signed-byte))
  (is (vl::subtype-p 'signed-byte '(signed-byte *)))
  (is (vl::subtype-p '(signed-byte *) '(signed-byte *)))
  (is (vl::subtype-p '(signed-byte *) 'signed-byte))
  (is (vl::subtype-p '(signed-byte 12) 'signed-byte))
  (is (vl::subtype-p '(signed-byte 12) '(signed-byte 12)))
  (is (vl::subtype-p '(signed-byte 12) '(signed-byte 16)))
  (is (not (vl::subtype-p '(signed-byte 12) '(signed-byte 8))))

  ;; unsigned vs signed
  (is (vl::subtype-p 'unsigned-byte 'signed-byte))
  (is (vl::subtype-p '(unsigned-byte *) 'signed-byte))
  (is (vl::subtype-p '(unsigned-byte *) '(signed-byte *)))
  (is (vl::subtype-p '(unsigned-byte 12) 'signed-byte))
  (is (vl::subtype-p '(unsigned-byte 12) '(signed-byte 13)))
  (is (not (vl::subtype-p '(unsigned-byte 12) '(signed-byte 12))))
  (is (not (vl::subtype-p '(unsigned-byte 12) '(signed-byte 8))))

  ;; signed vs unsigned
  (is (not (vl::subtype-p 'signed-byte 'unsigned-byte)))

  ;; bits
  (is (vl::subtype-p 'bit 'unsigned-byte))
  (is (vl::subtype-p 'bit '(unsigned-byte 1)))
  (is (vl::subtype-p 'bit '(unsigned-byte 8))))


(test test-type-fixed-width
  "Test the classifiers."

  ;; fixed width
  (is (vl::fixed-width-p 'unsigned-byte))
  (is (vl::fixed-width-p 'signed-byte))
  (is (vl::fixed-width-p '(unsigned-byte 8)))
  (is (vl::fixed-width-p '(signed-byte 8)))
  (is (vl::fixed-width-p 'bit))

  ;; detailed classes
  (is (vl::unsigned-byte-p 'unsigned-byte))
  (is (not (vl::unsigned-byte-p 'signed-byte)))
  (is (vl::signed-byte-p 'signed-byte))

  ;; signed-byte-p excludes unsigned-byte, but subtype-p admits it
  (is (not (vl::signed-byte-p 'unsigned-byte)))
  (is (vl::subtype-p 'unsigned-byte 'signed-byte)))


(test test-type-lattice
  "Test the type lattice."

  ;; top type
  (is (vl::subtype-p 'unsigned-byte t))
  (is (vl::subtype-p '(unsigned-byte 8) t))
  (is (vl::subtype-p nil t))
  (is (vl::subtype-p t t))
  (is (not (vl::subtype-p t 'unsigned-byte)))

  ;; bottom type
  (is (vl::subtype-p nil 'unsigned-byte))
  (is (vl::subtype-p nil '(unsigned-byte 8)))
  (is (not (vl::subtype-p 'unsigned-byte nil)))
  (is (not (vl::subtype-p '(signed-byte 8) nil)))
  (is (not (vl::subtype-p t nil)))
  (is (vl::subtype-p nil nil)))


(test test-type-complex
  "Test complex type specifiers."

  ;; union types
  (is (vl::subtype-p 'unsigned-byte '(or unsigned-byte signed-byte)))
  (is (vl::subtype-p '(unsigned-byte 8) '(or unsigned-byte signed-byte)))
  (is (vl::subtype-p 'signed-byte '(or unsigned-byte signed-byte)))
  (is (vl::subtype-p '(signed-byte 8) '(or unsigned-byte signed-byte)))
  (is (vl::subtype-p '(unsigned-byte 8) '(or (unsigned-byte 16) (unsigned-byte 4))))
  (is (vl::subtype-p 'bit '(or unsigned-byte signed-byte)))
  (is (vl::subtype-p 'bit '(or (unsigned-byte 1) signed-byte)))
  (is (vl::subtype-p '(unsigned-byte 1) '(or bit signed-byte)))
  (is (not (vl::subtype-p '(unsigned-byte 12) '(or bit (signed-byte 8)))))
  (is (vl::subtype-p '(unsigned-byte 12) '(or bit (signed-byte 8) t)))
  (is (vl::subtype-p '(or unsigned-byte signed-byte) 'signed-byte))
  (is (vl::subtype-p '(or (unsigned-byte 8) (unsigned-byte 12)) '(unsigned-byte 16)))
  (is (vl::subtype-p '(or bit (unsigned-byte 8)) '(unsigned-byte 12)))
  (is (not (vl::subtype-p '(or (unsigned-byte 12) (unsigned-byte 8)) 'bit)))

  ;; type-of types
  ;;TODO: Add type-of tests
  )


;;; ---------- Least upper-bounds of types ----------

(test test-lub-fixed-width
  "Test we can form LUBs of fixed-with types."

  ;; unsigned vs unsigned
  (is (equal (vl::lub 'unsigned-byte 'unsigned-byte)
	     'unsigned-byte))
  (is (equal (vl::lub 'unsigned-byte '(unsigned-byte *))
	     'unsigned-byte))
  (is (equal (vl::lub '(unsigned-byte *) 'unsigned-byte)
	     'unsigned-byte))
  (is (equal (vl::lub '(unsigned-byte 12) '(unsigned-byte *))
	     'unsigned-byte))
  (is (equal (vl::lub '(unsigned-byte *) '(unsigned-byte 12))
	     'unsigned-byte))
  (is (equal (vl::lub '(unsigned-byte 16) '(unsigned-byte 12))
	     '(unsigned-byte 16)))

  ;; signed vs signed
  (is (equal (vl::lub 'signed-byte 'signed-byte)
	     'signed-byte))
  (is (equal (vl::lub 'signed-byte '(signed-byte *))
	     'signed-byte))
  (is (equal (vl::lub '(signed-byte *) 'signed-byte)
	     'signed-byte))
  (is (equal (vl::lub '(signed-byte 12) '(signed-byte *))
	     'signed-byte))
  (is (equal (vl::lub '(signed-byte *) '(signed-byte 12))
	     'signed-byte))
  (is (equal (vl::lub '(signed-byte 16) '(signed-byte 12))
	     '(signed-byte 16)))

  ;; signed vs unsigned
  (is (equal (vl::lub 'unsigned-byte 'signed-byte)
	     'signed-byte))
  (is (equal (vl::lub '(signed-byte 16) '(unsigned-byte 12))
	     '(signed-byte 16)))
  (is (equal (vl::lub '(unsigned-byte 16) '(signed-byte 12))
	     '(signed-byte 17)))

  ;; bits
  (is (equal (vl::lub 'bit 'unsigned-byte)
	     'unsigned-byte))
  (is (equal (vl::lub 'unsigned-byte 'bit)
	     'unsigned-byte))
  (is (equal (vl::lub '(unsigned-byte 1) 'bit)
	     '(unsigned-byte 1)))
  (is (equal (vl::lub 'bit '(unsigned-byte 1))
	     '(unsigned-byte 1))))


(test test-lub-fold
  "Test we can correctly fold LUB across several types."
  (is (equal (vl::lub '(unsigned-byte 8)
		     '(unsigned-byte 16)
		     '(unsigned-byte 32)
		     '(unsigned-byte 8))
	     '(unsigned-byte 32)))
  (is (equal (vl::lub '(unsigned-byte 8)
		     '(unsigned-byte 16)
		     '(signed-byte 32)
		     '(unsigned-byte 8))
	     '(signed-byte 32)))

  ;; include the lattice types
  (is (equal (vl::lub '(unsigned-byte 8)
		     '(unsigned-byte 16)
		     nil
		     '(unsigned-byte 8))
	     '(unsigned-byte 16)))
  (is (equal (vl::lub '(unsigned-byte 8)
		     '(unsigned-byte 16)
		     '(unsigned-byte 32)
		     '(unsigned-byte 8)
		     t)
	     t))
   (is (equal (vl::lub '(unsigned-byte 8)
		     '(unsigned-byte 16)
		     '(unsigned-byte 32)
		     nil
		     '(unsigned-byte 8)
		     t)
	     t)))


;; TODO: Test signed as well

(test test-lub-and
  "Test we can form ANDs of fixed-width types."
  (is (equal (vl::lub '(and (unsigned-byte 1) (unsigned-byte 2)) '(unsigned-byte 1))
	     '(unsigned-byte 3))))


;;; ---------- Solving type constraints ----------

(test test-initial-value-constraints
  "Test we get constraints for initial values."
  (with-new-frame
    (vl::declare-variable 'a '((initial-value 7)))

    (vl::constrain-initial-value 'a (current-frame) (current-frame))
    (vl::solve-type-constraints-and-declare 'a (current-frame))
    (is (subtype-p (vl::get-environment-property 'a 'type (current-frame))
		   '(unsigned-byte 3)))))


(test test-assignment-constraints
  "Test we can constrain when we have a (simulated) assignment."
  (with-new-frame
    (vl::declare-variable 'a '((initial-value 7)))

    ;; simulate assigning to the variable
    (vl::add-type-constraint 'a '(unsigned-byte 8))

    (vl::constrain-initial-value 'a (current-frame) (current-frame))
    (vl::solve-type-constraints-and-declare 'a (current-frame))
    (is (equal (vl::get-environment-property 'a 'type (current-frame))
	       '(unsigned-byte 8))))

    (with-new-frame
      (vl::declare-variable 'a '((initial-value 7)))

      ;; simulate two assignments
      (vl::add-type-constraint 'a '(unsigned-byte 8))
      (vl::add-type-constraint 'a '(unsigned-byte 16))

      (vl::constrain-initial-value 'a (current-frame) (current-frame))
      (vl::solve-type-constraints-and-declare 'a (current-frame))
      (is (equal (vl::get-environment-property 'a 'type (current-frame))
		 '(unsigned-byte 16))))

    (with-new-frame
      (vl::declare-variable 'a '((initial-value 7)))

      ;; simulate assignment with a union type
      (vl::add-type-constraint 'a '(or (unsigned-byte 8) (signed-byte 16)))

      (vl::constrain-initial-value 'a (current-frame) (current-frame))
      (vl::solve-type-constraints-and-declare 'a (current-frame))
      (is (vl::get-environment-property 'a 'type (current-frame))
	  '(signed-byte 16))))


(test test-assignment-nested-constraints-down
  "Test we can compute the types of variables referring to already-solved types."
  (with-new-frame
    (let ((p (vl:expand/vl '(let ((a 1))
			     (let ((b a))
			       b)))))
      (is (vl::subtype-p (vl:typecheck p) '(unsigned-byte 1))))))


(test test-assignment-nested-constraints-up
  "Test we can compute the types of variables referring to unsolved types."
  (with-new-frame
    (let ((p (vl:expand/vl '(let ((a 1))
			     (let ((b 3))
			       (setq a b))
			     a))))
      (is (vl::subtype-p '(unsigned-byte 2) (vl:typecheck p))))))


;; TODO: Needs fixing

(test test-assignment-nested-constraints-circular
  "Test we catch circular type dependencies, e.g., assignment involving the variable itself."
  (with-new-frame

    (vl::declare-variable 'a '((initial-value 1)))
    (let ((outer (current-frame)))
      (with-new-frame

	(vl::declare-variable 'b '((initial-value a)))
	(let ((inner (current-frame)))
	  ;; LET-style assignment
	  (vl::constrain-initial-value 'a outer (empty-environment))
	  (vl::constrain-initial-value 'b inner outer)

	  ;; simulate an assignment to A
	  (vl::add-type-constraint 'a `(type-of b ,inner))

	  (signals vl::circular-type-dependencies
	    (vl::solve-type-constraints-and-declare 'a outer)))))))


;;; ---------- + and * types ----------

(test test-plus-type
  "Test we can generate types for additions."
  (let ((p (vl::expand/vl '(let ((a 1)
				 (b 8))
			    (+ a b 1)))))
    (is (vl::subtype-p (vl::typecheck p) '(unsigned-byte 6))))

    (let ((p (vl::expand/vl '(let ((a 1)
				   (b 8))
			      (declare (width 10 a))
			      (+ a b 1)))))
      (is (vl::subtype-p (vl::typecheck p) '(unsigned-byte 12))))

    (let ((p (vl::expand/vl '(let ((a 1)
				   (b 8))
			      (declare (width 10 a))
			      (- a b 1)))))
      (is (vl::subtype-p (vl::typecheck p) '(signed-byte 13)))))


(test test-times-type
  "Test we can generate types for multiplications."
  (let ((p (vl::expand/vl '(let ((a 1)
				 (b 8))
			    (* a b 2)))))
    (is (vl::subtype-p (vl::typecheck p) '(unsigned-byte 7)))
    (is (not (vl::subtype-p (vl::typecheck p) '(unsigned-byte 6))))))
