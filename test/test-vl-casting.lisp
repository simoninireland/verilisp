;; Tests of type assertions and casting
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


;; ---------- Type assertions  ----------

(test test-typecheck-the
  "Test we can typecheck THE."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(the (unsigned-byte 8) 12)))
		    '(unsigned-byte 8)))

  (signals (vl:type-mismatch)
    (vl:typecheck (vl:expand/vl '(the (unsigned-byte 8) 1230)))))


(test test-synthesise-the
  "Test we can synthesise a value whose type has been asserted explicitly."
  (is (vl:synthesise '(the (unsigned-byte 8) 12))))



;; ---------- Type coercions ----------

(test test-typecheck-coerce
  "Test we can typecheck coercions."
  (is (vl:subtype-p (vl:typecheck '(coerce 12 (unsigned-byte 8)))
		    '(unsigned-byte 8)))

  ;; type is the type coerced to, not of the value (unlike for the)
  (is (not (vl:subtype-p (vl:typecheck (vl:expand/vl '(coerce 12 (unsigned-byte 8))))
			 '(unsigned-byte 4))))

  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(coerce 12 (signed-byte 5))))
		    '(signed-byte 5)))

  ;; can't coerce anything not fixed-width
  (signals (vl:coercion-mismatch)
    (vl:typecheck (vl:expand/vl '(coerce (make-array '(10) :element-type (unsigned-byte 8))
				  (signed-byte 58)))))

  ;; can coerce elements though
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(let ((a (make-array '(10) :element-type (unsigned-byte 8))))
						  (coerce (aref a 3) (signed-byte 16)))))
		    '(signed-byte 16))))


(test test-synthesise-coerce-equal-width-same-sign
  "Test we can coerce equal-width and -signedness numbers."
  (vl::with-new-frame
    (vl::declare-variable 'a '((:type (unsigned-byte 8))))
    (is (vl:synthesise '(coerce a (unsigned-byte 8))))

    (vl::declare-variable 'b '((:type (signed-byte 8))))
    (is (vl:synthesise '(coerce b (signed-byte 8))))

    (vl::declare-variable 'c '((:type (unsigned-byte 8))))
    (is (vl:synthesise '(coerce c (signed-byte 8))))

    (vl::declare-variable 'd '((:type (signed-byte 8))))
    (is (vl:synthesise '(coerce d (unsigned-byte 8))))))


(test test-synthesise-coerce-narrower-same-sign
  "Test we can coerce narrower numbers."
  (vl::with-new-frame
    (vl::declare-variable 'a '((:type (unsigned-byte 16))))
    (is (vl:synthesise '(coerce a (unsigned-byte 8))))

    (vl::declare-variable 'b '((:type (signed-byte 16))))
    (is (vl:synthesise '(coerce b (signed-byte 8))))

    (vl::declare-variable 'c '((:type (unsigned-byte 16))))
    (is (vl:synthesise '(coerce c (signed-byte 8))))

    (vl::declare-variable 'd '((:type (signed-byte 16))))
    (is (vl:synthesise '(coerce d (unsigned-byte 8))))))


(test test-synthesise-coerce-wider-same-sign
  "Test we can coerce wider numbers."
  (vl::with-new-frame
    (vl::declare-variable 'a '((:type (unsigned-byte 8))))
    (is (vl:synthesise '(coerce a (unsigned-byte 16))))

    (vl::declare-variable 'b '((:type (signed-byte 8))))
    (is (vl:synthesise '(coerce b (signed-byte 16))))

    (vl::declare-variable 'c '((:type (unsigned-byte 8))))
    (is (vl:synthesise '(coerce c (signed-byte 16))))

    (vl::declare-variable 'd '((:type (signed-byte 8))))
    (is (vl:synthesise '(coerce d (unsigned-byte 16))))))


(test test-synthesise-coerce-real
  "Test coercions against a real expression."
  (let ((p (vl:expand/vl '(let ((instr 0 :width 32)
				a)
			   (let ((bs (vl:bref instr 31 :end 20)))
			     (let ((Iimm (coerce bs
						 (signed-byte 32))))
			       (setq a Iimm)))))))

    (vl:typecheck p)
    (is (vl:synthesise p))))
