;; Tests of WITH-BITFIELDS macro
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


(test test-extract-runs
  "Test we can extract runs of symbols from a list."
  (is (null (vl::extract-runs '())))
  (is (equal (vl::extract-runs '(a))
	     '((a 0 0))))
  (is (equal (vl::extract-runs '(a a a))
	     '((a 2 0))))
  (is (equal (vl::extract-runs '(a a a b b b))
	     '((a 5 3) (b 2 0))))
  (is (equal (vl::extract-runs '(a a a b a a))
	     '((a 5 3) (b 2 2) (a 1 0))))
  (is (equal (vl::extract-runs '(a a a b a a c))
	     '((a 6 4) (b 3 3) (a 2 1) (c 0 0)))))


;; Should this function be in utils?
(test test-duplicate-keys
  "Test we can detect duplicate keys in alists."
  (is (vl::duplicate-keys-p '((a 12) (b 13) (a 1))))
  (is (vl::duplicate-keys-p '((a 12) (a 13))))

  (is (not (vl::duplicate-keys-p '())))
  (is (not (vl::duplicate-keys-p '((a 12)))))
  (is (not (vl::duplicate-keys-p '((a 12) '(b 12))))))


(test test-extract-bitfields
  "Test we can extract bitfields from patterns."
  (is (equal (vl::extract-bitfields '(a))
	     '((a 0 0))))
  (is (equal (vl::extract-bitfields '(1 a))
	     '((1 1 1) (a 0 0))))
  (signals (vl:bitfield-mismatch)
    (vl::extract-bitfields '(1 a 1 a))))


(test test-with-bitfields-simple
  "Test we can extract bitfields."
  (let ((p '(let ((a #2r1001011010))
	     (vl::with-bitfields (a a a b b b c)
		 a
	       (setf a (+ b c))))))
    (is (subtypep (vl:typecheck (vl:expand-macros-in-environment p))
		  '(unsigned-byte 10)))))


(test test-with-bitfields-typo
  "Test we catch the typo of forgetting the matching value."
  (signals (vl:not-synthesisable)
    (vl:expand-macros-in-environment '(vl:with-bitfields (a b c)
			 ;; no argument to match against,
			 ;; just a one-form body
			 (setq a b)))))


(test test-with-bitfields-extensive
  "Test a more extensive example of with-bitfields."
  (let ((p `(vl:module test ((clk :type (unsigned-byte 1) :direction :in))
			(let ((ctrl 0 :type (unsigned-byte 6))
			      (a 0 :type (unsigned-byte 1)))
			  (vl:with-bitfields (a b c
						 d e f)
			      ctrl
			    (setf a d))))))
    (is (subtypep (vl:typecheck (vl:expand-macros-in-environment p))
		  'vl::module-interface))))
