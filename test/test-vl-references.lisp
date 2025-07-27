;; Tests of references to variables
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


(test test-variable-scope
  "Test we can see variables in scope."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 12))
						     ;;(declare (type (unsigned-byte 8) a))
						     a)))
		    '(unsigned-byte 8))))


(test test-variable-not-scope
  "Test we can see variables not in scope."
  (signals (vl::unknown-variable)
    (vl::typecheck (vl::expand/vl '(let ((a 12))
				  b)))))


(test test-legalise
  "Test we legalise variable names at synthesis."
  (let ((p (vl::expand/vl '(let ((a 1)
				(b-c-d 2))
			   (setq a (+ b-c-d 1))))))
    (vl::typecheck p)

    (let ((s (make-array '(0) :element-type 'base-char
			      :fill-pointer 0 :adjustable t)))
      (with-output-to-string (str s)
	(vl::with-synthesis-to-stream str
	  (vl::synthesise p)))

      ;; check for no illegal identifiers -- rough, just
      ;; using a pattern search
      (is (null (search s "b-c-d"))))))


(test test-legalise-modules
  "Test that we synthesise module elements correctly."
  (vl::clear-module-registry)

  (vl::defmodule/vl clock/123 (clk-in clk-out
				      &key (p 1) (q-r 2))
    (declare (type bit clk-in clk-out)
	     (direction in clk-in)
	     (direction out clk-out))

    (let ((a-b-c 12)
	  (d 19))
      (setq a-b-c (+ 3 a-b-c d q-r)))
    (setq clk-out clk-in))

  (let ((s (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (str s)
      (vl::with-synthesis-to-stream str
	(vl::synthesise (vl::get-module 'clock/123))))

    ;; check for no illegal identifiers -- rough, just
    ;; using a pattern search
    (is (null (search s "clk-in")))
    (is (null (search s "clock/123")))
    (is (null (search s "q-r")))
    (is (null (search s "a-b-c")))))


(test test-legalise-modules-instanciate
  "Test that we synthesise module instanciation correctly."
  (vl::clear-module-registry)

  (defmodule/vl clock/123 (clk-in
			   clk-out
			   &key (p 1) (q-r 2))
    (declare (type bit clk-in clk-out)
	     (direction in clk-in)
	     (direction out clk-out))

    (let ((a-b-c 12)
	  (d 19))
      (setq a-b-c (+ 3 a-b-c d q-r)))
    (setq clk-out clk-in))

  (let ((s (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t))
	(p (vl::expand/vl '(let ((iclk 0)
				(oclk 0))
			    (declare (type bit iclk oclk)
			     (as wire iclk oclk))
			    (let ((clock (make-instance 'clock/123 :clk-in iclk :clk-out oclk)))
			      (setq iclk 1))))))

    (vl::typecheck p)
    (with-output-to-string (str s)
      (vl::with-synthesis-to-stream str
	(vl::synthesise p)))

    ;; check for no illegal identifiers -- rough, just
    ;; using a pattern search
    (is (null (search s "clk-in")))
    (is (null (search s "clk-out")))
    (is (null (search s "clock/123")))))
