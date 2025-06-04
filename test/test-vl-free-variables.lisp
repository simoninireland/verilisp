;; Tests of free variable calculations
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


(test test-free-let
  "Test we can extract free variables from a LET binding."
  (let ((p (copy-tree '(let ((a 12))
			(setq a (+ a 1))))))
    (vl:typecheck p)
    (is (null (vl:free-variables p)))))


(test test-free-expression
  "Test we can axtract free variables from expressions."
  (is (set-equal (vl:free-variables '(+ 1 2 a 4 5))
		 '(a)))
  (is (set-equal (vl:free-variables '(+ 1 2 a 4 b))
		 '(a b)))
  (is (set-equal (vl:free-variables '(+ 1 2 a (+ 4 b 5)))
		 '(a b)))

  (is (null (vl:free-variables '(+ 1 2 4 5))))

  (is (set-equal (vl:free-variables '(>> 23 a))
		 '(a)))

  (is (set-equal (vl:free-variables '(logior 23 a))
		 '(a)))
  (is (set-equal (vl:free-variables '(logior 23 (make-bitfields (vl:bref b 5))))
		 '(b)))
  (is (set-equal (vl:free-variables '(logior 23 (make-bitfields (vl:bref b 4 :width 3))))
		 '(b)))
  (is (set-equal (vl:free-variables '(logior 23 (make-bitfields (vl:bref b 4 :width (+ c 1)))))
		 '(b c)))

  (is (set-equal (vl:free-variables '(setq a (+ b c)))
		 '(a b c)))

  (is (set-equal (vl:free-variables '(setf (bref b 2) 1))
		 '(b)))
  (is (set-equal (vl:free-variables '(setf (aref b 2) 33))
		 '(b))))

(test test-free-binders
  "Test we get all the free variables regardless of binders."
  (is (set-equal (vl:free-variables '(setq a (+ b c 1)))
		 '(a b c)))

  (is (set-equal (vl:free-variables '(let ((a 12))
				      (setq a (+ b c 1))))
		 '(b c))))
