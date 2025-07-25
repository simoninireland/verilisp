;; Tests of floating LET blocks pass
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


(test test-let-float
  "Test that nested LETs float."
  (let ((p (vl::expand/vl '(let ((a 1))
			    (declare (width 8 a))
			    (setq a 12)
			    (let ((b (+ a 1)))
			      (setq a (+ a b)))))))
    (vl::typecheck p)

    (destructuring-bind (form env)
	(vl::float-let-blocks p)
      (is (set-equal (vl::get-environment-names env)
		     '(a b)))

      ;; this may change when we float constant initial values
      (is (equal (vl::get-environment-property 'a 'initial-value env) 1))
      (is (equal (vl::get-environment-property 'b 'initial-value env) '(+ a 1))))))


(test test-let-float-markers
  "Test we retain constant (and other) markers when floating."
  (let ((p (vl::expand/vl  '(let ((a 1))
			     (declare (width 10 a))
			     (setq a 10)
			    (let ((b 12))
			      (declare (as constant b))
			      (setq a (+ b a)))))))
    (vl::typecheck p)
    (destructuring-bind (form env)
	(vl::float-let-blocks p)
      (is (eql (vl::get-environment-property 'b 'as env) 'constant))
      (is (not (eql (vl::get-environment-property 'a 'as env) 'constant))))))
