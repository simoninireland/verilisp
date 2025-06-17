;; Tests of state machine construction
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


(test test-tagbody-compled-form
  "Test we can construct a compiled form of TAGBODY."
  (let* ((p (copy-tree '(tagbody
			 one
			 (setq a 1)
			 (setq b 2)
			 two
			 (setq b 0))))
	 (q (copy-tree `(let (a b)
			  ,p))))

    (vl:typecheck q)
    (is (tree-equal (vl::compile-state-machine 'sss (vl::extract-tagbody-states (cdr p)))
		    '(let ((one 0 :as :constant)
			   (two 1 :as :constant))
		      (let ((sss one))
			(case (sss)
			  (one
			   (setq a 1)
			   (setq b 2))
			  (two
			   (setq b 0)))))))))


(test test-go-outside-tagbody
  "Test we can catch a GO out of context."
  (let ((p (copy-tree '(let ((one 1))
			(go one)))))

    (signals (vl:syntax-error)
      (vl:typecheck p))))


(test test-synthesise-tagbody
  "Teat we can synthesise a TAGBODY."
  (let* ((p (copy-tree '(tagbody
			 one
			 (setq a 1)
			 (setq b 2)
			 two
			 (setq b 0)
			 (go one))))
	 (q (copy-tree `(let (a b)
			  ,p))))

    (vl:typecheck q)
    (is (vl:synthesise q))))
