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


(test test-sm-linear
  "Test we can create a simple linear machine."
  (let ((p (copy-tree '(let ((a 1))
			(vl:state-machine (posedge clk)
			 (0
			  (incf a)
			  (next-state 1))
			 (1
			  (if (> a 5)
			      (next-state 2)
			      (next-state 1)))
			 (2
			  t))))))

    (vl:typecheck p)

    )



  )
