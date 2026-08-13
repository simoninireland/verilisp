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


;; Types and widths

(test test-literal-widths
  "Test we can extract the types of literals."
  (is (vl::subtype-p (vl::typecheck 2)
		    '(unsigned-byte 2)))

  (is (not (vl::subtype-p (vl::typecheck -2)
			 '(signed-byte 2)))) ;; no room for the sign bit
  (is (vl::subtype-p (vl::typecheck -2)
		    '(signed-byte 3))))
