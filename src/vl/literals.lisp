;; Literal constants
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

(in-package :vl)


;; ---------- Integers ----------

(defmethod bitwidth ((val integer))
  (bits-for-integer val))


(defmethod typecheck ((form integer))
  (let ((w (bitwidth form)))
    (if (< form 0)
	`(signed-byte ,w)
	`(unsigned-byte ,w))))


(defmethod free-variables ((form integer))
  nil)


(defmethod dependencies ((form integer))
  t)


(defmethod widthcheck ((form integer))
  form)


(defmethod float-let-blocks ((form integer))
  (list form '()))


(defmethod simplify-progn ((form integer))
  form)


(defmethod synthesise ((form integer))
  (as-literal (format nil "~s" form)))


(defmethod lispify ((form integer))
  form)
