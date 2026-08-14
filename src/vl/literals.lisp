;;;; Literal constants
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

(in-package :verilisp/core)


;;; ---------- Integers ----------

(defpassmethod compute-type ((form integer))
  (let ((w (bits-for-integer form)))
    (if (< form 0)
	`(signed-byte ,w)
	`(unsigned-byte ,w))))


(defpassmethod read-variables ((form integer))
  '())


(defpassmethod float-let-blocks ((form integer))
  (list form '()))


(defpassmethod simplify-progn ((form integer))
  form)


(defpassmethod synthesise ((form integer))
  (as-literal (format nil "~s" form)))


(defpassmethod lispify ((form integer))
  form)
