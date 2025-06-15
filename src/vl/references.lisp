;; References to variables
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


(defmethod bitwidth ((val symbol))
  (let ((tys (safe-car-cdr (get-type val))))
    (bitwidth-type (car tys) (cadr tys))))


(defmethod typecheck ((form symbol))
  (get-type form))


(defmethod free-variables ((form symbol))
  (list form))


(defmethod dependencies ((form symbol))
  nil)


(defmethod float-let-blocks ((form symbol))
  (list form '()))


(defmethod simplify-progn ((form symbol))
  form)


(defmethod synthesise ((form symbol))
  (as-literal (format nil "~(~a~)" (ensure-legal-identifier (symbol-name form)))))


(defmethod lispify ((form symbol))
  form)
