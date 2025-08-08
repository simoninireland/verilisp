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

(in-package :verilisp/core)


(defmethod compute-type ((form symbol))
  (if-let ((f (get-frame-declaring form (current-frame))))
    (or (get-frame-property form 'type f :default nil)
	`(type-of ,form ,f))

     ;; not declared
    (error 'unknown-variable :variable form)))


(defmethod read-variables ((form symbol))
  (list form))


(defmethod read-variables-setf ((selector symbol) val selectorargs)
  (union (list selector)
	 (read-variables val)))


(defmethod written-variables-setf ((selector symbol) val selectorargs)
  (list selector))


(defmethod compute-dependencies ((form symbol))
  (set-variable-property form 'read t))


(defmethod generalised-place-p ((form symbol))
  (writeable-p form))


(defmethod float-let-blocks ((form symbol))
  (list form '()))


(defmethod simplify-progn ((form symbol))
  form)


(defmethod synthesise ((form symbol))
  (as-literal (format nil "~(~a~)" (ensure-legal-identifier (symbol-name form)))))


(defmethod lispify ((form symbol))
  form)
