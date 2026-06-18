;;;; References to variables
;;;;
;;;; Copyright (C) 2024--2025 Simon Dobson
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


(defpassmethod expand-macros ((form symbol))
  (declare (optimize debug))

  (if (symbol-macro-declared-p form)
      ;; reference is a symbol macro, expand it
      (let ((newform (variable-property form 'initial-value)))
	(let ((p (apply newform (list form))))

	  ;; recurse into the expanded form
	  (expand-macros p)))

      ;; symbol is a variable, leave unchanged
      form))


(defpassmethod compute-type ((form symbol))
  (if-let ((f (get-frame-declaring form (current-frame))))
    (or (get-frame-property form 'type f :default nil)
	`(type-of ,form ,f))

    ;; not declared
    (error 'unknown-variable :variable form)))


(defpassmethod compute-variable-types ((form symbol))
  nil)


(defpassmethod read-variables ((form (eql nil)))
  nil)


(defpassmethod read-variables ((form symbol))
  (list form))


(defpassmethod read-variables-setf ((form symbol))
  nil)


(defpassmethod written-variables-setf ((form symbol))
  (list form))


(defpassmethod generalised-place-p ((form symbol))
  t)


(defpassmethod float-let-blocks ((form symbol))
  (list form '()))


(defpassmethod simplify-progn ((form symbol))
  form)


(defpassmethod synthesise ((form symbol))
  (as-literal (format nil "~(~a~)" (ensure-legal-identifier (symbol-name form)))))


(defpassmethod lispify ((form symbol))
  form)
