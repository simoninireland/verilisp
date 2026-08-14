;;;; Bitwise access to variables
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
(declaim (optimize debug))

;;; Bitwise access follows the same scheme as aray access, defining a new
;;; generic place BREF to extracting bits.

;;; TODO: There is probably something to do with the isomorphism between
;;; UNSIGNED-BYTE types and (ARRAY 'BIT) types. Common Lisp has entirely
;;; separate functions for these two cases, which we can of course replicate:
;;; but we can also coerce one to the other, which Common Lisp doesn't allow.

(defun compute-end-bit (start end width)
  "Compute the end bit given START, END, and WIDTH."
  (if (null end)
      (if width
	  ;; no end set, extract from width if present
	  (progn
	    (setq end (1+ (- start width)))
	    (if (< end 0)
		(warn 'type-mismatch :expected 0
				     :got end
				     :hint "Width greater than the number of remaining bits")
		end))

	  ;; default is to the end of the pattern
	  0)

      (if width
	  ;; if both are set, width and end must agree
	  (if (/= width (1+ (- start end)))
	      (warn 'type-mismatch :expected (1+ (- start end))
				   :got width
				   :hint "Explicit width does not agree with start and end positions")
	      end)

	  ;; otherwise just use the given end
	  end)))


(defpassmethod generalised-place-p (bref place start &key end width)
  t)


(defpassmethod read-variables (bref place start &key end width)
  (declare (optimize debug))

  ;; include any variables read in PLACE, which might itself be a generalised place
  ;; rather than being just a variable
  (union-all (mapcar #'read-variables (remove-nulls (list place start end width)))))


(defpassmethod read-variables-setf (bref place start &key end width)
  ;; do not include variables read in PLACE
  (union-all (mapcar #'read-variables (remove-nulls (list start end width)))))


(defpassmethod written-variables-setf (bref place start &key end width)
  (list place))


(defpassmethod compute-type (bref place start &key end width)
  (declare (optimize debug))

  ;; check syntax
  (when (and (not (null width))
	     (not (null end)))
    (error 'syntax-error :hint "Provide at most one of :END and :WIDTH)"))

  ;; extract width
  (if (null width)
      (if (null end)
	  ;; default to accessing the single START bit
	  (setq width 1)

	  ;; compute width from start and end
	  (setq width (1+ (- (eval-in-static-environment start)
			     (eval-in-static-environment end)))))

      ;; width must be constant
      (let ((w (eval-in-static-environment width)))
	;; if width is not 1, start must be constant
	(if (> w 1)
	    (ensure-static start))
	(setq width w)))

  ;; sanity check bounds
  (when (< width 1)
    ;; negative width
    (error 'value-mismatch :expected "non-negative number"
			   :got width
			   :hint "Make sure width bits are positive"))

  ;; recurse into the place
  (compute-type place)

  ;; type depends on the number of bits extracted
  `(unsigned-byte ,width))


(defpassmethod simple-expression-p (bref place start &key end width)
  (and (simple-expression-p place)
       (simple-expression-p start)
       (or (null end)
	   (simple-expression-p end))
       (or (null width)
	   (simple-expression-p width))))


(defpassmethod synthesise (bref place start &key end width)
  ;; compute bounds give the optional arguments
  (if (null width)
      (if (null end)
	  ;; one bit at start
	  (setq end start)

	  ;; set width to reflect end
	  ;; (not actually used, just needs to be non-nil)
	  (setq width `(+ (- start end) 1)))

      (if (null end)
	  ;; compute end based on width
	  (setq end `(- ,start (- ,width 1)))))

  (synthesise place)
  (as-literal "[ ")
  (synthesise start)
  (when width
    (as-literal " : ")
    (synthesise end))
  (as-literal " ]"))


(defpassmethod lispify (bref place start &key end width)
  `(logand (ash ,(lispify place) (- ,end)) (1- (ash 1 ,(+ 1 (- start end))))))
