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


(defpassmethod read-variables (bref place start &key end width)
  (declare (optimize debug))

  (let ((place-rws (if (symbolp place)
		       ;; place targets a variable directly
		       (list place)

		       ;; place is complex, recurse into it
		       (read-variables place)))
	(sel-rws (union-all (read-variables (remove-nulls (list start end width))))))

    (union place-rws sel-rws)))


(defpassmethod compute-dependencies (bref &rest args)
  (dolist (n (read-variables `(bref ,@args)))
    (set-variable-property n 'read t)))


(defpassmethod read-variables-setf (bref place start &key end width)
  (declare (optimize debug))

  ;; (let* ((params (foldr #'union (read-variables (remove-nulls (list start end width))) '()))
  ;;	 (vals (read-variables value))
  ;;	 (val-width (union params vals)))

  ;;   (cond ((symbolp place)
  ;;	   val-width)

  ;;	  ((integerp place)
  ;;	   ;; legitimate to have a constant as the value
  ;;	   params)

  ;;	  (t
  ;;	   (destructuring-bind (psel &rest pselargs)
  ;;	       place
  ;;	     (union val-width
  ;;		    (read-variables-setf psel val pselargs))))))

  (let* ((params (union-all (read-variables (remove-nulls (list start end width))))))

    (if (atom place)
	params

	;; compound place, recurse
	(union params (read-variables-setf place)))))


(defpassmethod written-variables-setf (bref place start &key end width)
  (cond ((symbolp place)
	 ;; variable, written to
	 (list place))

	((integerp place)
	 ;; constant, nowhere to write
	 nil)

	(t
	 ;; complex place, recurse into it
	 (written-variables-setf place))))


(defpassmethod generalised-place-p (bref place start &key end width)
  (generalised-place-p place))


(defpassmethod compute-type (bref place start &key end width)
  (declare (optimize debug))

  ;; check syntax
  (when (and (not (null width))
	     (not (null end)))
    (error 'syntax-error :form `(bref ,place ,start :width ,width :end ,end)
			 :hint "Provide at most one of :END and :WIDTH)"))

  ;; extract width
  (if (null width)
      (if (null end)
	  ;; default to accessing the single START bit
	  (setq width 1)

	  (progn
	    ;; start bit and end must be a constants
	    (ensure-static start)
	    (ensure-static end)

	    ;; compute width from start and end
	    (setq width `(1+ (- ,start ,end)))))

      (progn
	;; width must be constant
	(let ((w (eval-in-static-environment width)))
	  ;; if width is not 1, start must be constant
	  (if (> w 1)
	      (ensure-static start)))))

  ;; recurse into the complex place
  (compute-type place)

  ;; type depends on the number of bits extracted
  `(unsigned-byte ,width))


(defpassmethod apply-type-constraints (bref place start &key end width)
  (declare (optimize debug))

  ;; we use the actual values in the constraints
  (if (null width)
      (if (null end)
	  ;; default to accessing the single START bit
	  (setq width 1)

	  ;; compute width from start and end
	  (setq width (1+ (- start (eval-in-static-environment end)))))

      ;; compute width
      (setq width (eval-in-static-environment width)))

  ;; sanity check bounds
  (when (< width 1)
    ;; negative width
    (error 'value-mismatch :expected "non-negative number"
			   :got width
			   :hint "Make sure width bits are positive"))
  (when (static-p start)
    ;; we have a static start bit so we can check width against it
    (let ((s (eval-in-static-environment start)))
      (when (> width (1+ s))
	(error 'value-mismatch :expected (1+ s)
			       :got width
			       :hint "Make sure width bits can be extracted"))))

  ;; check whether variable should be widened
  (let ((ty (eval-type (compute-type place))))
    (let ((vw (bitwidth ty)))

      (when (> width vw)
	;; signal to allow this to be picked up
	(warn 'type-mismatch :expected vw
			     :got width
			     :hint "Width wider than base variable")))))


(defpassmethod simple-expression-form-p (bref place start &key end width)
  (and (simple-expression-form-p start)
       (or (null end)
	   (simple-expression-form-p end))
       (or (null width)
	   (simple-expression-form-p width))))


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
  (let ((l (eval-in-static-environment `(+ 1 (- ,start ,end)))))
    `(logand (ash ,(lispify place) (- ,end)) (1- (ash 1 ,l)))))
