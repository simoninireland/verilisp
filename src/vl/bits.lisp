;; Bitwise access to variables
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
(declaim (optimize debug))


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


(defmethod generalised-place-sexp-p ((fun (eql 'bref)) args)
  t)


(defmethod typecheck-sexp ((fun (eql 'bref)) args)
  (destructuring-bind (var start &key end width)
      args
    ;; we use the actual values in the type
    (setq start (eval-in-static-environment start))
    (when width
      (setq width (eval-in-static-environment width)))
    (when end
      (setq end (eval-in-static-environment end)))

    ;; check everything is positive
    (unless (>= start 0)
      (error 'value-mismatch :expected "the non-negative integers" :got start
			     :hint "Start bit must be negative"))
    (unless (or (null end)
		(>= end 0))
      (error 'value-mismatch :expected "the non-negative integers" :got end
			     :hint "End bit must be non-negative"))
    (unless (or (null width)
		(> width 0))
      (error 'value-mismatch :expected "the positive integers" :got width
			     :hint "Width must be positive"))

    ;; default to accessing the single START bit
    (if (null width)
	(if (null end)
	    (setq width 1)
	    (setq width (1+ (- start end)))))

    (let ((tyvar (typecheck var)))
      (setq end (compute-end-bit start end width))

      ;; check whether variable should be widened
      (let ((l (1+ (- start end)))
	    (vw (bitwidth tyvar)))
	(when (> l vw)
	  ;; signal to allow this to be picked up
	  (warn 'type-mismatch :expected vw
			       :got l
			       :hint "Width greater than base variable")

	  ;; add a constraint
	  (add-type-constraint var `(unsigned-byte ,l)))

	;; width is the nunmber of bits extracted
	`(unsigned-byte ,l)))))


(defmethod free-variables-sexp ((fun (eql 'bref)) args)
  (declare (optimize debug))

  (destructuring-bind (var start &key end width)
      args
    (foldr #'union (mapcar #'free-variables (remove-nulls (list var start end width))) '())))



(defmethod synthesise-sexp ((fun (eql 'bref)) args)
  (destructuring-bind (var start &key end width)
      args

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
	    (setq end `(+ ,start (- ,width 1)))))

    (synthesise var)
    (as-literal "[ ")
    (synthesise start)
    (when width
      (as-literal " : ")
      (synthesise end))
    (as-literal " ]")))


(defmethod lispify-sexp ((fun (eql 'bref)) args)
  (destructuring-bind (var start &key end width)
      args
    (let ((l (eval-in-static-environment `(+ 1 (- ,start ,end)))))
      `(logand (ash ,(lispify var) (- ,end)) (1- (ash 1 ,l))))))
