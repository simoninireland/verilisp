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

(in-package :verilisp/core)
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


(defmethod read-variables-sexp ((fun (eql 'bref)) args)
  (declare (optimize debug))

  (destructuring-bind (place start &key end width)
      args

    (let ((place-rws (if (symbolp place)
			 ;; place targets a variable directly
			 (list place)

			 ;; place is complex, recurse into it
			 (read-variables place)))
	  (sel-rws (foldr #'union (read-variables (remove-nulls (list start end width))) '())))

      (union place-rws sel-rws))))


(defmethod compute-dependencies-sexp ((fun (eql 'bref)) args)
  (dolist (n (read-variables `(bref ,@args)))
    (set-variable-property n 'read t)))


(defmethod read-variables-setf ((selector (eql 'bref)) val selectorargs)
  (declare (optimize debug))

  (destructuring-bind (place start &key end width)
      selectorargs

    (let ((val-width (union (foldr #'union (read-variables (remove-nulls (list start end width))) '())
			    (read-variables val))))
      (if (symbolp place)
	  val-width

	  (destructuring-bind (psel &rest pselargs)
	      place
	    (union val-width
		   (read-variables-setf psel val pselargs)))))))


(defmethod written-variables-setf ((selector (eql 'bref)) val selectorargs)
  (destructuring-bind (place start &key end width)
      selectorargs

    (if (listp place)
	;; complex place, recurse into it
	(destructuring-bind (psel &rest pselargs)
	    place
	  (written-variables-setf psel val pselargs))

	;; variable, this is written to
	(list place))))


(defmethod generalised-place-sexp-p ((selector (eql 'bref)) selectorargs)
  (destructuring-bind (place start &key end width)
      selectorargs
    (generalised-place-p place)))


(defmethod compute-type-sexp ((fun (eql 'bref)) args)
  (declare (optimize debug))

  (destructuring-bind (place start &key end width)
      args

    ;; check syntax
    (when (and (not (null width))
	       (not (null end)))
      (error 'syntax-error :form (cons fun args)
			   :hint "Provide at most one of :END and :WIDTH)"))

    ;; extract width
    (if (null width)
	(if (null end)
	    ;; default to accessing the single START bit
	    (setq width 1)

	    ;; compute width from start and end
	    (setq width `(1+ (- ,start ,end)))))

    (if (symbolp place)
	(progn
	  ;; constrain the written variable
	  (add-type-constraint place `(unsigned-byte (1+ ,start))))

	;; recurse into the complex place
	(compute-type place))

    ;; type depends on the number of bits extracted
    `(unsigned-byte ,width)))


(defmethod apply-type-constraints-sexp ((fun (eql 'bref)) args)
  (declare (optimize debug))

  (destructuring-bind (place start &key end width)
      args
    ;; we use the actual values in the constraints
    (setq start (eval-in-static-environment start))
    (if (null width)
	(if (null end)
	    ;; default to accessing the single START bit
	    (setq width 1)

	    ;; compute width from start and end
	    (setq width (1+ (- start (eval-in-static-environment end)))))

	(setq width (eval-in-static-environment width)))

    ;; sanity check bounds
    (when (or (< width 0)
	      (> width (1+ start)))
      (error 'value-mismatch :expected (1+ start)
			     :got width
			     :hint "Make sure width bits can be extracted"))

    ;; check whether variable should be widened
    (let ((ty (compute-type place)))
      (let ((vw (bitwidth ty)))

	(when (> width vw)
	  ;; signal to allow this to be picked up
	  (warn 'type-mismatch :expected vw
			       :got width
			       :hint "Width wider than base variable"))))))


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
