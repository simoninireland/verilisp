;;;; Arrays of variables
;;;;
;;;; Copyright (C) 2024--2026 Simon Dobson
;;;;
;;;; This file is part of verilisp, a Common Lisp DSL for hardware design
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

;;; Array types, constructors, and access.
;;; An array is accessed using AREF, which is a generic place suitable for SETF.


;;; ---------- Array type ----------

(defmethod subtype-type ((ty1tag (eql 'array)) ty1args
			 (ty2tag (eql 'array)) ty2args)
  (declare (optimize debug))

  (cond ((null ty1args)
	 (null ty2args))

	((null ty2args)
	 t)

	(t
	 (destructuring-bind (et1 &rest et1args)
	     ty1args
	   (destructuring-bind (et2 &rest et2args)
	       ty2args

	     ;; arrays match covariantly on element types, ignoring shapes
	     (subtype-p et1 et2))))))


(defmethod lub-type ((ty1tag (eql 'array)) ty1args
		     (ty2tag (eql 'array)) ty2args)
  (cond ((null ty1args)
	 (if (null ty2args)
	     'array
	     (construct-type ty2tag ty2args)))

	 ((null ty2args)
	  (construct-type ty1tag ty1args))

	 (t
	  (destructuring-bind (ety1 &optional esh1)
	      ty1args
	    (destructuring-bind (ety2 &optional esh2)
		ty2args

	      (let ((evty1 (eval-type ety1))
		    (evty2 (eval-type ety2))
		    (sh (cond ((and (null esh1)
				    (null esh2))
			       '(0))

			      ((null esh1)
			       esh2)
			      ((null esh2)

			       esh1)

			      (t
			       ;; maximum of the two lengths
			       ;; TODO: need to fix the shapes for multiple dimensions
			       (list (max (car esh1) (car esh2)))))))

		;; LUB has the LUB element type
		`(array ,(lub evty1 evty2) ,sh)))))))


(defmethod representable-type-sexp-p ((tytag (eql 'array)) tyargs)
  (if (null tyargs)
      ;; an unconstrained array type is not representable
      nil

      (destructuring-bind (ty &optional sh)
	  tyargs

	;; an array is representable if its element type is
	(representable-type-p (lub ty)))))


;;; ---------- Array initialisation data ----------

;;; TODO: Should allow multi-dimensionsal shapes

(defun valid-array-shape-p (shape)
  "Test that SHAPE is a valid array shape.

At the moment this means a one-dimensional list of integers
whose values are statically determinable."
  (handler-bind ((error nil))
    (and (listp shape)
	 (= (length shape) 1)
	 (every #'eval-in-static-environment shape))))


(defun ensure-valid-array-shape (shape)
  "Ensure SHAPE is a valid array shape."
  (unless (valid-array-shape-p shape)
    (error 'not-synthesisable :hint "Arrays must be 1d with dimensions known statically")))


(defun data-has-shape-p (data shape)
  "Test whether DATA has the given SHAPE."
  (and (= (length shape) 1)
       (= (length data) (car shape))))


(defun ensure-data-has-shape (data shape)
  "Ensure DATA has the given SHAPE."
  (unless (data-has-shape-p data shape)
    (error 'shape-mismatch :expected shape
			   :hint "Ensure initial contents have the right shape")))


;;; ---------- Array construction ----------

(defmacro unquote (place)
  "Remove any leading quote from the data in PLACE.

This is mainly used to preserve compatability with Lisp, where
these forms need to be quoted. We *allow* them to be quoted in
Verilisp, but don't *require* it."
  `(if (and (not (null ,place))
	    (listp ,place)
	    (eql (car ,place) 'quote))
       (setq ,place (cadr ,place))))


(defun array-element-width (form)
  "Return the width of the elements of array constructor FORM."
  (if-let ((m (member :element-type (cdr form))))
    ;; use the element width of there is one
    (bitwidth (safe-cadr m) '())

    ;; otherwise the default
    *default-register-width*))


(defpassmethod compute-type (make-array shape &key (initial-element 0)
					initial-contents
					element-type)
  ;; skip an initial quotes, allowed for Lisp compatability
  (unquote shape)
  (unquote element-type)
  (unquote initial-contents)

  ;; check shape
  (ensure-valid-array-shape shape)

  ;; initialise type from the initial element unless an explicit element type is provided
  (unless element-type
    (setq element-type (compute-type initial-element)))

  `(array ,element-type ,shape))


(defpassmethod apply-type-constraints (make-array shape &key (initial-element 0)
						  initial-contents
						  element-type)
  ;; skip an initial quotes, allowed for Lisp compatability
  (unquote shape)
  (unquote initial-contents)

  ;; initial contents must either match the size of the array
  ;; or identify a file
  ;; TODO: We need to type-check the initial contents, which we can't
  ;; at the moment as we don't know the inferred element type
  (if initial-contents
      (if (listp initial-contents)
	  (cond ((eql (car initial-contents) :file)
		 ;; nothing to do at the moment
		 t)

		(t
		 ;; check all elements of literal data
		 (ensure-data-has-shape initial-contents shape))))))


(defpassmethod read-variables (make-array &rest args)
  '())


(defpassmethod compute-dependencies (make-array &rest args)
  (:schema constant-form))


(defun rebuild-options (ns vs)
  "Build an options list from NS where the corresponding value in VS is non-null."
  (flet ((key-value (opts nv)
	   (destructuring-bind (n v)
	       nv
	     (if (null v)
		 opts
		 (append opts (list n v))))))

    (foldr #'key-value (zip ns vs) '())))


;;; Only works for one-dimensional arrays at the moment
;;; TODO: xpand constants

(defun synthesise-array-init-from-data (n data shape)
  "Return the initialisation of N using DATA with the given SHAPE."
  (flet ((initialise-array-from-data ()
	   (dotimes (i (car shape))
	     (synthesise n)
	     (as-literal "[")
	     (synthesise i)
	     (as-literal "] = ")
	     (synthesise (nth i data))
	     (as-literal ";" :newline t))))

    (add-module-late-initialisation #'initialise-array-from-data)))


(defun synthesise-array-init-from-value (n v shape)
  "Insert V into all elements of N using SHAPE.

This is implemented using late initialisation."
  (flet ((initialise-array-from-value ()
	   (dotimes (i (car shape))
	     (synthesise n)
	     (as-literal "[")
	     (synthesise i)
	     (as-literal "] = ")
	     (synthesise v)
	     (as-literal ";" :newline t))))

    (add-module-late-initialisation #'initialise-array-from-value)))


(defun synthesise-array-init-from-file (n fn)
  "Synthesise the code to load array data for N from a file FN.

This is implemented using a late initialisation function."
  (flet ((initialise-array-from-file ()
	   (as-literal "$readmemh(\"")
	   (as-literal fn)
	   (as-literal "\", ")
	   (synthesise n)
	   (as-literal ");" :newline t)))

    (add-module-late-initialisation #'initialise-array-from-file)))


(defun synthesise-array-init (n v)
  "Parse the initial contents of N as described by V.

If the initial value is a list of the form (:FILE FN) the data is read from file FN.
Otheriwse it is read as a literal list."
  (destructuring-bind (shape &key
			       initial-element initial-contents
			       element-width element-type)
      (cdr v)
    ;; skip an initial quote, allowed for Lisp compatability
    (unquote shape)
    (unquote initial-contents)

    ;; 1d arrays only for now
    (as-literal "[ 0 : ")
    (synthesise (car shape))
    (as-literal " - 1 ]")

    ;; intialisation data, if any
    (cond (initial-contents
	   (if (listp initial-contents)
	       (cond ((eql (car initial-contents) :file)
		      ;; initialising from file
		      (let ((fn (cadr initial-contents)))
			(synthesise-array-init-from-file n fn)))

		     (t
		      ;; inline initial data
		      (synthesise-array-init-from-data n initial-contents shape)))))

	  (initial-element
	   ;; initial single element
	   (synthesise-array-init-from-value n initial-element shape)))))


;;; ---------- Array access ----------

(defun valid-array-index-p (ty indices)
  "Ensure INDICES are a potentially valid index into TY.

TY must be an array type, and INDICES must have the correct
dimensions, and must be unsigned integers.

We don't check the validity of the values -- although we could, and
probably should, for those that are statically determined."
  (and (subtype-p ty 'array)
       (or (not (listp ty))
	   (= (length (cddr ty))
	      (length indices)))
       (every (lambda (i)
		(subtype-p (compute-type i)
			   'unsigned-byte))
	      indices)))


(defun ensure-valid-array-index (ty indices)
  "Ensure INDICES are valid for accessing TY."
  (unless (valid-array-index-p ty indices)
    (warn 'type-mismatch :expected ty :got indices
			 :hint "Indices must match array dimension")))


(defun element-type-of-array (ty)
  "Extract the element type of array TY."
  (unless (or (null ty)
	      (eql ty 'array)
	      (and (listp ty)
		   (eql (car ty) 'array)))
    (error 'type-mismatch :expected 'array :got ty :hint "Needs an array to get its elemnent type"))

  (if (or (atomp ty)
	  (= (length ty) 1)
	  (eql (cadr ty) *))
      ;; array has no specialiser
      t

      ;; array has a specialiser
      (cadr ty)))


(defpassmethod compute-type (aref place &rest indices)
  (declare (optimize debug))

  (let ((ty (compute-type place)))
    ;; constrain the variable
    (add-type-constraint place `array)
    (break)
    (element-type-of-array ty)))


(defpassmethod apply-type-constraints (aref place &rest indices)
  (declare (optimize debug))

  (let ((ty (compute-type place)))
    (break)
    (ensure-subtype ty 'array)
    (mapc (compose #'ensure-fixed-width #'compute-type) indices)))


(defpassmethod read-variables (aref place &rest indices)
  (let ((place-rws (if (symbolp place)
		       ;; place targets a variable directly
		       (list place)

		       ;; place is complex, recurse into it
		       (read-variables place)))
	(indices-rws (union-all (mapcar #'read-variables indices))))

    (union place-rws indices-rws)))


(defpassmethod compute-dependencies (aref &rest args)
  (dolist (n (read-variables `(aref ,@args)))
     (set-variable-property n 'read t)))


(defpassmethod read-variables-setf (aref place &rest indices)
  (declare (optimize debug))

  (let ((val-indices (union-all (mapcar #'read-variables (safe-list indices)))))
    (if (symbolp place)
	;; place is just a symbol
	val-indices

	;; place is more than just a symbol, destructure again
	(union val-indices
	       (read-variables place)))))


(defpassmethod written-variables-setf (aref place &rest  indices)
  (declare (optimize debug))

  (if (symbolp place)
      ;; variable, this is written to
      (list place)

      ;; complex place, recurse into it
      (written-variables-setf place)))


(defpassmethod generalised-place-p (aref place &rest indices)
  (generalised-place-p place))


(defpassmethod simple-expression-form-p  (aref &rest args)
  (every #'simple-expression-form-p args))


(defpassmethod synthesise (aref place &rest indices)
   (synthesise place)
    (as-literal "[ ")
    (as-list indices)
    (as-literal " ]"))
