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

(defsubtype ((array lty &optional indices) array)
  t)


(defsubtype ((array lty &optional li) (array rty &optional ri))
  (subtype-p lty rty))


(deflub ((array lty &optional li) (array rty &optional ri))
  `(array ,(lub lty rty)))


;;; ---------- Dope vector ----------

;;; A dope vector is the representation of the metadata of an array. Unlike
;;; in most situations, in Verilisp they only exist at compile time.

(defclass ArrayDopeVector ()
  ((dimensions
    :initarg :dimensions
    :reader dimensions
    :documentation "The dimensions of the array.")
   (element-type
    :initarg :element-type
    :reader element-type
    :documentation "The element type of the array."))
  (:documentation "A dope vector of metadata for an array."))


;;; ---------- Array initialisation data ----------

(defun valid-array-shape-p (shape)
  "Test that SHAPE is a valid array shape.

The dimensions must be statically determined."
  (and (listp shape)
       (every #'static-p shape)))


(defun ensure-valid-array-shape (shape)
  "Ensure SHAPE is a valid array shape."
  (unless (valid-array-shape-p shape)
    (error 'not-synthesisable :hint "Arrays dimensions must be known statically")))


(defun data-has-shape-p (data shape)
  "Test whether DATA has the given SHAPE.

DATA should consist of nested lists, one nesting per dimension, with each
having the correct length as given in SHAPE."
  (let ((d (car shape)))
    (and (= (length data) d)
	 (if-let ((ds (cdr shape)))
	     (and (every #'listp data)
		  (every (rcurry #'data-has-shape-p ds) data))

	     t))))


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


(defpassmethod read-variables (make-array shape &key (initial-element 0)
					  initial-contents
					  element-type
					  displaced-to
					  displaced-index-offset
					  displaced-offset)
  (unquote shape)

  (union (union-all (mapcar #'read-variables shape))
	 (read-variables initial-element)))


(defpassmethod compute-type (make-array shape &key initial-element
					initial-contents
					element-type
					displaced-to
					displaced-index-offset
					displaced-offset)
  (declare (optimize debug))

  ;; skip an initial quotes, allowed for Lisp compatability
  (unquote shape)
  (unquote element-type)
  (unquote initial-contents)

  ;; check shape
  (ensure-valid-array-shape shape)

  ;; initial contents must either match the size of the array
  ;; or identify a file
  (if initial-contents
      (if (listp initial-contents)
	  (cond ((eql (car initial-contents) :file)
		 ;; nothing to do at the moment
		 (unless element-type
		   (error 'syntax-error :hont "Provide an explicit type for data in file")))

		(t
		 ;; check all elements of literal data
		 (let ((icty (apply #'lub (mapcar #'compute-type initial-contents))))
		   (if element-type
		       ;; if we have an explicit type, make sure the contents match
		       (ensure-subtype icty element-type)

		       ;; use the initial contents' type as the element type
		       (setq element-type icty)))

		 ;; check shape
		 (ensure-data-has-shape initial-contents shape)))

	  (error 'syntax-error :hint "Make sure initial conmtents are an array")))

  ;; initialise type from the initial element unless an explicit element type is provided
  (unless element-type
    (when initial-element
      (setq element-type (compute-type initial-element))))

  ;; TODO: need to constrain the element types too
  (when displaced-to
    ;; if displaced, that needs to be an array too
    (add-type-constraint displaced-to `(array ,element-type))

    ;; TODO: Handle conformant displaced arrays too

    ;; mustn't have initial values or element types
    ;; TODO: This isn't actually true....
    (when (or initial-element initial-contents)
      (error 'syntax-error :hint "Displaced arrays can't be initialised"))
    (when element-type
      (error 'syntax-error :hint "Displaced arrays inherit their element type")))

  `(array ,element-type ,shape))


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
;;; TODO: Expand constants

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
			       element-width element-type
			       displaced-to
			       displaced-index-offset
			       conformal
			       displaced-offset)
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
    (error 'type-mismatch :expected 'array :got ty :hint "Needs an array to get its element type"))

  (if (or (atom ty)
	  (= (length ty) 1)
	  (eql (cadr ty) *))
      ;; array has no specialiser
      t

      ;; array has a specialiser
      (cadr ty)))


(defpassmethod generalised-place-p (aref place &rest indices)
  t)


;;; TODO: Assume place is simple (no indirect references)

;;; TODO: Should this be a transform pass? (I don't think it can be...)

(defun displaced-aref (place indices)
  "Dereference through a displaced array PLACE, correcting INDICES.

If PLACE is displaced, return the actual underlying array and corrected
indiced; otherwise return the original place and indices unchanged. This
may involve recursively checking for displacements.

Return a list containing the final array and indices."
  (declare (optimize debug))
  ;; can only currently assign directly to variables (no indirect references)
  (ensure-symbol place)

  (let ((iv (get-initial-value place)))
    (destructuring-bind (shape &key initial-element
				 initial-contents
				 element-type
				 displaced-to
				 (displaced-index-offset 0)
				 displaced-offset)
	(cdr iv)

      (if displaced-to
	  ;; array is displaced into another, correct
	  (let ((realindices (list (+ (car indices) displaced-index-offset))))
	    ;; recurse in case the target is displaced as well
	    (displaced-aref displaced-to realindices))

	  ;; array is not displaced, return as-is
	  (list place indices)))))


(defpassmethod read-variables-setf (aref place &rest indices)
  (union-all (mapcar #'read-variables indices)))


(defpassmethod written-variables-setf (aref place &rest indices)
  (destructuring-bind (dplace dindices)
      (displaced-aref place indices)

    (list dplace)))


(defpassmethod compute-type (aref place &rest indices)
  (declare (optimize debug))

  (destructuring-bind (dplace dindices)
      (displaced-aref place indices)

    (mapc #'ensure-fixed-width dindices)
    (add-type-constraint dplace `array) ; TODO: add bounds on indices?
    (let ((ty (get-type dplace)))
      (element-type-of-array ty))))


(defpassmethod compute-dependencies (aref place &rest indices)
  (destructuring-bind (dplace dindices)
      (displaced-aref place indices)

    (let ((rs (union-all (mapcar #'read-variables dindices))))
      (add-dependencies dplace rs))))


(defpassmethod simple-expression-p  (aref &rest args)
  (every #'simple-expression-p args))


(defpassmethod synthesise (aref place &rest indices)
  (destructuring-bind (realplace realindices)
      (displaced-aref place indices)

    (synthesise realplace)
    (as-literal "[ ")
    (as-list realindices)
    (as-literal " ]")))
