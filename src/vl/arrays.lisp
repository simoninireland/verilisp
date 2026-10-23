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


;;; ---------- Array type algebra ----------

;; array types

(defsubtype ((array lty &optional indices) array)
  t)


(defsubtype ((array lty &optional li) (array rty &optional ri))
  (subtype-p lty rty))


(deflub ((array lty &optional li) (array rty &optional ri))
  `(array ,(lub lty rty)))


;; member types

;; TODO: These could be improved with better pattern-matching in DEFSUBTYPE and LUB

(defsubtype ((member ty1 &optional mem) (&type ty2))
  (if (subtype-p ty1 'array)
      (subtype-p (array-type-element-type (lub ty1)) ty2)

      (call-next-method)))


(defsubtype ((&type ty1) (member ty2 &optional mem))
  (if (subtype-p ty2 'array)
      (subtype-p ty1 (array-type-element-type (lub ty2))))

  (call-next-method))


(deflub ((member ty1 &optional mem) (&type ty2))
  (declare (optimize debug))

  (if (subtype-p ty1 'array)
      (lub (array-type-element-type (lub ty1)) ty2)

      (call-next-method)))


(deflub ((&type ty1) (member ty2 &optional mem))
  (lub `(member ,ty2 ,mem) ty1))


(defmethod bitwidth/form ((tytag (eql 'member)) tyargs)
  (declare (optimize debug))

  (if (subtype-p (car tyargs) 'array)
      (bitwidth (array-type-element-type (lub (car tyargs))))

      (call-next-method)))


;;; ---------- Array initialisation data ----------

(defun valid-array-shape-p (shape)
  "Test that SHAPE is a valid array shape.

The dimensions must be statically determined."
  (and (listp shape)
       (every #'static-p shape)))


(defun ensure-valid-array-shape (shape)
  "Ensure SHAPE is a valid array shape."
  (unless (valid-array-shape-p shape)
    (error 'not-static :hint "Arrays dimensions must be known statically")))


(defun data-has-shape-p (data shape)
  "Test whether DATA has the given SHAPE.

DATA should consist of nested lists, one nesting per dimension, with each
having the correct length as given in SHAPE."
  (let ((d (car shape)))
    (and (= (length data) d)
	 (if-let ((ds (cdr shape)))
	     (and (every #'listp data)
		  (every (rcurry #'data-has-shape-p ds) data))

	     ;; no shape given, accept it
	     t))))


(defun ensure-data-has-shape (data shape)
  "Ensure DATA has the given SHAPE."
  (unless (data-has-shape-p data shape)
    (error 'shape-mismatch :expected shape
			   :hint "Ensure initial contents have the right shape")))


;;; ---------- Array construction ----------

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

  ;; check shape is valid (and must be static)
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

	  (error 'syntax-error :hint "Make sure initial contents are an array")))

  ;; initialise type from the initial element unless an explicit element type is provided
  (unless element-type
    (when initial-element
      (setq element-type (compute-type initial-element))))

  ;; TODO: need to constrain the element types too
  (when displaced-to
    ;; mustn't have initial values or element types
    ;; TODO: This isn't actually true....
    (when (or initial-element initial-contents)
      (error 'syntax-error :hint "Displaced arrays can't be initialised"))
    (when element-type
      (error 'syntax-error :hint "Displaced arrays inherit their element type"))

    ;; find the type of the underlying array
    (setq element-type `(member (type-of ,displaced-to ,(current-environment))))

    ;; TODO: Handle conformant displaced arrays too

    ;; if the displacement is an index, the size of the target
    ;; array must be at least as large as the total size of
    ;; this array plus the offset
    (when displaced-index-offset
      (let ((tty (get-type displaced-to)))
	(unless (> (array-type-total-size tty)
		   (+ (array-shape-total-size shape)
		      displaced-index-offset))
	  (error 'syntax-error :hint "Ensure underlying array is large enough to hold the displaced array")))))

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
    ;; skip an initial quote, allowed for Lisp compatibility
    (unquote shape)
    (unquote initial-contents)

    ;; TODO: displaced arrays

    ;; synthesise overall size
    (as-literal "[ 0 : ")
    (if (= (length shape) 1)
	;; one-dimensional, synthesise as-is
	(synthesise (car shape))

	;; multi-dimensional, synthesise the expansion
	(as-list shape :before "(" :after ")" :sep " * "))
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


;;; ---------- Array type access ----------

(defun array-type-p (ty)
  "Test whether TY is an array type."
  (subtype-p ty 'array))


(defun vector-type-p (ty)
  "Test whether TY is a one-dimensional array."
  (and (array-type-p ty)
       (= (length (array-type-shape ty)) 1)))


(defun ensure-array-type (ty)
  "Ensure TY is an array type"
  (unless (array-type-p ty)
    (error 'type-mismatch :expected 'array :got ty)))


(defun array-shape-total-size (ds)
  "Compute the total number of elements in dimensions DS."
  (foldr #'* (mapcar #'eval-in-static-environment ds) 1))


(defun array-type-total-size (ty)
  "Return the total number of elements in array type TY.

The total size is the product of all the dimensions."
  (array-shape-total-size (array-type-shape ty)))


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


(defun index-from-indices-and-shape (indices ds)
  "Return the one-dimensional index of INDICES into an array of the given SHAPE.

Lisp arrays are always represented row-major."
  ;; this is taken pretty much directly from the Lisp hyperspec's reference definition
  ;; of the ARRAY-ROW-MAJOR-INDEX function
  ;; https://novaspec.org/cl/f_array-row-major-index
  (apply #'+ (maplist (lambda (x y)
			(* (car x) (apply #'* (cdr y))))
		      (mapcar #'eval-in-static-environment indices)
		      ds)))


(defun ensure-valid-array-index (ty indices)
  "Ensure INDICES are valid for accessing TY."
  (unless (valid-array-index-p ty indices)
    (warn 'type-mismatch :expected ty :got indices
			 :hint "Indices must match array dimension")))


;; The name of this function is a bit strange, to avoid a clash
;; with the ARRAY-ELEMENT-TYPE function in Common Lisp
(defun array-type-element-type (ty)
  "Extract the element type of array TY."
  (ensure-array-type ty)

  (if (or (atom ty)
	  (= (length ty) 1)
	  (eql (cadr ty) *))
      ;; array has no specialiser
      t

      ;; array has a specialiser
      (cadr ty)))


(defun array-type-shape (ty)
  "Extract the shape of array TY."
  (ensure-array-type ty)

  (if (and (list ty)
	   (= (length ty) 3))
      ;; shape is the last element
      (caddr ty)

      ;; otherwise fail
      (error 'representation-mismatch :variable (car ty) :expected "a bounded array" :got ty :hint "Need a specified shape")))


;;; ---------- Array element access (aref) ----------

(defpassmethod generalised-place-p (aref place &rest indices)
  t)


;;; TODO: Assume place is simple (no indirect references)

;;; TODO: Should this be a transform pass? (I don't think it can be...)

;;; TODO: This assumes compile-time resolution, but there may be situations
;;; where this doesn't work, i.e., module arguments that are arrays, and
;;; are passed a displaced array. The Lisp semantics would then have any
;;; assignments passed-through to the argument value. Handling this would
;;; require that we provide dope vectors for array values, which makes them
;;; more complicated that Verilog's arrays -- but perhaps that's fine.

(defun displaced-aref (place indices)
  "Dereference through a displaced array PLACE, correcting INDICES.

If PLACE is displaced, return the actual underlying array and corrected
indiced; otherwise return the original place and indices unchanged. This
may involve recursively checking for displacements.

Return a list containing the final array and indices."
  (declare (optimize debug))

  ;; can only currently assign directly to variables (no indirect references)
  (ensure-symbol place)

  (if-let ((displaced-to (get-environment-property place 'array-displaced-to (current-environment))))
    ;; array is displaced into another, correct the indices
    ;; TODO: need to handle conformat displacement too
    (let* ((offset (get-environment-property place 'array-displaced-offset (current-environment)))
	   (realindices (list (+ (car indices) offset))))

      ;; recurse in case the target is displaced as well
      (displaced-aref displaced-to realindices))

    ;; array is not displaced, return as-is
    (list place indices)))


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

    `(member (type-of ,dplace ,(get-frame-declaring dplace (current-environment))) ,dindices)))


(defpassmethod compute-type-setf (aref place &rest indices)
  (declare (ignore indices))

  (let* ((ty (get-type place))
	 (shape (array-type-shape ty)))

    `(array ,tyval ,shape)))


;; TODO: Should we check indices are within bounds if possible? It isn't
;; generally, but there are useful special cases that it might be
;; worth catching

(defpassmethod check-all-variables-typed (aref place &rest indices)
  (let ((shape (get-environment-property place 'array-shape (current-environment))))
    (unless (= (length shape) (length indices))
      (error 'index-mismatch :expected shape :indices indices))))


(defpassmethod compute-dependencies (aref place &rest indices)
  (destructuring-bind (dplace dindices)
      (displaced-aref place indices)

    (let ((rs (union-all (mapcar #'read-variables dindices))))
      (add-dependencies dplace rs))))


(defpassmethod simple-expression-p  (aref &rest args)
  (every #'simple-expression-p args))


(defun synthesise-array-indices (indices ds)
  "Synthesise the INDICES along dimensions DS as a single index.

This uses the same algorithm as INDEX-FROM-INDICES-AND-SHAPE, but
generates code instead of computing the value."
  (flet ((synthesise-index (x y)
	   (if (= (length x) 1)
	       (synthesise (car x))

	       (synthesise `(* ,(car x) ,@(cdr y))))

	   (if (not (null (cdr y)))
	       (as-literal " + "))))

    (as-literal "(")
    (maplist #'synthesise-index indices ds)
    (as-literal ")")))


(defpassmethod synthesise (aref place &rest indices)
  (declare (optimize debug))

  (destructuring-bind (realplace realindices)
      (displaced-aref place indices)

    (let ((realshape (get-environment-property realplace 'array-shape (current-environment))))

      (synthesise realplace)
      (as-literal "[ ")
      (if (= (length realshape) 1)
	  ;; one-dimensional, synthesise as-is
	  (synthesise (car realindices))

	  ;; multi-dimensional, synthesise the expansion
	  (synthesise-array-indices realindices realshape))
      (as-literal " ]"))))
