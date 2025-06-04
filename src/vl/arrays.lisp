;; Arrays of variables
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a Common Lisp DSL for hardware design
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


;; ---------- Array initialisation data ----------

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


;; ---------- Array construction ----------

(defmacro unquote (place)
  "Remove any leading quote from the data in PLACE.

This is mainly used to preserve compatability with Lisp, where
these forms need to be quoted. We /allow/ them to be quoted in
Verilisp, but don't /require/ it."
  `(if (and (not (null ,place))
	    (listp ,place)
	    (eql (car ,place) 'quote))
       (setq ,place (cadr ,place))))


(defmethod expand-type-parameters-type ((ty (eql 'array)) args)
  (if (null args)
      ty
      (destructuring-bind (element-type shape)
	  args

	;; expand the embedded type parts
	(setq element-type (expand-type-parameters element-type))
	(setq shape (mapcar #'eval-in-static-environment shape))

	`(array ,element-type ,shape))))


(defun array-element-width (form)
  "Return the width of the elements of array constructor FORM."
  (if-let ((m (member :element-type (cdr form))))
    ;; use the element width of there is one
    (bitwidth (safe-cadr m) '())

    ;; otherwise the default
    *default-register-width*))


;; shape needs to be statically determinable

(defmethod typecheck-sexp ((fun (eql 'make-array)) args)
  (destructuring-bind (shape &key
			       (initial-element 0)
			       initial-contents
			       element-type)
      args

    ;; skip an initial quotes, allowed for Lisp compatability
    (unquote shape)
    (unquote element-type)
    (unquote initial-contents)

    ;; check shape
    (ensure-valid-array-shape shape)

    ;; check or derive element type
    (if element-type
	(ensure-subtype (typecheck initial-element) element-type)

	;; default is a fixed-width unsigned
	(setq element-type `(unsigned-byte ,*default-register-width*)))

    ;; initial contents must either match the size of the array
    ;; or identify a file
    (if initial-contents
	(if (listp initial-contents)
	    (cond ((eql (car initial-contents) :file)
		   ;; nothing to do at the moment
		   t)

		  (t
		   ;; check all elements of literal data
		   (ensure-data-has-shape initial-contents shape)
		   (dolist (c initial-contents)
		     (ensure-subtype (typecheck c) element-type))))))

    `(array ,element-type ,shape)))


(defmethod free-variables-sexp ((fun (eql 'array)) args)
  ;; can't have any free variables (I don't think)
  nil)


(defmethod dependencies-sexp ((fun (eql 'array)) args)
  ;; can't have any dependencies (I don't think)
  nil)


(defun rebuild-options (ns vs)
  "Build an options list from NS where the corresponding value in VS is non-null."
  (flet ((key-value (opts nv)
	   (destructuring-bind (n v)
	       nv
	     (if (null v)
		 opts
		 (append opts (list n v))))))

    (foldr #'key-value (zip ns vs) '())))


;; Only works for one-dimensional arrays at the moment
;; Should expand constants

(defun synthesise-array-init-from-data (data shape)
  "Return the initialisation of DATA with the given SHAPE."
  (as-list data :before "{ " :after " }"
		:per-row 16))


(defun synthesise-array-init-from-file (n fn)
  "Synthesise the code to load array data for N from a file FN.

Thi is implemented using a late initialisation function."
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
    (when initial-contents
      (if (listp initial-contents)
	  (cond ((eql (car initial-contents) :file)
		 ;; initialising from file
		 (let ((fn (cadr initial-contents)))
		   (synthesise-array-init-from-file n fn)))

		(t
		 ;; inline initial data
		 (as-literal " = " :newline t)
		 (with-indentation
		   (synthesise-array-init-from-data initial-contents shape))))))))


;; ---------- Array access ----------

(defun valid-array-index-p (ty indices)
  "Ensure INDICES are a potentially valid index into TY.

TY must be an array type, and INDICES must have the correct
dimensions, and must be unsigned integers.

We don't check the validity of the values -- although we could, and
probably should, for those that are statically determined."
  (and (subtypep ty 'array)
       (or (not (listp ty))
	   (= (length (cddr ty))
	      (length indices)))
       (every (lambda (i)
		(subtypep (typecheck i)
			  'unsigned-byte))
	      indices)))


(defun ensure-valid-array-index (ty indices)
  "Ensure INDICES are valid for accessing TY."
  (unless (valid-array-index-p ty indices)
    (warn 'type-mismatch :expected ty :got indices
			 :hint "Indices must match array dimension")))


(defun element-type-of-array (ty)
  "Extract the element type of array TY."
  (cadr ty))


(defmethod typecheck-sexp ((fun (eql 'aref)) args)
  (declare (optimize debug))

  (destructuring-bind (var &rest indices)
      args
    (let ((ty (typecheck var)))
      (ensure-subtype ty 'array)
      (ensure-valid-array-index ty indices)

      ;; the type is the type of the elements
      (element-type-of-array ty))))


(defmethod free-variables-sexp ((fun (eql 'make-array)) args)
  ;; can't have any free variables (I don't think)
  nil)


(defmethod dependencies-sexp ((fun (eql 'make-array)) args)
  ;; can't have any dependencies (I don't think)
  nil)


(defmethod generalised-place-sexp-p ((fun (eql 'aref)) args)
  t)


(defmethod synthesise-sexp ((fun (eql 'aref)) args)
  (destructuring-bind (var &rest indices)
      args
    (synthesise var)
    (as-literal "[ ")
    (as-list indices)
    (as-literal " ]")))


(defmethod lispify-sexp ((fun (eql 'aref)) args)
  (destructuring-bind (var &rest indices)
      args
    `(aref ,var ,@indices)))
