;;;; Helper macros for building type algebras in the Verilisp DSL
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

(in-package :verilisp/dsl)

;;; A type algebra lets us talk about the relationships between types.
;;; Common Lisp uses SUBTYPEP for this, indicating whether one type is
;;; a sub-type of another.
;;;
;;; For DSLs we often need something different or more flexible than
;;; the built-in operation: we might want to exclude some Lisp types
;;; from consideration, for example. For this reason the DSL builder
;;; lets us define type algebras specific to a given language and
;;; provides SUBTYPE-P and LUB operations over the algebra.
;;;
;;; The algebra always has the "lattice" types of T and NIL defined,
;;; but no others by default.


;;; ---------- Type normalisation ----------

(defun deconstruct-type (ty)
  "Deconstruct the type specifier TY into tag and arguments.

If TY is a naked symbol then the arguments part is nil; otherwise
it contains the arguments."
  (if (listp ty)
      (list (car ty) (cdr ty))
      (list ty '())))


(defun construct-type (tytag tyargs)
  "Construct a type specifier from TYTAG and TYARGS.

This is simply TYTAG is TYARGS is nil, or a list specifier consisting
of TYTAG and TYARGS."
  (if (null tyargs)
      tytag
      (cons tytag tyargs)))


;;; ---------- Subtypes and LUBs ----------

(defgeneric subtype-p (l r)
  (:documentation "Test whether L is a sub-type of R.")

  ;; lattice types
  (:method ((l (eql t)) (r (eql t)))
    ;; top is only a sub-type of itself
    t)
  (:method (l (r (eql t)))
    ;; all types are sub-types of top
    t)
  (:method ((l (eql nil)) r)
    ;; bottom is a sub-type of everything
    t)
  (:method (l (r (eql nil)))
    ;; bottom has no sub-types
    nil)

  (:method (l r)
    (declare (optimize debug))

    (destructuring-bind (lfun largs)
	(deconstruct-type l)
      (destructuring-bind (rfun rargs)
	  (deconstruct-type r)

	;; if l has arguments and r doesn't, and they have the same
	;; tag, then they're sub-types
	(if (and (eql lfun rfun)
		 (null rargs))
	    t

	    ;; otherwise evaluate the forms
	    (subtype-p/form lfun largs rfun rargs))))))


(defgeneric subtype-p/form (lfun largs rfun rargs)
  (:documentation "Test whether the type (LFUN . LARGS) is a sub-type of (RFUN . RARGS).

Use the DEFSUBTYPE macro to define methods for this function.")

  ;; by default there is no relationship between types
  (:method (lfun largs rfun rargs)
    nil))


(defun lub (&rest tys)
  "Return the least upper bound of types TYS.

The most common form will be a pair of types, but multiple are
acceptable -- as is one, in which case the type will be reduced
to a normal form (if one exists according to the type rules)."
  (flet ((lub/pair (l r)
	   ;; lattice types
	   (cond
	     ;; top on either side is always the result
	     ((or (eql l t)
		  (eql r t))
	      t)

	     ;; bottom always returns the other type
	     ((null l)
	      r)
	     ((null r)
	      l)

	     (t
	      (destructuring-bind (lfun largs)
		  (deconstruct-type l)
		(destructuring-bind (rfun rargs)
		    (deconstruct-type r)

		  (lub/form lfun largs rfun rargs)))))))

    (case (length tys)
      (0
       ;; lub of no types is bottom
       nil)

      (1
       ;; for a single type we LUB it with bottom /without/
       ;; triggering the shortcut above, so we reduce
       ;; any complex types
       (destructuring-bind (lfun largs)
	   (deconstruct-type (car tys))
	 (lub/form lfun largs nil nil)))

      (2
       ;; short-cut for a pair of types to save constructing the fold
       (lub/pair (car tys) (cadr tys)))


      (t
       ;; by default we fold over the list of types
       (foldr #'lub/pair (cdr tys) (car tys))))))


(defgeneric lub/form (lfun largs rfun rargs)
  (:documentation "Form the least upper-bound of (LFUN . LARGS) and (RFUN . RARGS).")

  ;; by default the LUB of two types is the larger, or T if they're
  ;; incomparable under sub-typing
  ;;
  ;; It's slightly wasteful to re-construct a just-deconstructed type,
  ;; but we want the default to be here and not in LUB to allow the
  ;; default to be overridden if needed.
  (:method (lfun largs rfun rargs)
    (declare (optimize debug))

    (let ((l (construct-type lfun largs))
	  (r (construct-type rfun rargs)))

      (cond ((subtype-p l r)
	     r)
	    ((subtype-p r l)
	     l)

	    ;; default for incomparable types is t
	    (t
	     t)))))


;;; ---------- Defining sub-type relationships ----------

;;; TODO: Add error checks on syntax of patterns

;;; TODO: Add IGNORABLE declarations for variables in type patterns


(defun parse-type-parameter-patterns (lform rform body)
  "Parse the type patterns LFORM and RFORM.

The patterns are used to form three code fragments:

- The left and right patterns for matching the types
- A body that declares the pattern's variables around BODY

This means that BODY has access to the variables delcared in the
LFORM and RFORM patterns."
  (declare (optimize debug))

  (labels ((variables-in-pattern (pat)
	     (if (null pat)
		 0

		 (let ((l (car pat)))
		   (cond ((listp l)
			  ;; optional or key parameters each declare one variable
			  (1+ (variables-in-pattern (cdr pat))))

			 ((eql l '&rest)
			  ;; &rest declares one variable
			  1)

			 ((member l '(&key &optional))
			  ;; ignore other pattern keywords
			  (variables-in-pattern (cdr pat)))

			 ((symbolp l)
			  ;; a variable itself
			  (1+ (variables-in-pattern (cdr pat))))

			 (t
			  (error 'dsl-error :hint (format nil "~s can't appear in type patterns" l))))))))

    (with-gensyms (ltag lrest rtag rrest)

      (let (lmatcher lpattern rmatcher rpattern)

	;;RHS
	(match rform
	  ;; full type capture
	  ((list '&type ty)
	   (setq rmatcher `(let ((,ty (construct-type ,rtag ,rrest)))
			     ,@body))
	   (setq rpattern `(,rtag ,rrest)))

	  ;; type tag and arguments
	  ((cons rfun rargs)
	   (setq rmatcher `(destructuring-bind ,rargs
			       (or ,rrest
				   ',(n-copies '() (variables-in-pattern rargs)))
			     ,@body))
	   (setq rpattern `((,rtag (eql ',rfun)) ,rrest)))

	  ;; just a tag
	  (rfun
	   (setq rmatcher `(progn
			     ,@body))
	   (setq rpattern `((,rtag (eql ',rfun)) (,rrest (eql nil))))))

	;; LHS
	(match lform
	  ;; full type capture
	  ((list '&type ty)
	   (setq lmatcher `(let ((,ty (construct-type ,ltag ,lrest)))
			     ,rmatcher))
	   (setq lpattern `(,ltag ,lrest)))

	  ;; type tag and arguments
	  ((cons lfun largs)
	   (setq lmatcher `(destructuring-bind ,largs
			       (or ,lrest
				   ',(n-copies '() (variables-in-pattern largs)))
			     ,rmatcher))
	   (setq lpattern `((,ltag (eql ',lfun)) ,lrest)))

	  ;; just a tag
	  (lfun
	   (setq lmatcher rmatcher)
	   (setq lpattern `((,ltag (eql ',lfun)) (,lrest (eql nil))))))

	;; return the generated code
	(list lpattern rpattern lmatcher)))))


(defmacro defsubtype ((lform rform) &body body)
  "Define a sub-type relationship."
  (declare (optimize debug))

  ;; set optional docstring
  (let ((docstring (format nil "Test if ~s <: ~s" lform rform)))
    (when (stringp (car body))
      (setq docstring (car body))
      (setq body (cdr body)))

    ;; generate the method
    (destructuring-bind (lpattern rpattern matcher)
	(parse-type-parameter-patterns lform rform body)

      `(defmethod subtype-p/form (,@lpattern ,@rpattern)
	 ,docstring

	 , matcher))))


(defmacro deflub ((lform rform) &body body)
  "Define a least upper bound calculation for LFORM and RFORM."
  (declare (optimize debug))

  ;; set optional docstring
  (let ((docstring (format nil "Compute the least upper-bound of ~s ~s" lform rform)))
    (when (stringp (car body))
      (setq docstring (car body))
      (setq body (cdr body)))

    ;; generate the method
    (destructuring-bind (lpattern rpattern matcher)
	(parse-type-parameter-patterns lform rform body)

      `(defmethod lub/form (,@lpattern ,@rpattern)
	 ,docstring

	 ,matcher))))
