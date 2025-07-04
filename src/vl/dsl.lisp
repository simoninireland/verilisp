;; Simple macro set for building Lisp-embedded DSLs
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


;; ---------- DSL data structure ----------

(define-condition dsl-error ()
  ((message
    :documentation "Explanation for the error."
    :initarg :message
    :reader message))
  (:report (lambda (c str)
	     (format str "DSL error: ~a" (message c))))
  (:documentation "Condition signalled for problems in the DSL."))


(defclass form ()
  ((tag
    :documentation "The head of the form."
    :initarg :tag
    :reader tag)
   (compound-form
    :documentation "Whether the form is an atom of list."
    :initarg :compound-p
    :initform t
    :reader compound-p)
   (lambda-list
    :documentation "The lambda-list describing the arguments to the form."
    :initarg :lambda-list
    :initarg :args
    :reader lambda-list)
   (docstring
    :documentation "Docstring for the form."
    :initarg :docstring
    :initarg :documentation
    :reader docstring))
  (:documentation "The description of a form within a DSL."))


(defclass dsl ()
  ((forms
    :documentation "Alist of form tags and their descriptions."
    :initform nil
    :accessor forms)
   (docstring
    :documentation "Dostring for the DSL."
    :initarg :docstring
    :initarg :documentaton
    :initform "A DSL."
    :reader docstring))
  (:documentation "A domain-specific language embedded into Lisp."))


(defun form-p (formtag dsl)
  "Test whether FORMTAG is valid within DSL."
  (not (null (assoc formtag (forms dsl)))))


(defun ensure-form (formtag dsl)
  "Test whether FORMTAG is valid within DSL.

An UNRECOGNISED-DSL-FORM error is signalled if the form
is invalid."
  (unless (form-p formtag dsl)
    (error 'dsl-error :message (format nil "Unknown form ~a" formtag))))


(defun add-form (f dsl)
  "Add F to DSL."
  (let ((formtag (tag f)))
    (when (form-p formtag dsl)
      (error 'dsl-error :message (format nil "Duplicate form ~a" formtag)))

    (appendf (forms dsl) (list (list formtag f)))))


(defun get-form (formtag dsl)
  "Retrieve form with tag FORMTAG from DSL."
  (if-let ((a (assoc formtag (forms dsl))))
    (cadr a)

    (error 'dsl-error :message (format nil "Unknown form ~a" formtag))))


;; ---------- Helpers Current DSL ----------

(defvar *current-dsl* nil
  "The DSL currently being defined.

New forms are added to this DSL by default. This can be
overridden by providing a specific :DSL clause.")


(defun in-dsl (dsl)
  "Make DSL current for future definitions."
  (setq *current-dsl* dsl))


(defun current-dsl ()
  "Return the current DSL into which definitions are being placed."
  *current-dsl*)


;; ---------- DSL forms ----------

(defun clause-p (l)
  "Test whether L is a clause.

Clauses are lists that begin with a keyword."
  (and (listp l)
       (not (null l))
       (keywordp (car l))))


(defmacro deform/dsl (form &rest clauses)
  "Define FORM as a valid form in the DSL.

FORM should either be a list consisting of the form tag and a
lambda-list of arguments, or a symbol identifying the type of an atom.

CLAUSES are a list of lists headed by a symbol defining extra features fot the form. Valid clauses are :DSL for defining the DSL into which to add the form (defaulting to CURRENT-DSL, as set by IN-DSL), and
:DOCUMENTATION providing a docstring. The arguments to clauses are
evaluated."
  (let ((dsl (current-dsl))
	(docstring "A form")
	formtag compound-p args)

    ;; see whether we're defining an atom or a "normal" form
    (if (atom form)
	(progn
	  ;; an atomic type
	  (setq formtag form)
	  (setq compound-p nil))

	(progn
	  ;; a list form possibly with arguments
	  (setq formtag (car form))
	  (setq compound-p t)
	  (setq args (cdr form))))

    ;; parse the clauses
    (dolist (clause clauses)
      (unless (clause-p clause)
	(error 'dsl-error :message (format nil "Non-clause ~a encountered" clause)))

      (case (car clause)
	(:documentation
	 ;; docstring for the form
	 (setq docstring (eval (cadr clause))))

	(:dsl
	 ;; set the DSL receiving the form
	 (setq dsl (eval (cadr clause))))

	(t
	 (error 'dsl-error :message (format nil "Unknown clause ~a" clause)))))

    ;; check we have a DSL to add form to
    (when (null dsl)
      (error 'dsl-error :message "No DSL specified for form"))

    ;; check that the lambda list is valid
    (handler-case
	(parse-ordinary-lambda-list args :allow-specializers nil)

      (program-error ()
	(make-condition 'dsl-error :message (format nil "Malformed lambda-list ~a" args))))

    ;; add form
    (let ((f (make-instance 'form :tag formtag
				  :compound-p compound-p
				  :lambda-list args
				  :documentation docstring)))
      (add-form f dsl)

      ;; return the form
      `',form)))


;; ---------- Generic functions over DSL forms ----------

(defun form-generic-name (name)
  "Return the name used for the generic function NAME."
  name)


(defun form-method-name (name)
  "Return the name used for the methods on generic function NAME."
  (intern (concat (symbol-name name) "/form")))


(defmacro defgeneric/dsl (name &rest clauses)
  "Declare NAME as a generic function over "
  (with-gensyms (f fun a)
    `(progn
       ;; the per-form generic function
       (defgeneric ,(form-method-name name) (,fun ,a))

       ;; the overall generic function
       (defgeneric ,(form-generic-name name) (,f)
	 (:method ((,f list))
	   (destructuring-bind (,fun &rest ,a)
	       ,f
	     (,(form-method-name name) ,fun ,a)))))))


(defmacro defmethod/dsl (name form &body body)
  "Define a method for NAME over FORM.

FORM may be a list representing a form tag, or a list
representing a specialisation, which consists of a list
with a variable name and a type correspondong to the atomic
form.

CLAUSES are a list of lists headed by a symbol defining extra features
fot the form. Valid clauses are :DSL for defining the DSL into which
to add the form (defaulting to CURRENT-DSL, as set by IN-DSL), and
:DOCUMENTATION providing a docstring. The arguments to clauses are
evaluated.

ARGS will be bound in BODY."
  (declare (optimize debug))

  (let ((dsl (current-dsl))
	docstring
	formtag args must-be-compound-p)

    ;; parse any clauses on the head of the body
    (labels ((parse-clause (clauses)
	       (let ((clause (car clauses)))
		 (if (clause-p clause)
		     (progn
		       (case (car clause)
			 (:dsl
			  (setq dsl (eval (cadr clause))))

			 (:documentation
			  (setq docstring (eval (cadr clause))))

			 (t
			  (error 'dsl-error :message (format nil "Unknown clause ~a" clause))))

		       (parse-clause (cdr clauses)))

		     ;; clauses ended, now in the body
		     clauses))))

      ;; remaining "clauses" are the body of the method
      (setq body (parse-clause body)))

    ;; ensure we have a DSL to work on
    (when (null dsl)
      (error 'dsl-error :message "No DSL specified for method"))

    ;; check FORM is a list
    (unless (listp form)
      (error 'dsl-error :message "Form specifier must be a list, not ~a" form))

    ;; extract the form tag, which varies depending on whether
    ;; the form is atomic or compound
    (if (listp (car form))
	(progn
	  ;; atomic form
	  (setq formtag (cadar form))
	  (setq args nil)
	  (setq must-be-compound-p nil))

	(progn
	  ;; compound form
	  (setq formtag (car form))
	  (setq args (cdr form))
	  (setq must-be-compound-p t)))

    ;; set default docstring
    (unless docstring
      (setq docstring (format nil "Method for ~a" formtag)))

    ;; build the method on the appropriate generic function
    (let ((f (get-form formtag (current-dsl))))
      (if (compound-p f)
	  (progn
	    ;; compound form, generate method on form function
	    (unless must-be-compound-p
	      (error 'dsl-error :message "Wrong form specifier for ~a" formtag))

	    (with-gensyms (fun a)
	      `(defmethod ,(form-method-name name) ((,fun (eql ',formtag)) ,a)
		 ,docstring
		 (destructuring-bind (,@args)
		     ,a
		   ,@body))))

	  (progn
	    ;; atomic form, generate method on main function
	    (when must-be-compound-p
	      (error 'dsl-error :message "Wrong form specifier for ~a" formtag))

	    (with-gensyms (f)
	      `(defmethod ,(form-generic-name name) ,form
		 ,docstring
		 ,@body)))))))


;; ---------- Example ----------

(defparameter arithmetic (make-instance 'dsl))

(in-dsl arithmetic)

(deform/dsl integer
  (:documentation "Integer literals."))

(deform/dsl (+ &rest args))

(deform/dsl (- &rest args)
  (:documentation "Unary negation and n-ary subtraction."))

(deform/dsl (* &rest args))

(defgeneric/dsl typecheck
  (:documentation "Typecheck a form in the arithmetic DSL."))


(defgeneric/dsl evaluate
    (:documentation "Evaluate a form in the arithmetic DSL."))


(defmethod/dsl typecheck ((n integer))
  'integer)


(defmethod/dsl evaluate ((n integer))
  n)


(defmethod/dsl typecheck (+ &rest args)
  (every (lambda (ty)
	   (subtypep ty 'integer))
	 (mapcar #'typecheck args))

  'integer)


(defmethod/dsl evaluate (+ &rest args)
  (foldr #'+ (mapcar #'evaluate args)))


(in-dsl nil)

(defmethod/dsl typecheck (- &rest args)
  (:dsl arithmetic)

  (every (lambda (ty)
	   (subtypep ty 'integer))
	 (mapcar #'typecheck args))

  'integer)
