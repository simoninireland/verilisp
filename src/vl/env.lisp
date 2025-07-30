;; Environments
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


;; ---------- Defaults ----------

(defparameter *default-register-width* 8
  "Default width for registers.

This is used absent any specfic width specification. It will
generally reflect the word size of the desired circuit, for
example 8, 16, 32, or 64 bits.")


;; ---------- Frames----------

(defclass frame ()
  ((parent-frame
    :documentation "The frame containing this one."
    :initarg :parent
    :initform nil
    :accessor parent-frame)
   (decls
    :documentation "An alist mapping names to plists of properties."
    :initarg :decls
    :initform nil
    :accessor decls))
  (:documentation "A frame in an environment.

An environment is composed of frames, which are in turn composed of declarations
consisting of a name and a list of key-value property pairs. The names in a frame
must be unique, but may be the same as names in parent frames, in which case
the name and properties in the shallower frame shadow those in the deeper frame.

There are functions that operate on the shallowest frame, and corresponding
one that operate on the complete environment."))


(defun make-frame ()
  "Return a new, empty, detached, frame."
  (make-instance 'frame))


(defun empty-environment ()
  "Return a new empty environment."
  (make-frame))


(defun add-frame (env)
  "Return a new environment consisting of ENV with a new empty frame.

ENV is unchanged by this operation."
  (make-instance 'frame :parent env))


(defun detach-frame (env)
  "Detach the shallowest frame in ENV.

Return the now-detached frame."
  (setf (parent-frame env) nil)
  env)


(defun detached-frame-p (env)
  "Test whether ENV is a detached frame."
  (null (parent-frame env)))


(defun attach-frame (f env)
  "Attach the detached frame F to ENV as its new shallowest frame.

Returns the extended version of ENV (which is actually just F)."
  (unless (detached-frame-p f)
      (error "Attaching a frame that's already attached"))

  (setf (parent-frame f) env)
  f)


(defun get-frame-properties-assoc (n env)
  "Return the alist association of N to properties in ENV.

Return nil if there is no such association."
  (if (symbolp n)
      (assoc n (decls env))
      (assoc n (decls env)
	     :key #'symbol-name
	     :test #'string-equal)))


(defun get-frame-properties (n env)
  "Return the properties of N in the top frame of ENV.
N can be a symbol (usually) or a string. In the latter case the
variable is checked by string equality aginst the symbol name.

An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (if-let ((kv (get-frame-properties-assoc n env)))
    (cadr kv)

    (error 'unknown-variable :variable n
			     :hint "Make sure the variable is in scope in the current frame")))


(defun get-frame-names (env)
  "Return the names of the variables in the topmost frame of ENV."
  (mapcar #'car (decls env)))


(defun empty-frame-p (env)
  "Test whether ENV's shallowest frame is empty."
  (null (get-frame-names env)))


(defun variable-declared-in-frame-p (n env)
  "Test whether N is defined in the topmost frame of ENV."
  (not (null (if (symbolp n)
		 (assoc n (decls env))
		 (assoc n (decls env)
			:key #'symbol-name
			:test #'string-equal)))))


(defun get-frame-property (n prop env &key default)
  "Return the property PROP of variable N in the topmost frame of ENV.

Undeclared properties value value NIL, which can be changed using the
:DEFAULT argument. An UNKNOWN-VARIABLE error is signalled if N is
undefined."
  (let ((props (get-frame-properties n env)))
    (if-let ((m (assoc prop props)))
      ;; we found a binding, return the property
      (cadr m)

      ;; no property defined, return the default
      default)))


(defun set-frame-properties (n props env)
  "Replace the properties of N with ENV with PROPS."
  (if-let ((kv (get-frame-properties-assoc n env)))
    ;; overwrite the old properties
    (setf (cdr kv) (list props))))


(defun set-frame-property (n prop v env)
  "Set the value of PROP to V for variable N in the topmost frame of ENV.

The property is updated if it is defined, and created if not.
An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (declare (optimize debug))

  (if-let ((props (get-frame-properties n env)))
    (if-let ((m (assoc prop props)))
      ;; property exists, update it
      (setf (cdr m) (list v))

      ;; append new property to the list
      (let ((e (last props)))
	(setf (cdr e) (list (list prop v)))))

    ;; empty property list, update it with just this property
    (let ((kv (get-frame-properties-assoc n env)))
      (setf (cdr kv) (list (list (list prop v)))))))


(defun get-frame-declaring (n env)
  "Return the shallowest frame in ENV that declares N.

An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (if (variable-declared-in-frame-p n env)
      env

      (if-let ((penv (parent-frame env)))
	(get-frame-declaring n penv)

	;; we ran out of frames to search
	(error 'unknown-variable :variable n))))


(defun forget-frame-variable (n env)
  "Forget the declaration of NAME in the shallowest frame of ENV.

Returns the name oe the variable forgotten."
  (unless (variable-declared-in-frame-p n env)
    (error 'unknown-variable :variable n))

  (setf (decls env) (remove-if (lambda (m)
				 (eql (car m) n))
			       (decls env)))

  n)


;; ---------- Environments ----------

(defun get-environment-properties (n env)
  "Return the key/value list for N in ENV.

N can be a symbol (usually) or a string. In the latter case the
variable is checked by string equality aginst the symbol name.

An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (get-frame-properties n (get-frame-declaring n env)))


(defun get-environment-names (env)
  "Return the names in ENV."
  (foldr #'union (map-environment (lambda (n env) (list n))
				  env)
	 '()))


(defun empty-environment-p (env)
  "Test whether ENV is empty."
  (null (get-environment-names env)))


(defun get-environment-property (n prop env &key default)
  "Return PROP for N in ENV.

If there is no property associated with N then NIL will be
returned: this can be changed by defining the :DEFAULT argument."
  (if-let ((p (assoc prop (get-environment-properties n env))))
    (cadr p)

    ;; property doesn't exist, return the default
    default))


(defun set-environment-properties (n props env)
  "Replace the properties of N in ENV with PROPS."
  (set-frame-properties n props (get-frame-declaring n env)))


(defun set-environment-property (n prop v env)
  "Set the value of PROP of N in ENV to V.

This affects the shallowest declaration of N."
  (set-frame-property n prop v (get-frame-declaring n env)))


(defun variable-declared-in-environment-p (n env)
  "Test whether N is declared in ENV."
  (not (null (member n (get-environment-names env)))))


(defun declare-environment-variable (n props env &optional at-start)
  "Declare a variable N with properties PROPS in the shallowest frame of ENV.

If AT-START is non-nil, add the variable to the start of the environment;
otherwise (by default) add it to the emd.

Return the updated environment.

Signals a DUPLICATE-VARIABLE error if the variable already exists in this frame."
  (when (variable-declared-in-frame-p n env)
    (error 'duplicate-variable :variable n))

  ;; copy properties to avoid re-writing the original
  (if (null (decls env))
      (setf (decls env) (list (list n (copy-tree props))))

      (if at-start
	  ;; add to the start
	  (setf (decls env) (cons (list n (copy-tree props)) (decls env)))

	  ;; add to the end
	  (setf (cdr (last (decls env))) (list (list n (copy-tree props))))))

  ;; return the updated environment
  env)


(defun forget-environment-variable (name env)
  "Forget the definition of NAME in ENV.

  Returns the name of the variable forgotten."
  (forget-frame-variable name (get-frame-declaring name env)))


(defun add-frame-to-environment (f env &optional at-start)
  "Add all entries from F to ENV.

If AT-START is non-nil, add the entries to the start of ENV; otherwise
(by default) add them to the end. In either case the entries appear in
ENV in the same order as they appear in F.

Return ENV."
  (let ((ns (if at-start
		(reverse (get-frame-names f))
		(get-frame-names f))))

    (dolist (n ns)
      (let ((props (get-environment-properties n f)))
	(declare-environment-variable n props env at-start))))

  env)


(defun filter-frame (pred env)
  "Return a frame  containing all entries in the shallowest frame of ENV matching PRED.

PRED should be a predicate taking a name and the environment with the
frame containing that name."
  (let ((retained (remove-if-not (lambda (n)
				   (funcall pred n env))
				 (get-frame-names env)))
	(fenv (make-frame)))

    (dolist (n retained)
      (declare-environment-variable n (get-frame-properties n env) fenv))

    fenv))


(defun filter-environment (pred env)
  "Return an environment containing all the entries of ENV matching PRED.

PRED should be a predicate taking a name and the environment with the
frame containing that name on top (so that a call to GET-ENVIRONMENT-PROPERTY
will return the correct value for that variable at that depth)."
  (labels ((descend-env (l)
	     (if (null l)
		 nil

		 (let ((fenv (filter-frame pred l))
		       (penv (descend-env (parent-frame l))))
		   (setf (parent-frame fenv) penv)
		   fenv))))

    (descend-env env)))


(defun map-environment (f env)
  "Map F across each declaration in ENV.

F should be a function taking a name and an environment. The result is
a list of the values returned from F, from deepest to shallowest. The
list will be flat, regardless of the frame structure of ENV."
  (labels ((descend-env (l)
	     (if (null l)
		 nil

		 (append (descend-env (parent-frame l))
			 (list (mapcar (lambda (n)
					 (funcall f n l))
				       (get-frame-names l)))
			 ))))

    (flatten1 (remove-nulls (descend-env env)))))
