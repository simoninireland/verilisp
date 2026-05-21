;;;; Compiler nanopass queue management
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


(defvar *pass-queues* nil
  "Alist from queue name tags used in DEFPASS :QUEUE options to pass queues.")


(defun pass-queue-p (queue-name)
  "Test whether QUEUE-NAME is the name of a defined pass queue."
  (assoc queue-name *pass-queues*))


(defun ensure-pass-queue (queue-name)
  "Ensure QUEUE-NAME is the name of a defined pass queue."
  (unless (pass-queue-p queue-name)
      (error 'dsl-error :hint (format nil "No pass queue ~s defined" queue-name))))


(defun get-pass-queue-assoc (queue-name)
  "Return the pair of QUEUE-NAME and queue."
   (if-let ((a (assoc queue-name *pass-queues*)))
     a

     (error 'dsl-error :hint (format nil "No pass queue ~s defined" queue-name))))


(defun get-pass-queue (queue-name)
  "Return the pass queue named QUEUE-NAME."
  (cdr (get-pass-queue-assoc queue-name)))


(defun pass-on-pass-queue-p (pass-name queue-name)
  "Test whether PASS-NAME appears on QUEUE-NAME."
  (member (pass-wrap-level-function-name pass-name)
	  (get-pass-queue queue-name)))


(defun add-pass-to-queue (pass-name queue-name &key prepend)
  "Add pass with name PASS-NAME to QUEUE-NAME.

The queue stores the name of the wrapper function for PASS-NAME,
which is the function that's called when the queue is run.

If :PREPEND is non-NIL then the pass is added to the front of the
queue; otherwise it is added to the back. If a pass called PASS-NAME
already exists on QUEUE-NAME then nothing happens."
  (declare (optimize debug))

  (ensure-pass-queue queue-name)

  (unless (pass-on-pass-queue-p pass-name queue-name)
    (let ((a (assoc queue-name *pass-queues*))
	  (wrap-level-f (pass-wrap-level-function-name pass-name)))
      (if prepend
	  (setf (cdr a) (cons wrap-level-f (cdr a)))

	  (appendf (cdr a) (list wrap-level-f))))))


(defun clear-pass-queue (queue-name)
  "Clear all passes from queue QUEUE-NAME."
  (setf (cdr (get-pass-queue-assoc queue-name)) nil))


(defun run-pass-queue (queue-name form)
  "Run the passes of QUEUE-NAME in order against FORM.

The return value of each pass is used as the input to the next."
  (let ((f form))
    (dolist (pass-name (get-pass-queue queue-name))
      (setq f (funcall pass-name f)))

    ;; return the result of the last pass
    f))


;;; ---------- Defining pass queues ----------

(defmacro define-pass-queue (queue-name)
  "Define and install a new pass queue QUEUE-NAME.

Repeatedly adding a queue does nothing."
  `(unless (pass-queue-p ',queue-name)
     (appendf *pass-queues* (list (list ',queue-name)))))
