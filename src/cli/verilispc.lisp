;;;; Command line Verilisp-to-Verilog transpiler
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

(in-package :verilisp/cli)

;;; This command-line wrapper mainly justs ets up the "real" code
;;; for use in a shell script. The logic for com,puilation is all
;;; held within the verilisp package.


;;; ---------- Error handling ----------

(defvar *errors* 0
  "Number of errors caught.")


(defvar *warnings* 0
  "Number of warnings caught.")


(defvar *reported-warnings* ()
  "The warning conditions that should be reported.")


(defvar *fatal-warnings* ()
  "The warning conditions that should be treated as errors.")


(defvar *warning-flags* '(("dup"       duplicate-module duplicate-macro)
			  ("mismatch"  type-mismatch direction-mismatch coercion-mismatch
				       representation-mismatch precision-mismatch bitfield-mismatch)
			  ("declare"   unrecognised-declaration)
			  ("infer"     type-inferred)
			  ("resources" resources-created)
			  ("reach"     unreachable-code)
			  ("use"       unused-variable used-variable))
  "An alist mapping warning flags on the command line to warning class names.")


(defun reported-warning-condition-p (condition)
  "Test whether CONDITION should be reported as a warning."
  (member (type-of condition) *reported-warnings*))


(defun fatal-warning-condition-p (condition)
  "Test whether CONDITION should be treated as a fatal error."
  (member (type-of condition) *fatal-warnings*))


(defun flag-to-warnings (w)
  "Convert flag W to a list of warnings.

This uses *WARNING-FLAGS* to convert flags to conditions. It also
accepts \"none\" and \"all\" as abbreviations."
  (if-let ((a (assoc w *warning-flags* :test #'string-equal)))
    ;; flag found, return the associated condition
    (cdr a)

    ;; flag not found, try abbreviaions
    (cond ((string-equal w "all")
	   (flatten (mapcar #'cdr *warning-flags*)))

	  ((string-equal w "none")
	   '())

	  (t
	   ;; not recognised as a flag
	   (error "Flag ~s not recognised as a warning (valid options are ~s)"
		  w
		  (mapcar #'car *warning-flags*))))))


(defun add-reported-warning (w)
  "Add the warning corresponding to the given flag to those reported."
  (if-let ((ws (flag-to-warnings w)))
    (appendf *reported-warnings* ws)

    ;; no warnings, clear the list
    (setq *reported-warnings* nil)))


(defun add-fatal-warning (w)
  "Add the warning corresponding to the given flag to those treated as fatal."
  (if-let ((ws (flag-to-warnings w)))
    (appendf *fatal-warnings* ws)

    ;; no warnings, clear the list
    (setq *fatal-warnings* nil)))


;;; ---------- Module source file handling ----------


(defvar *module-source-file-names* nil
  "Map module names to their source files.")


(defun add-module-source-file-name (modname fn)
  "Add FN as the source file for MODNAME."
  (appendf *module-source-file-names* (list (list modname fn))))


(defun get-module-source-file-name (modname)
  "Retrieve the source file for MODNAME.

Reyirn NIL if the module isn't known."
  (if-let ((m (assoc modname *module-source-file-names*)))
    (cadr m)))


(defun record-new-modules (fn)
  "Record modules loaded from FN into *MODULE-SOURCE-FILE-NAMES*."
  (dolist (mm (get-modules-for-synthesis))
    (destructuring-bind (modname module)
	mm
      (declare (ignore module))
      (unless (get-module-source-file-name modname)
	(add-module-source-file-name modname fn)))))


;;; ---------- Command-line options handling ----------

(defvar *verbosity* 0
  "Verbosity (reporting) level (higher is more verbose).")


(defvar *fail-on-load* t
  "If T, don't synthesise any files if there were errors in loading any.")


(defvar *debug-on-error* nil
  "If T, enter the debugger when any Verilog errors occur.

This is intended for debugging the compiler, not the program being compiled.")


(defvar *output-file* nil
  "Name of file that gets the synthesised Verilog.

If absent, generate one file per module.")


(defvar *elaborated-file* nil
  "Name of file that gets the elaborated Lisp.

If absent, don't output the Lisp code.")


(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "Verbose output"
   :short #\v
   :long "verbose")
  (:name :continue-on-fail
   :description "Synthesise even if there were errors loading"
   :short #\c
   :long "continue")
  (:name :report-warnings
   :description "Report the given warning."
   :arg-parser (lambda (w) (add-reported-warning w))
   :short #\W
   :meta-var "WARNING")
  (:name :fatal-warnings
   :description "Treat the given warning as fatal."
   :arg-parser (lambda (w) (add-fatal-warning w))
   :short #\F
   :meta-var "WARNING")
  (:name :all-fatal-warnings
   :description "Treat all warnings as fatal."
   :long "strict")
  (:name :debug-on-error
   :description "Invoke the Lisp debugger on errors."
   :long "debug")
  (:name :output-file
   :description "File name for all synthesised code"
   :arg-parser (lambda (fn) (setq *output-file* fn))
   :short #\o
   :long "output-file"
   :meta-var "VERILOG-FILE")
  (:name :elaborated-file
   :description "File name for elaborated Lisp code"
   :arg-parser (lambda (fn) (setq *elaborated-file* fn))
   :short #\e
   :long "elaborated-file"
   :meta-var "LISP-FILE"))


(defun unknown-option (condition)
  (format t "warning: ~s option is unknown~%" (opts:option condition))
  (invoke-restart 'skip-option))


(defmacro when-option ((options opt) &body body)
  "Run forms in BODY when OPT if defined in OPTIONS."
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))


(defun parse-command-line ()
  "Parse the command line options.

This will set the various status flags and return
a list of files to be processed."
  ;; parse the command-line arguments
  (multiple-value-bind (options free-args)
      (handler-case
	  (handler-bind
	      ((opts:unknown-option #'unknown-option))
	    (opts:get-opts))

	(opts:missing-arg (condition)
	  (format t "Option ~s needs an argument~%"
		  (opts:option condition))
	  (uiop:quit 1))

	(opts:arg-parser-failed (condition)
	  (format t "Cannot parse ~s as argument of ~s~%"
		  (opts:raw-arg condition)
		  (opts:option condition))
	  (uiop:quit 1))

	(opts:missing-required-option (con)
	  (format t "Fatal: ~a~%" con)
	  (uiop:quit 1)))

    (when-option (options :verbose)
		 (setq *verbosity* 1))

    (when-option (options :debug-on-error)
		 (setq *debug-on-error* 1))

    (when-option (options :continue-on-fail)
		 (setq *fail-on-load* nil))

    (when-option (options :all-fatal-warnings)
		 (add-fatal-warning "all"))

    (when-option (options :help)
		 (opts:describe
		  :prefix   "Transpiler from Verilisp to Verilog."
		  :suffix   (format nil "Lisp files can use all of Lisp as long as they yield Verilisp.~%~%Valid WARNINGs are ~s."
				    (append '("all" "none") (mapcar #'car *warning-flags*)))
		  :usage-of (car (opts:argv))
		  :args     "[LISP-FILES]")
		 (uiop:quit 0))

    ;; return the files, which are all the non-option arguments
    free-args))


;;; ---------- File handling ----------

(defun filename-for-module (modname)
  "Return the filename used to store module MODNAME.

The filename is the name of the module in lowwr case with
a .v (Verilog) extension."
  (format nil "~(~a~).v" modname))


(defun iso-8601-timestamp ()
  "Return the ISO 8602 (RFC3339) timestamp for the current time.

See https://en.wikipedia.org/wiki/ISO_8601"
  (format-rfc3339-timestring nil (universal-to-timestamp (get-universal-time))))


(defun file-header ()
  "Return the file header placed at the top of each synthesised file.

The header will need to be preceded by an appropriate comment string."
  (format nil "Generated by verilispc at ~a~%" (iso-8601-timestamp)))


(defun filename-header (fn)
  "Return the header for synthesising FN.

FN should be the name of the Lisp file giving rise to the synthesised
Verilog.

The header will need to be preceded by an appropriate comment string."
  (format nil "Source: ~a~%" fn))


;;; ---------- Errors and progress reporting ----------

(defun info (format &rest args)
  "Generate an info message when we're in verbose mode."
  (when (> *verbosity* 0)
    (let ((line (format nil "~a" format)))
      (apply #'format `(,*error-output* ,line ,@args)))))


;;; Error handlers

(defun handle-vl-error (condition)
  (format *error-output* "ERROR: ~a~%" condition)
  (incf *errors*)

  ;; call debugger if requested
  (if *debug-on-error*
      (invoke-debugger condition)

      ;; continue compilation if possible
      (if-let ((recovery (find-restart 'recover)))
	(invoke-restart recovery)

	;; otherwise skip the file
	(invoke-restart 'ignore-file-with-errors))))


(defun handle-error (condition)
  (format *error-output* "SYSTEM ERROR: ~a~%" condition)
  (incf *errors*)

  ;; call debugger if requested
  (if *debug-on-error*
      (invoke-debugger condition)

      ;; skip the rest of the file (don't try to recover)
      (invoke-restart 'ignore-file-with-errors)))


(defun handle-vl-warning (condition)
  (cond ((fatal-warning-condition-p condition)
	 (format *error-output* "ERROR: ~a~%" condition)
	 (incf *errors*))

	((reported-warning-condition-p condition)
	 (format *error-output* "WARNING: ~a~%" condition)
	 (incf *warnings*)))

  ;; call debugger if requested, otherwise just continue
  (if (and *debug-on-error*
	   (fatal-warning-condition-p condition))
      (invoke-debugger condition)

      (muffle-warning condition)))


(defun continue-or-fail ()
  (when (and *fail-on-load*
	     (> *errors* 0))
    (when (> *warnings* 0)
      (format *error-output* "~s warnings~%" *warnings*))
    (when (> *errors* 0)
      (format *error-output* "~s errors~%" *errors*))
    (uiop:quit 1)))


;;; Helper macro

(defmacro with-error-handling (&body body)
  "Run BODY in an environment that handles warnings and errors appropriately.

Warnings are reported as requested and/or treated as fatal, causing processing
to continue after recovery. Errors are similarly recovered. After BODY has
run, errors will usually cause an exit unless failure-on-load has been
explicitly disabled."
  `(progn
     (handler-bind
	 ((vl-error #'handle-vl-error)
	  (error #'handle-error)
	  (vl-warning #'handle-vl-warning))

       ,@body)

     ;; decide whether to bail out
     (continue-or-fail)))


(defmacro with-ignore-file-with-errors (&body body)
  "Run BODY in an environment offering an IGNORE-FILE-WITH-ERRORS restart.

This restart is used by the handlers establish by WITH-ERROR-HANDLING
to skip files with errors."
  `(restart-case
       (progn
	 ,@body)

     (ignore-file-with-errors ()
       :report "Ignore this file and continue"

       (format *error-output* "Skipping the rest of ~a~%" fn)
       nil)))


;;; ---------- Entry point ----------

;; Package used to hold loaded code
(defpackage verilisp/cli/compiling
  (:use :cl :verilisp :verilisp/core))


(defun main ()
  (let ((files (parse-command-line)))

    ;; quit if no files given
    (when (= (length files) 0)
      (format *error-output* "No input files given~%")
      (uiop:quit 0))

    (with-error-handling
      ;; load all the source files in order
      (let ((*package* (find-package :verilisp/cli/compiling)))
	(dolist (fn files)
	  (with-ignore-file-with-errors
	      ;; use standard input if - appears as a filename
	      ;; TBD

	      (info "Loading ~a~%" fn)
	    (load fn :if-does-not-exist t)
	    (record-new-modules fn)))))

    (with-error-handling
	;; generate elaborated Lisp if requested
	(if *elaborated-file*
	    (with-open-file (str *elaborated-file* :direction :output
						   :if-exists :supersede)
	      (format str ";; ")
	      (format str (file-header))
	      (format str "~%")

	      (info "Writing elaborated Lisp to ~a~%" *elaborated-file*)
	      (let ((*print-case* :downcase)
		    (*print-base* 10)
		    (*package* (find-package :verilisp/cli/compiling)))
		(dolist (mm (get-modules-for-synthesis))
		  (with-ignore-file-with-errors
		      (destructuring-bind (modname module)
			  mm
			(let ((fn (get-module-source-file-name modname)))
			  (format str ";; ")
			  (format str (filename-header fn)))
			(pprint module str)
			(format str "~%~%"))))))))

    (with-error-handling
	(if *output-file*
	    ;; generate one file for all modules
	    (with-open-file (str *output-file* :direction :output
					       :if-exists :supersede)
	      (format str "// ")
	      (format str (file-header))
	      (format str "~%")

	      (info "Compiling all modules to ~a~%" *output-file*)
	      (dolist (mm (get-modules-for-synthesis))
		(with-ignore-file-with-errors
		    (destructuring-bind (modname module)
			mm
		      (info "Compiling ~a~%" modname)
		      (let ((fn (get-module-source-file-name modname)))
			(format str "// ")
			(format str (filename-header fn))
			(format str "~%"))

		      (synthesise/vl module str)
		      (format str "~%")))))

	    ;; generate the Verilog for each module
	    (dolist (mm (get-modules-for-synthesis))
	      (with-ignore-file-with-errors
		  (destructuring-bind (modname module)
		      mm
		    (let ((fn (filename-for-module modname)))
		      (info "Compiling ~a to ~a~%" modname fn)
		      (with-open-file (str fn :direction :output
					      :if-exists :supersede)
			(format str "// ")
			(format str (file-header))
			(format str (filename-header fn))
			(format str "~%")

			(synthesise/vl module str))))))))

    ;; if we get here we didn't bail-out earlier, so do a successful exit
    (uiop:quit 0)))
