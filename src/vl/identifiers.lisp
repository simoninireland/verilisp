;; Identifiers
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

;; There are probably more reserved words
(defvar *reserved-words* '("module" "input" "output" "inout"
			   "always" "@"  "posedge" "negedge" "assign"
			   "parameter" "localparam" "reg" "wire"
			   "signed")
  "Reserved words in Verilog.")


(defvar *identifier-legal-leading-character-regexp* (create-scanner "[A-Za-z_]")
  "Regexp identifying the characters allowed as the first character of an identifier.")


(defvar *identifier-illegal-character-regexp* (create-scanner "[^A-Za-z0-9_]")
  "Regexp identifying all characters that are illegal in an identifier.")


(defvar *identifier-illegal-character-replacement* "_"
  "The character used to replace any illegal characters in identifiers.

This is also added to the front of any identifiers that clash
with *RESERVED-WORDS*.")


(defun make-legal-identifier (n)
  "Make a legal identifier from N.

Any non-permitted characters (identifiers by *IDENTIFIER-ILLEGAL-CHARACTER-REGEXP*)
are replaced with instances of *IDENTIFIER-ILLEGAL-CHARACTER-REPLACEMENT*. If
the resulting identifier is a reserved word it is prefixed with an instance
of *IDENTIFIER-ILLEGAL-CHARACTER-REPLACEMENT*."

  ;; replace any unacceptable characters
  (let ((no-illegal (regex-replace-all *identifier-illegal-character-regexp*
				       (if (symbolp n)
					   (symbol-name n)
					   n)
				       *identifier-illegal-character-replacement*)))

    ;; prepend any keywords
    (if (member no-illegal *reserved-words* :test #'string-equal)
	(setq no-illegal (concat *identifier-illegal-character-replacement* no-illegal)))

    ;; prepend any identifiers not starting with a legal starting character
    (when (not (scan *identifier-legal-leading-character-regexp* (s-first no-illegal)))
      (setq no-illegal (concat *identifier-illegal-character-replacement* no-illegal)))

    ;; bomb-out if the identifier is still illegal
    (when (string-equal no-illegal "_")
      (error 'not-synthesisable :hint "Use meaningful variable names :-)"))

    ;; return the legalised identifier or nil if unchanged
    (if (string-equal n no-illegal)
	nil
	(ensure-symbol no-illegal))))


(defun ensure-legal-identifier (n)
  "Return a legal version of N.

This will either be N itself or the legalised version
constructed by MAKE-LEGAL-IDENTIFIER."
  (or (make-legal-identifier n)
      n))


(defun legal-identifier-p (n)
  "Test whether N is a legal identifier."
  (null (make-legal-identifier n)))
