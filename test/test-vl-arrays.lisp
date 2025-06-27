;; Tests of arrays
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

(in-package :verilisp/test)
(in-suite verilisp/vl)


;; ---------- Array declarations ----------

(test test-array-decl
  "Test we can declare arrays."
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(make-array '(16)
						  :element-type (unsigned-byte 8))))
		    '(array (unsigned-byte 8) (16))))

  ;; version without the Lisp-compatible quote on the shape
  (is (vl:subtype-p (vl:typecheck (vl:expand/vl '(make-array (16)
						  :element-type (unsigned-byte 8))))
		    '(array (unsigned-byte 8) (16))))

  ;; at the moment we only allow one dimension
  (signals (vl:not-synthesisable)
    (vl:typecheck (vl:expand/vl '(make-array '(16 16)
				  :element-type (unsigned-byte 8))))))


(test test-array-bind
  "Test we can bind arrays in LET forms."
  (let ((p (vl:expand/vl '(let ((a (make-array '(16)
				    :element-type (unsigned-byte 8)))
				(b 0))
			   (setq b 12)))))

    (is (vl:subtype-p (vl:typecheck p)
		      '(unsigned-byte 8)))))


(test test-synthesise-array-decl
  "Test we can synthesise array declarations."
  (let ((p (vl:expand/vl '(let ((a (make-array '(16)
				    :element-type (unsigned-byte 8))
				 :type (array (unsigned-byte 8) (32)))
				(b (make-array '(8)
				    :element-type (unsigned-byte 8))
				 :type (array (unsigned-byte 8) (16))
				 :as :wire)
				(c 10 :type (unsigned-byte 8)))
			   (setf c 0)))))

    (vl:typecheck p)
    (is (vl:synthesise p))))


(test test-synthesise-array-decl-type-inferred
  "Test we can synthesise array declarations when we infer the type of the array."
  (let ((p (vl:expand/vl '(let ((a (make-array '(16)
				    :element-type (unsigned-byte 8)))
				(b (make-array '(8)
				    :element-type (unsigned-byte 8))
				 :as :wire)
				(c 10))
			   (setf c 100)))))

    (vl:typecheck p)
    (is (vl:synthesise p))))


(test test-synthesise-array-init-from-data
  "Test we can synthesise array declarations with initial data inline."
  (let ((p (vl:expand/vl '(let ((b (make-array '(4)
				    :element-type (unsigned-byte 8)
				    :initial-contents '(1 2 3 4))
				 :as :register)
				(c 10))
			   (setf c (aref b 1))))))

    (vl:typecheck p)
    (is (vl:synthesise p))))


(test test-synthesise-array-init-from-file
  "Test we can synthesise array declarations with initial data from a file."
  (vl::clear-module-late-initialisation)

  (let ((p (vl:expand/vl '(let ((b (make-array '(4)
				    :element-type (unsigned-byte 8)
				    :initial-contents '(:file "ttt.hex"))
				 :as :register)
				(c 10))
			   (setf c (aref b 1))))))

    (vl:typecheck p)
    (vl:synthesise p)
    (is (vl::module-late-initialisation-p))))


;; ---------- Array accesses ----------

(test test-aref-simple
  "Test we can index into an array."
  (let ((p (vl:expand/vl '(let ((a (make-array '(16)
				    :element-type (unsigned-byte 32))))
			   (setf (aref a 8) (aref a 0))))))

    (is (vl:subtype-p (vl:typecheck p)
		      '(unsigned-byte 32)))))


(test test-aref-bits
  "Test we can bit-index into an element of an array."
  (let ((p (vl:expand/vl '(let ((a (make-array '(16)
				    :element-type (unsigned-byte 32))))
			   (setf (vl::bref (aref a 8) 3 :end 0)
			    (vl::bref (aref a 0) 3 :end 0))))))

    (is (vl:subtype-p (vl:typecheck p)
		      '(unsigned-byte 32)))))


(test test-synthesise-aref-simple
  "Test we can synthesise a simple array reference."
  (let ((p (vl:expand/vl '(let ((a (make-array '(16)
				    :element-type (unsigned-byte 32))))
			   (setf (aref a 8) (aref a 0))))))

    (vl:typecheck p)
    (is (vl:synthesise p))))


;; ---------- Generalised place ----------

(test test-aref-dependencies
  "Test we can extract aref dependencies properly."
  (vl:with-new-frame
    (vl::declare-variable 'a '((:initial-value (make-array '(16)
						:element-type (unsigned-byte 8)))))
    (vl::declare-variable 'b '((:type (unsigned-byte 8))
			       (:initial-value 24)))
    (vl::declare-variable 'c '((:type (unsigned-byte 8))
			       (:initial-value 0)))
    (vl::declare-variable 'd '((:type (unsigned-byte 8))
			       (:initial-value 0)
			       (:as :constant)))

    (vl:expand/vl '(progn
		    (setq b (+ (aref a 23) 19))
		    (setq c (+ (aref a d) b))))

    (is (set-equal (vl::variable-property 'b :depends-on)
		   '(a)))
    (is (set-equal (vl::variable-property 'c :depends-on)
		   '(a b)))))


(test test-aref-target
  "Test we can use array elements as targets."
  (vl:with-new-frame
    (vl::declare-variable 'a '((:initial-value (make-array '(16)
						:element-type (unsigned-byte 8)))))
    (vl::declare-variable 'b '((:type (unsigned-byte 8))
			       (:initial-value 24)))
    (vl::declare-variable 'c '((:type (unsigned-byte 8))
			       (:initial-value 0)))

    (vl:expand/vl '(setq (aref a 1) b))
    (is (set-equal (vl::variable-property 'a :depends-on)
		   '(b)))
    (is (null (vl::variable-property 'b :depends-on)))

    (vl:expand/vl '(setq (aref a 1) (aref a c)))
    (is (set-equal (vl::variable-property 'a :depends-on)
		   '(a b c))) ; b from the previous form

    (is (set-equal (vl:updated-variables '(setq (aref a 1) b))
		   '(a)))
    (is (set-equal (vl:updated-variables '(setq (aref a 1) (aref a c)))
		   '(a)))))


;; ---------- Initialisation ----------

(test test-typecheck-array-initialiser
  "Test we can typecheck an array initialiser."
  (let ((p (vl:expand/vl '(let ((a (make-array (5)
				    :initial-contents (1 2 3 4 5))))
			   (aref a 0)))))
    (is (vl:subtype-p (vl:typecheck p)
		      '(unsigned-byte 8))))

  (signals (vl:shape-mismatch)
    (vl:typecheck
     (vl:expand/vl (copy-tree '(let ((a (make-array (5)
					 :initial-contents (1 2 3))))
				(aref a 0)))))))


(test test-typecheck-array-initialiser-bad-value
  "Test we can detect a badly-typed value in an array initialiaser."
  (signals (vl:shape-mismatch)
    (vl:typecheck (vl:expand/vl '(let ((a (make-array (5)
					   :element-type '(unsigned-byte 4)
					   :initial-contents (1 2 35))))
				  (aref a 0))))))


(test test-syntheseise-array-init
  "Test we can synthesise array initialisation."
  (let ((p (vl:expand/vl '(let ((a (make-array '(10)
				    :initial-contents '(1 2 3 4 5 6 7 8 9 10)))
				(b 0))
			   (setf b (aref a 1))))))
    (vl:typecheck p)
    (is (vl:synthesise p))))


;; The next test uses ROM data from the SAP-1 example

(test test-synthesise-array-init-from-example
  "Test we can synthesise array initialisation from a file."
  (let* ((fn (pathname-relative-to-project-root "examples/sap-1-raw/program.bin"))
	 (p (vl:expand/vl `(let ((a (make-array '(10)
						:initial-contents '(:file ,fn)))
				 (b 0))
			     (setf b (aref a 1))))))

    (vl:typecheck p)
    (is (vl:synthesise p))
    (vl::run-module-late-initialisation)))
