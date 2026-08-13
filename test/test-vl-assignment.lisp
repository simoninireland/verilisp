;; Tests of simple assignments and generalised places
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


;; ---------- Simple assignment (SETQ) ----------

(test test-setq
  "Test we can typecheck the SETQ form."
  (vl::with-new-frame
    (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 13))
						       (setq a 9))))
		       '(unsigned-byte 4)))

    (signals (vl::access-mismatch)
      (vl::typecheck (vl::expand/vl '(let ((a 12))
				      (declare (as constant a))
				      (setq a 9)))))))


(test test-assignment-to-constant
  "Test we can't assign to a constant."
  (signals (not-synthesisable)
    (vl::typecheck (vl::expand/vl '(let ((a 12))
				    (setf 24 a))))))


(test test-assignment-same-width
  "Test we can assign."
  (vl::with-new-frame
    (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 10))
						       (setq a 12))))
		       '(unsigned-byte 5)))))


(test test-assignment-too-wide
  "Test we catch assigning a value that's too wide for its explicit type."
  (signals (vl::type-mismatch)
    (vl::with-new-frame
      (vl::typecheck (vl::expand/vl '(let ((a 10))
				      (declare (type (unsigned-byte 5) a))
				      (setq a 120)))))))


(test test-assignment-too-wide-widenable
  "Test we can assign a value to a variable that can be widened."
  (vl::with-new-frame
    (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 10))
						       (setq a 120))))
		       '(unsigned-byte 7)))))


(test test-assignment-too-wide-updated
  "Test we don't update the type when we have an explicit one already."
  (vl::with-new-frame
    (let ((p (vl::expand/vl '(let ((a 10))
			      (declare (type (unsigned-byte 5) a))
			      (setq a 120)))))
      (vl::subtype-p (vl::typecheck p)
		     '(unsigned-byte 7)))))


(test test-assignment-too-wide-widenable-updated
  "Test we update the code to match inferred types."
  (vl::with-new-frame
    (let ((p (vl::expand/vl '(let ((a 10))
			      (setq a 120)))))
      (vl::subtype-p (vl::typecheck p)
		     '(unsigned-byte 7)))))


(test test-assignment-out-of-scope
  "Test we can't assign to a non-existent variable."
  (signals (vl::unknown-variable)
    (vl::with-new-frame
      (vl::typecheck (vl::expand/vl '(let ((a 10))
				      (setq b 12)))))))


(test test-assignment-constant
  "Test we can't assign to a constant variable."
  (signals (vl::access-mismatch)
    (vl::with-new-frame
      (vl::typecheck (vl::expand/vl '(let ((a 10))
				      (declare (as constant a))
				      (setq a 12)))))))


(test test-synthesise-setq
  "Test we can synthesise/vlassignments."
  ;; as statements
  (is (vl::synthesise/vl '(setq a 5))))


(test test-typecheck-setq-generalised-place
  "Test we catch the common mistake of using SETQ when we mean SETF."
  (signals (vl::not-synthesisable)
    (vl::with-new-frame
      (vl::typecheck (vl::expand/vl '(let ((a 0))
				      (setq (bref a 0) 1)))))))


(test test-setq-dependencies
  (vl::with-new-frame
  "Test we can extract SETQ dependencies."
    (vl::declare-variable 'a '((type (unsigned-byte 8))))
    (vl::declare-variable 'b '((type (unsigned-byte 8))))
    (vl::declare-variable 'c '((type (unsigned-byte 8))))
    (vl::declare-variable 'd '((type (unsigned-byte 8))))
    (vl::declare-variable 'e '((type (unsigned-byte 8))))

    (let ((p (vl::expand/vl '(setq a (+ b c 23)))))
      (vl:typecheck p)
      (is (set-equal (vl::variable-property 'a 'depends-on)
		     '(b c))))

    (let ((p (vl::expand/vl '(setq d (+ a b 1)))))
      (vl::typecheck p)
      (is (set-equal (vl::variable-property 'd 'depends-on)
		     '(a b)))

      ;; if we travese the dependencies of d we should see c via a
      (is (set-equal (vl::traverse-dependencies '(d))
		     '(a b c))))

    (let ((p (vl::expand/vl '(setq e (+ e 1)))))
      (vl:typecheck p)
      (is (set-equal (vl::variable-property 'e 'depends-on)
		     '(e))))))


;; ---------- Generalised places (SETF) ----------

(test test-setf-as-setq
  "Test we can convert a simple SETF into a SETQ."
  (vl::with-new-frame
    (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 12))
						       (setf a 9))))
		       '(unsigned-byte 4)))))


(test test-synthesise-setf-conditional
  "Test we can synthesise/ SETF with a conditional value."
  (vl::with-new-frame
    (let ((p (vl::expand/vl '(let ((a 12)
				   (b 2))
			      (declare (type (unsigned-byte 8) a b))
			      (setf a (if (= b 1)
					  a
					  (+ a 1)))))))
      (vl::typecheck p)
      (is (vl::synthesise/vl p)))))


(test test-synthesise-setf-conditional-not-simple
  "Test we can't synthesise SETF with a non-simple expression."
  (vl::with-new-frame
    (let ((p (vl::expand/vl '(let ((a 12)
				   (b 2)
				   (c 0))
			      (declare (type (unsigned-byte 8) a b c))
			      (setf a (if (= b 1)
					  a
					  (setf c 1)
					  (+ a 1)))))))
      (vl::typecheck p)
      (signals not-synthesisable
	(is (vl::synthesise/vl p))))))


;; Tests of the actual generalised place forms appear in their
;; respective test files.


;;; ---------- Variables ----------

;;; The READ-VARIABLES-SETF and WRITTEN-VARIABLES-SETF passes
;;; refer to forms in the first (generalised place) argument position
;;; of a SETF.

(test test-setf-read-variables
  "Test we can extract the read variables correctly."
  (is (null (vl::read-variables-setf 'a)))

  (is (null (vl::read-variables-setf '(aref a 5))))
  (is (set-equal (vl::read-variables-setf '(aref a 1 2 b)) '(b)))

  (is (null (vl::read-variables-setf '(bref b 1 :end 4))))
  (is (set-equal (vl::read-variables-setf '(bref b c :end 4)) '(c)))

  (is (set-equal (vl::read-variables-setf '(aref a (bref b 1 :end 4))) '(b)))
  (is (set-equal (vl::read-variables-setf '(bref b (aref a 8) :end 4)) '(a))))


(test test-setf-written-variables
  "Test we can identify the written variables."
  (with-new-frame
    (vl::declare-variable 'v '((type (unsigned-byte 8))))
    (vl::declare-variable 'c '((type (unsigned-byte 8))))
    (vl::declare-variable 'a '((type (array (unsigned-byte 8)))
			       (initial-value (make-array (10) :element-type (unsigned-byte 8)))))

    (is (set-equal (written-variables-setf 'v) '(v)))

    (is (set-equal (written-variables-setf '(aref a 5)) '(a)))
    (is (set-equal (written-variables-setf '(aref a v)) '(a)))

    (is (set-equal (written-variables-setf '(bref v 1 :end 4)) '(v)))
    (is (set-equal (written-variables-setf '(bref v c :end 4)) '(v)))

    (is (set-equal (written-variables-setf '(aref a (bref v 6 :end 4))) '(a)))
    (is (set-equal (written-variables-setf '(bref v (aref a 1) :end 4)) '(v)))))
