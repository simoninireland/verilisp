;; Tests of binders
;;
;; Copyright (C) 2024--2026 Simon Dobson
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


(test test-let-single
  "Test we can typecheck an expression."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 1))
						     (declare (type (unsigned-byte 5) a))
						     (+ 1 a))))
		    '(unsigned-byte 6))))


(test test-let-at-least-one
  "Test that a variable gets at least a width of one bit."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 0))
						     a)))
		     '(unsigned-byte 1))))


(test test-let-single-infer-width
  "Test we can infer a width."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 1))
						  (+ 1 a))))
		    '(unsigned-byte 2))))


(test test-let-double
  "Test we can typecheck an expression with two variables."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 1)
							  (b 6))
						     (declare (width 8 a)
						      (width 12 b))
						     (+ a b))))
		    '(unsigned-byte 13))))


(test test-let-too-narrow
  "Test we pick up too-wide initial values."
  (signals (vl::type-mismatch)
    (vl::typecheck (vl::expand/vl '(let ((a 100))
				    (declare (type (unsigned-byte 5) a))
				    (+ 1 a))))))


(test test-let-widen
  "Test we can take the width from a given type."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 5))
						     (declare (type (unsigned-byte 8) a))
						     (setf a (+ 1 a)))))
		    '(unsigned-byte 9))))


(test test-let-missing-width-type-conflicts
  "Test we pick up an inferred width conflicting with a set type"
  (signals (vl::type-mismatch)
    (vl::typecheck (vl::expand/vl '(let ((a 5))
				    (declare (type (unsigned-byte 4) a))
				    (setq a 17))))))


(test test-let-scope
  "Test we catch variables not declared."
  (signals (vl::unknown-variable)
    (vl::typecheck (vl::expand/vl '(let ((a 1))
				  (+ 1 b))))))


(test test-let-result
  "Test we pick up the right result type."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 99)
							  (b 100))
						     (declare (width 8 b))
						     (+ b 1)
						  (+ b a b))))
		    '(unsigned-byte 10))))


(test test-let-constant
  "Test we admit constant bindings."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 15))
						     (declare (as constant a))
						     a)))
		    '(unsigned-byte 4))))


(test test-let-naked
  "Test that we accept "naked" declarations."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 10)
						       b)
						  (+ a b))))
		    `(unsigned-byte 5))))


(test test-binders-conditional
  "Test we can assign to a conditional."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 1)
						       (b 23))
						  (let ((c (if (= a 0)
							       23
							       1)))
						    (declare (as wire c)
							     (type (unsigned-byte 32) c))
						    (setq a c)))))
		    '(unsigned-byte 32))))


(test test-let-two-arms
  "Test we can unify types where constraints need to propagate down the tree."
  (let ((p (vl::expand/vl '(let (a)
			    (let (c)
			      (setq c (+ a 2))) ; this depends on the final type of A...
			    (setq a 23)))))     ; ...which changes after the inner LET
						; has been processed
    (vl::typecheck p)

    (let ((outer (elt p 1))                  ; outer LET's decls
	  (inner (elt (elt p 2) 1))) ; inner LET's decls
      (vl::with-local-frame outer
	(is (vl::subtype-p (get-type 'a)
			   '(unsigned-byte 5))))    ; type coming from the lower SETQ

      (vl::with-local-frame inner
	(is (vl::subtype-p (get-type 'c)
			   '(unsigned-byte 6))))))) ; type based on the final type,
						    ; not the one know when the inner
						    ; LET was processed


(test test-binders-free-variables
  "Test we can extract free variables correctly"
  (let ((p (vl::expand/vl '(let (b)
			    (let ((a 10))
			      (+ a b))))))
    (vl::typecheck p)
    (is (equal (vl::read-variables (caddr p)) ; body of the outer LET
	       '(b))))

  (let ((p (vl::expand/vl '(let ((a 10)
				 b)
			    (+ a b)))))
    (vl::typecheck p)
    (is (null (vl::read-variables p)))))


(test test-synthesise-binders
  "Test we can synthesise binders."
  ;; as statements
  (dolist (x '((let ((a 1))
		 (declare (width 8 a))
		 (setf a (+ a 1)))
	       (let ((a 1)
		     (b 23)
		     c)
		 (declare (width 8 a)
			  (as constant b))
		 (setf c (+ a b)))
	       (let ((a 1)
		     (b 0)
		     c)
		 (declare (width 8 a)
			  (width 16 b)
			  (as wire b))
		 (setf c (+ a b 1)))))

    (let ((p (vl::expand/vl (copy-tree x))))
      (vl::typecheck p)
      (is (vl::synthesise p)))))


(test test-synthesise-binders-not-simple
  "Test we can't synthesise some binders with complicated RHSs."
  ;; simple RHS (although an expression)
  (vl::with-new-frame
    (vl::declare-variable 'b '((type (unsigned-byte 8))
			       (initial-value 12)))

    (let ((p (vl::expand/vl '(let ((a (+ 10 (- 9 b)))) a))))
      (vl::typecheck p)
      (is (vl::synthesise p))))

  ;; nested if expression RHS
  (let ((p (vl::expand/vl '(let ((a (+ 10 (if (> 1 2) 1 2)))) a))))
    (vl::typecheck p)
    (is (vl::synthesise p)))

  ;; RHS that accesses an array
  (let ((p (vl::expand/vl '(let ((a (make-array (10) :element-type (unsigned-byte 8))))
			    (let ((b (aref a 5)))
			      b)))))
    (vl::typecheck p)
    (is (vl::synthesise p)))

  ;; nested if that's got a complicated body
  (let ((p (vl::expand/vl '(let (a (b (+ 10 (if (< 1 2) 1 (setf a 27))))) a))))
    (vl::typecheck p)
    (signals not-synthesisable
      (is (vl::synthesise p)))))


(test test-let-width
  "Test the :width shortcuts works."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 0))
						     (declare (width 12 a))
						     a)))
		     '(unsigned-byte 12))))


(test test-let-float-order
  "Test we maintain the order of declarations when we float LET blocks."
  (let ((p (vl::expand/vl '(let ((a 1)
				(b 2)
				c)
			   (setf c (+ a b))
			   (let ((d 4)
				 e
				 (f 6)
				 g)
			     (setf e 5))))))

    (vl::typecheck p)
    (let* ((q-env (vl::float-let-blocks p))
	   (q (car q-env))
	   (env (cadr q-env)))
      (is (equal (vl::get-frame-names env)
		 '(a b c d e f g))))))


(test test-let-dependencies
  "Test we can extract dependencies properly."
  (vl::with-new-frame
    (vl::declare-variable 'a '((type (unsigned-byte 8))
			       (initial-value 12)))
    (vl::declare-variable 'b '((type (unsigned-byte 8))
			       (initial-value 24)))
    (vl::declare-variable 'c '((type (unsigned-byte 8))
			       (initial-value 0)))

    (let ((p (vl::expand/vl  '(progn
			       (setq a (+ b 19))
			       (setq c a)))))
      (vl::typecheck p)

      (is (set-equal (vl::variable-property 'a 'depends-on)
		     '(b)))
      (is (null (vl::variable-property 'b 'depends-on)))
      (is (set-equal (vl::variable-property 'c 'depends-on)
		     '(a))))))


(test test-let-declarations
  "Test the detailed behaviour of declarations."
  (vl::with-new-frame
    (vl::declare-variable 'a '())
    (vl::declare-variable 'b '())
    (vl::declare-variable 'c '())

    (vl::expand/vl '(declare (type (unsigned-byte 16) a b)
		     (width 8 c)))
    (is (vl::subtype-p (vl::get-type 'a) '(unsigned-byte 16)))
    (is (vl::subtype-p (vl::get-type 'b) '(unsigned-byte 16)))
    (is (vl::subtype-p (vl::get-type 'c) '(unsigned-byte 8)))

    (vl::expand/vl '(declare (as wire a)))
    (is (eql (vl::get-representation 'a) 'wire))

    (signals (vl::unrecognised-declaration)
      (vl::expand/vl '(declare (temp a b c))))

    (signals (vl::unknown-variable)
      (vl::expand/vl '(declare (type bit d))))))


(test test-let-representation-inference
  "Check we infer the right representations."
  (vl::with-new-frame
    (let* ((p (expand/vl `(let ((a 23)
				(b (+ 56 17))
				(c 0))
			    (setq a (+ c b))
			    (let ((d (+ a b)))
			      (setq c (bref d 2 :end 0)))))))
      (vl::typecheck p)

      (let ((decls (cadr p)))		; outer LET
	(with-local-frame decls
	  (is (eql (vl::variable-property 'a 'as) 'register))
	  (is (eql (vl::variable-property 'b 'as) 'constant))
	  (is (eql (vl::variable-property 'a 'as) 'register))))

      (let ((decls (elt (elt p 3) 1)))	; inner LET
	(format t "~a" decls)
	(with-local-frame decls
	  (is (eql (vl::variable-property 'd 'as) 'wire)))))))


;; ---------- Variable accesses ----------

(test test-let-accesses
  "Test we can extract and update variable accesses in LET."
  (let ((p (vl::expand/vl '(let (a b c)
			    (setq a 1)
			    (setq b (+ a 1))
			    (setq c (+ a c))))))
    (vl::typecheck p)

    ;; no free variables
    (let ((vas (vl::read-variables p)))
      (is (null vas)))

    (let ((decls (cadr p)))
      (with-local-frame decls
	(is (vl::variable-property 'a 'written))
	(is (vl::variable-property 'a 'read))
	(is (vl::variable-property 'b 'written))
	(is (not (vl::variable-property 'b 'read)))
	(is (vl::variable-property 'c 'written))
	(is (vl::variable-property 'c 'read))))))


;; ---------- Variable declarations ----------

(test test-declare-non-local
  "Test we can't add declarations to variables from shallower frames."
  (signals (vl::unknown-variable)
    (vl::expand/vl '(let (a b c)
		     (declare (type bit a))

		     (let (d)
		       (declare (type (unsigned-byte 8) c)))))))
