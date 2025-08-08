;; Tests of modules
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
;; GNU General Public License for more details.q
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/vl)


;; ---------- Module lambda-lists ----------

(test test-module-lambda-list
  "Test the parsing of module lambda lists."

  ;; successful examples
  (is (tree-equal (vl::parse-module-lambda-list '(a b c))
		  '((a b c) nil nil)))
  (is (tree-equal (vl::parse-module-lambda-list '(a b &optional c))
		  '((a b) (c) nil)))
  (is (tree-equal (vl::parse-module-lambda-list '(a b &optional c (d 5)))
		  '((a b) (c (d 5)) nil)))
  (is (tree-equal (vl::parse-module-lambda-list '(a b &key c))
		  '((a b) nil (c))))
  (is (tree-equal (vl::parse-module-lambda-list '(a b &key c (d 1)))
		  '((a b) nil (c (d 1)))))
  (is (tree-equal (vl::parse-module-lambda-list '(a b &optional c (d 5) &key (e 34)))
		  '((a b) (c (d 5)) ((e 34)))))

  ;; unsuccessful examples
  (signals (syntax-error)
     (vl::parse-module-lambda-list '()))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b a)))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b (c 34))))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b &optional a)))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b &optional c c)))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b &key f &optional a)))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b &rest body)))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b &body body)))
  (signals (syntax-error)
     (vl::parse-module-lambda-list '(a b 5))))


;; ---------- Module definition ----------

(test test-typecheck-module
  "Test we can typecheck a module definition."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(module test (clk
								  &key (p 1))
						     (declare (type bit clk)
						      (direction in clk))

						     (let ((a 1))
						       (setq a 0)))))
		     'vl::module)))


(test test-test-typecheck-module-correctness
  "Test we can identify non-module interface types."
  (is (not (vl::subtype-p (vl::typecheck (vl::expand/vl '(+ 1 2)))
			 'module))))


(test test-module-no-wires
  "Test modules always need a wire."
  (signals (vl::syntax-error)
    (vl::typecheck (vl::expand/vl '(module test ()
				    (let ((a 0))
				      (setq a 1))))))
  (signals (vl::syntax-error)
    (vl::typecheck (vl::expand/vl '(module test (&optional p)
				    (let ((a 0))
				      (setq a 1))))))
  (signals (vl::syntax-error)
    (vl::typecheck (vl::expand/vl '(module test (&key (p 1))
				    (let ((a 0))
				      (setq a 1)))))))


(test test-synthesise-moduletest
  "Test we can syntheise a module with a variety of features."
  (let ((p (vl::expand/vl '(module test (clk a b
					    &key e (f 45))
			   (declare (type bit clk)
			    (type (unsigned-byte 8) a)
			    (type (unsigned-byte 4) b)
			    (direction in clk a b))
			   (let ((x 0)
				 (y 10)
				 (z 44))
			     (declare (as constant z))
			     (vl::@ (vl::posedge clk)
				    (setf x (+ x b) :sync t)))))))

    (vl::typecheck p)
    (is (vl::synthesise p))))


(test test-synthesise-module-late-init
  "Test we can synthesise modules with late initialisation."
  (vl::clear-module-late-initialisation)

  (let ((p (vl::expand/vl '(module test (clk
					    a b
					    &key e (f 45))
			   (declare (type bit clk)
			    (type (unsigned-byte 8) a)
			    (type (unsigned-byte 4) b)
			    (direction in clk a b))
			   (let ((x 0)
				 (a (make-array '(8) :initial-contents (:file "test.hex"))))
			     (@ (posedge clk)
				(setf x (aref a 4))))))))

    (vl::typecheck p)
    (is (vl::synthesise p))

    ;; make sure synthesis cleared the late intialisation queue
    (is (not (vl::module-late-initialisation-p)))))


(test test-synthesise-module-from-defmodule
  "Test we can synthesise directly from a DEFMODULE/VL."
  (vl::clear-global-environment)
  (vl::clear-module-late-initialisation)

  (defmodule/vl test/998 (clk
			     a b
			     &key e (f 45))
    (declare (type bit clk)
	     (type (unsigned-byte 8) a)
	     (type (unsigned-byte 4) b)
	     (direction in clk a b))

    (let ((x 0)
	  (a (make-array '(8) :initial-contents (:file "test.hex"))))
      (@ (posedge clk)
	 (setf x (aref a 4)))))

  (is (vl::synthesise (vl::get-module 'test/998)))

  ;; make sure synthesis cleared the late intiialisation queue
  (is (not (vl::module-late-initialisation-p))))


;; ---------- Module instanciation ----------

(test test-module-instanciate
  "Test we can instanciate a module."
  (vl::clear-global-environment)

  (defmodule/vl clock (clk-in clk-out &optional data)
    (declare (type bit clk-in clk-out data)
	     (direction in clk-in data)
	     (direction out clk-out))
    (setq clk-out clk-in))

  ;; check that the import types correctly
  (let ((p (vl::expand/vl '(let ((clk 0)
				 (clk-in 0))
			    (declare (type bit clk-in clk))
			    (let ((clock (make-instance 'clock :clk-in clk-in
							       :clk-out clk)))
			      clock)))))

    (is (vl::typecheck p)))

  ;; check we can wire the optional arguments too
  (let ((p (vl::expand/vl '(let ((clk 0)
				 (clk-in 0))
			    (declare (type bit clk-in clk))
			    (let ((clock (make-instance 'clock :clk-in clk-in
							       :clk-out clk
							       :data 1)))
			      clock)))))

    (is (vl::typecheck p)))

  ;; check we need to wire all required arguments
  (signals (vl::not-importable)
    (vl::typecheck (vl::expand/vl '(let ((clk 0))
				  (declare (type bit clk))
				  (let ((clock (make-instance 'clock :clk-out clk)))
				    clock)))))

  ;; check we can' provide extra arguments
  (signals (vl::not-importable)
    (vl::typecheck (vl::expand/vl '(let ((clk 0))
				  (declare (type bit clk))
				  (let ((clock (make-instance 'clock :clk-in clk
								     :clk-out clk
								     :test 56)))
				    clock)))))

  ;; check module instanciation where we have to perform several
  ;; passes to get the variables into the body of the module
  (let ((p (vl::expand/vl '(module module-instanciate (clk-in)
			    (declare (type bit clk-in)
			     (direction in clk-in))
			    (let ((clk 0))
			      (let ((clock (make-instance 'clock :clk-in clk-in
								 :clk-out clk)))
				(setq clk 1)))))))

    (vl::typecheck p)
    (setq p (vl::simplify-progn (car (vl::float-let-blocks p))))
    (is (vl::synthesise p))))


(test test-module-instanciate-with-bitfields
  "Test we can instanciate a module that uses bitfields in its wiring."
  (vl::clear-global-environment)

  (defmodule/vl clock (clk_in clk_out)
    (declare (type bit clk_in clk_out)
	     (direction in clk_in)
	     (direction out clk_out))
    (setq clk_out clk_in))

  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(module moduleinstanciatebitfields
						     (clk_in)
						     (declare (type bit clk_in)
						      (direction in clk_in))
						     (let ((ctrl 0))
						       (declare (type (unsigned-byte 4) ctrl))
						       (with-bitfields (clk b2 b1 b0)
							 ctrl
							 (let ((clock (make-instance 'clock :clk_in clk_in
											    :clk_out clk)))
							   clock))))))
		     'vl::module))

  (let ((p (vl::expand/vl '(module moduleinstanciatebitfields
			    (clk_in)
			    (declare (type bit clk_in)
			     (direction in clk_in))
			    (let ((ctrl 0))
			      (with-bitfields (clk b2 b1 b0)
				ctrl
				(let ((clock (make-instance 'clock :clk_in clk_in
								   :clk_out clk)))
				  (setf ctrl 1))))))))

    (vl::typecheck p)
    (setq p (car (vl::float-let-blocks p)))
    (setq p (vl::simplify-progn p))
    (is (vl::synthesise p))))


(test test-synthesise-module-instanciation
  "Test we can synthesise a module instanciation."
  (vl::clear-global-environment)

  (defmodule/vl clock (clk_in clk_out
			  &key (p 1) (q 2))
    (declare (type bit clk_in clk_out)
	     (direction in clk_in)
	     (direction out clk_out))
    (setq clk_out clk_in))

  (let ((p (vl::expand/vl '(let ((c 0)
				(d 0))
			   (let ((a (make-instance 'clock :clk_in c :clk_out d)))
			     (setq c 1))))))

    (vl::typecheck p)
    (is (vl::synthesise p)))

  (let ((p (vl::expand/vl '(let ((c 0)
				(d 0))
			   (let ((a (make-instance 'clock :clk_in c :clk_out d :p 23)))
			     (setq c 1))))))

    (vl::typecheck p)
    (is (vl::synthesise p))))


(test test-module-dependencies
  "Test we can perform dependency checks over modules and instanciations."
  (vl::clear-global-environment)

  (defmodule/vl clock (clk_in clk_out
		       &key (p 1) (q 2))
    (declare (type bit clk_in clk_out)
	     (direction in clk_in)
	     (direction out clk_out))
    (setq clk_out clk_in))

  (let ((p (vl::expand/vl '(let ((a 0)
				 (c 0)
				 (d 0))
			    (let ((m (make-instance 'clock :clk_in c :clk_out d)))
			      (setq c 1))))))
    (vl::typecheck p)
    (setq p (vl::simplify-progn p))

    (let ((decls (elt (elt p 2) 1)))  ; inner LET
      (vl::with-local-frame decls
	(is (set-equal (vl::variable-property 'm 'vl::depends-on)
		       '(c d)))))

    (let ((decls (elt p 1)))  ; outer LET
      (vl::with-local-frame decls
	(is (not (vl::variable-property 'a 'written)))
	(is (not (vl::variable-property 'a 'read)))
	(is (vl::variable-property 'c 'read))
	(is (vl::variable-property 'c 'written))
	(is (vl::variable-property 'd 'read))
	(is (not (vl::variable-property 'd 'written)))))))


;; ---------- Larger and more complicated/contrived examples ----------

(test test-module-real
  "Test module synthesis on a real-ish example."
  (let ((p (vl::expand/vl '(module clockworks (clk-in reset-in
			  clk reset
			  &key (slow 0))
			   (declare (type bit clk-in reset-in clk reset)
			    (direction in clk-in reset-in)
			    (direction out clk reset))

			   ;; clock divider
			   (let ((slow-clk 0))
			     (declare (type (unsigned-byte (1+ slow)) slow-clk))

			     (@ (posedge clk-in)
				(incf slow-clk))
			     (setf clk (bref slow-clk slow)))

			   ;; reset (always active-high)
			   (setq reset reset-in)))))

    (vl::with-new-frame
      (let ((m (vl::elaborate/vl p)))
	(is (vl::synthesise (cadr m)))))))


(test test-module-real-instanciate
  "Test we can instanciate a real module."
  (vl::clear-global-environment)

  (defmodule/vl clockworks (clk-in reset-in
			    clk reset
			    &key (slow 0))
    (declare (type bit clk-in reset-in clk reset)
	     (direction in clk-in reset-in)
	     (direction out clk reset))

    ;; clock divider
    (let ((slow-clk 0))
      (declare (type (unsigned-byte (1+ slow)) slow-clk))

      (@ (posedge clk-in)
	 (incf slow-clk))
      (setf clk (bref slow-clk slow)))

    (setq reset reset-in))

  (defmodule/vl soc (clk-in clk reset)
    (declare (type bit clk-in clk reset)
	     (direction in clk-in)
	     (direction out clk reset))

    (let ((c (make-instance 'clockworks :clk-in clk-in
					:reset-in 0
					:clk clk
					:reset reset
					:slow 9)))
      (setf reset 1)))

  (is (vl::synthesise (vl::get-module 'soc))))


(test test-module-array-size-param
  "Test we can use a parameter to instanciate an array."
  (let ((p (vl::expand/vl '(module memory (clk
					   addr-in data-out
					   &key (size 256))
			    (declare (type bit clk)
			     (type (unsigned-byte 32) addr-in data-out)
			     (direction in clk addr-in)
			     (direction out data-out))

			    (let ((mem (make-array '((>> size 2))
						   :element-type (unsigned-byte 8) )))
			      (@ (posedge clk)
				     (setq data-out (aref mem addr-in))))))))

    (is (vl::subtype-p (vl::typecheck p)
		       'module))
    (is (vl::synthesise p))))


(test test-module-in-error-handler
  "Test we get the correct error-handling behaviour."
  (let ((errors 0)
	(warnings 0))

    (handler-bind ((error (lambda (condition)
			    (format t "~a~%" condition)
			    (incf errors)
			    (recover)))

		   (warning (lambda (condition)
			      (format t "~a~%" condition)
			      (incf warnings)
			      (muffle-warning condition))))

      (vl::with-new-frame
	(vl::declare-variable 'clk '())
	(vl::declare-variable 'addr-in '())
	(vl::declare-variable 'data-out '())
	(vl::declare-variable 'size '())

	(vl::set-variable-property 'clk 'direction 'in)
	(vl::set-variable-property 'addr-in 'direction 'in)
	(vl::set-variable-property 'data-out 'direction 'out)
	(vl::with-recover-on-error
	    t
	  (let ((p (vl::expand/vl '(let ((mem (make-array '(12)
					       :element-type (unsigned-byte 8)))
					 b)
				    ;; b not used (warning)
				    ;; clk not writeable (error)
				    (setq clk (aref mem addr-in))))))

	    (vl::typecheck p)))))

    (is (= errors 1))
    (is (> warnings 0))))
