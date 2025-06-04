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
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/vl)


;; ---------- Module definition ----------

(test test-typecheck-module
  "Test we can typecheck a module definition."
  (is (subtypep (vl:typecheck (copy-tree '(vl:module test ((clk :type (unsigned-byte 1) :direction :in)
							   &key (p 1))
					   (let ((a 1))
					     (setq a 0)))))
	     'vl::module-interface)))


(test test-test-typecheck-module-interface-correctness
  "Test we can identify non-module interface types."
  (is (not (subtypep (vl:typecheck (copy-tree '(+ 1 2)))
		     'module-interface))))


(test test-module-no-wires
  "Test modules always need a wire."
  (signals (vl:not-synthesisable)
    (vl:typecheck (copy-tree '(vl:module test (&key (p 1))
			       (let ((a 0))
				 (setq a 1)))))))


(test test-synthesise-moduletest
  "Test we can syntheise a module with a variety of features."
  (let ((p (copy-tree '(vl::module test ((clk :type (unsigned-byte 1) :direction :in)
					 (a   :type (unsigned-byte 8) :direction :in)
					 (b   :type (unsigned-byte 4) :direction :in)
					 &key e (f 45))
			(let ((x 0  :type (unsigned-byte 8))
			      (y 10 :type (unsigned-byte 8))
			      (z 44 :as :constant))
			  (vl::@ (vl::posedge clk)
				 (setf x (+ x b) :sync t)))))))
    (vl:typecheck p)
    (is (vl:synthesise p))))


(test test-synthesise-module-late-init
  "Test we can synthesise modules with late initialisation."
  (vl::clear-module-late-initialisation)

  (let ((p (copy-tree '(vl::module test ((clk :type (unsigned-byte 1) :direction :in)
					 (a   :type (unsigned-byte 8) :direction :in)
					 (b   :type (unsigned-byte 4) :direction :in)
					 &key e (f 45))
			(let ((x 0 :type (unsigned-byte 8))
			      (a (make-array '(8) :initial-contents (:file "test.hex"))))
			  (vl::@ (vl::posedge clk)
				 (setf x (aref a 4))))))))
    (vl:typecheck p)
    (is (vl:synthesise p)))

  ;; make sure synthesis cleared the late intiialisation queue
  (is (not (vl::module-late-initialisation-p))))


;; ---------- Module instanciation ----------

(test test-module-instanciate
  "Test we can instanciate a module."

  (vl:clear-module-registry)

  (vl:defmodule clock ((clk-in  :direction :in  :as :wire :type (unsigned-byte 1))
		       (clk-out :direction :out :as :wire :type (unsigned-byte 1)))
    (setq clk-out clk-in))

  ;; ckeck that the import types correctly
  (let ((p (copy-tree '(let ((clk 0    :type (unsigned-byte 1) :as :wire)
			     (clk-in 0 :type (unsigned-byte 1) :as :wire))
			(let ((clock (make-instance 'clock :clk-in clk-in
							   :clk-out clk)))
			  clock)))))
    (is (subtypep (vl:typecheck p)
		  'vl::module-interface)))

  ;; check we need to wire all arguments
  (signals (vl:not-importable)
    (vl:typecheck '(let ((clk 0 :type (unsigned-byte 1) :as :wire))
		    (let ((clock (make-instance 'clock :clk-out clk)))
		      clock))))

  ;; check wires can be deliberately left unconnected
  ;; TBD

  ;; to check module instanciation we have to perform several
  ;; passes to get the variables into the body of the module
  (let ((p (copy-tree '(vl:module module-instanciate
			((clk-in :type (unsigned-byte 1) :direction :in :as :wire))
			(let ((clk 0 :type (unsigned-byte 1) :as :wire))
			  (let ((clock (make-instance 'clock :clk-in clk-in
							     :clk-out clk)))
			    (setq clk 1)))))))
    (vl:typecheck p)
    (setq p (car (vl:float-let-blocks p)))
    (setq p (vl:simplify-progn (car (vl:float-let-blocks p))))
    (is (vl:synthesise p))))


(test test-module-instanciate-with-bitfields
  "Test we can instanciate a module that uses bitfields in its wiring."

  (vl:clear-module-registry)

  (vl:defmodule clock ((clk_in  :direction :in  :as :wire :type (unsigned-byte 1))
		       (clk_out :direction :out :as :wire :type (unsigned-byte 1)))
    (setq clk_out clk_in))

  (is (subtypep (type-of (vl:typecheck (vl::expand-macros
					(copy-tree '(vl:module moduleinstanciatebitfields
						     ((clk_in :type (unsigned-byte 1) :direction :in :as :wire))
						     (let ((ctrl 0 :type (unsigned-byte 4) :as :wire))
						       (vl:with-bitfields (clk b2 b1 b0)
							   ctrl
							 (let ((clock (make-instance 'clock :clk_in clk_in
											    :clk_out clk)))
							   clock))))))))
		'vl::module-interface))

  (let ((p (copy-tree '(vl:module moduleinstanciatebitfields
			((clk_in :type (unsigned-byte 1) :direction :in :as :wire))
			(let ((ctrl 0 :type (unsigned-byte 4) :as :wire))
			  (vl:with-bitfields (clk b2 b1 b0)
			      ctrl
			    (let ((clock (make-instance 'clock :clk_in clk_in
							       :clk_out clk)))
			      (setf ctrl 1))))))))
    (setq p (vl:expand-macros p))
    (vl:typecheck p)
    (setq p (car (vl:float-let-blocks p)))
    (setq p (vl:simplify-progn p))
    (vl:synthesise p)))


(test test-synthesise-module-instanciation
  "Test we can synthesise a module instanciation."
  (vl:clear-module-registry)

  (vl:defmodule clock ((clk_in  :direction :in  :as :wire :type (unsigned-byte 1))
		       (clk_out :direction :out :as :wire :type (unsigned-byte 1))
		       &key (p 1) (q 2))
    (setq clk_out clk_in))

  (let ((p (copy-tree '(let ((c 0 :as :wire :type (unsigned-byte 1))
			     (d 0 :as :wire :type (unsigned-byte 1)))
			(let ((a (make-instance 'clock :clk_in c :clk_out d)))
			  (setq c 1))))))
    (vl:typecheck p)
    (is (vl:synthesise p)))

  (let ((p (copy-tree '(let ((c 0 :as :wire :type (unsigned-byte 1))
			     (d 0 :as :wire :type (unsigned-byte 1)))
			(let ((a (make-instance 'clock :clk_in c :clk_out d :p 23)))
			  (setq c 1))))))
    (vl:typecheck p)
    (is (vl:synthesise p))))


(test test-module-dependencies
  "Test we can perform dependency checks over modules and instanciations."
  (vl:clear-module-registry)

  (vl:defmodule clock ((clk_in  :direction :in  :as :wire :type (unsigned-byte 1))
		       (clk_out :direction :out :as :wire :type (unsigned-byte 1))
		       &key (p 1) (q 2))
    (setq clk_out clk_in))

  (vl:with-new-frame
    (vl::declare-variable 'c '((:type (unsigned-byte 1))
			       (:initial-value 0)
			       (:as :wire)))
    (vl::declare-variable 'd '((:type (unsigned-byte 1))
			       (:initial-value 0)
			       (:as :wire)))

    ;; this dives into the decl-level functions so that we still
    ;; have access to the environment: if we used a LET block
    ;; it'd get nested
    (vl::typecheck-decl '(a (make-instance 'clock :clk_in c :clk_out d)))
    (vl::dependencies-decl '(a (make-instance 'clock :clk_in c :clk_out d)))
    (is (set-equal (vl::variable-property 'a :dependencies)
		   '(c d)))))


;; ---------- Larger and more complicated/contrived examples ----------

(test test-module-real
  "Test module synthesis on a real-ish example."
  (let ((p (copy-tree '(vl:module clockworks ((clk-in   :type (unsigned-byte 1) :direction :in)
					      (reset-in :type (unsigned-byte 1) :direction :in)
					      (clk      :type (unsigned-byte 1) :direction :out)
					      (reset    :type (unsigned-byte 1) :direction :out)
					      &key (slow 0))

			;; clock divider
			(let ((slow-clk 0 :type (unsigned-byte (1+ slow))))
			  (vl:@ (vl:posedge clk-in)
				(incf slow-clk))
			  (setf clk (vl:bref slow-clk slow)))

			(setq reset reset-in)))))

    (vl:with-new-frame
      (let ((m (vl:elaborate-module p)))
	(is (vl:synthesise (cadr m)))))))


(test test-module-real-instanciate
  "Test we can instanciate a real module."
  (vl:clear-module-registry)

  (vl:defmodule clockworks ((clk-in   :type (unsigned-byte 1) :direction :in)
			    (reset-in :type (unsigned-byte 1) :direction :in)
			    (clk      :type (unsigned-byte 1) :direction :out)
			    (reset    :type (unsigned-byte 1) :direction :out)
			    &key (slow 0))

	     ;; clock divider
	     (let ((slow-clk 0 :type (unsigned-byte (1+ slow))))
	       (vl:@ (vl:posedge clk-in)
		     (incf slow-clk))
	       (setf clk (vl:bref slow-clk slow)))

	     (setq reset reset-in))

  (vl:defmodule soc ((clk-in   :type (unsigned-byte 1) :direction :in)
		     (clk      :type (unsigned-byte 1) :direction :out)
		     (reset    :type (unsigned-byte 1) :direction :out))
    (let ((c (make-instance 'clockworks :clk-in clk-in
					:reset-in 0
					:clk clk
					:reset reset
					:slow 9)))
      (setf reset 1)))

  (is (vl:synthesise (vl::get-module 'soc))))


(test test-module-array-size-param
  "Test we can use a parameter to instanciate an array."
  (let ((p (copy-tree '(vl:module memory ((clk      :width 1  :direction :in)
					  (addr-in  :width 32 :direction :in)
					  (data-out :width 32 :direction :out)
					  &key (size 256))
			(let ((mem (make-array '((vl:>> size 2))
					       :element-type (unsigned-byte 8) )))
			  (vl:@ (vl:posedge clk)
				(setq data-out (aref mem addr-in))))))))

    (is (subtypep (vl:typecheck p)
		  'vl::module-interface))))


(test test-module-array-type-correct
  "Test we can create an array with size given by a parameter."
  (vl:with-new-frame
    (vl::make-module-environment '((clk      :width 1  :direction :in)
				    (addr-in  :width 32 :direction :in)
				    (data-out :width 32 :direction :out)
				    &key (size 256)))
    (let ((p (copy-tree '(let ((mem (make-array '((vl:>> size 2))
				     :element-type (unsigned-byte 8)))
			       b)
			  (setq b (aref mem addr-in))))))

      (is (subtypep (vl:typecheck p)
		    '(unsigned-byte 8)))
      (is (vl:synthesise p)))))


(test test-module-in-error-handler
  "Test we get the correct error-handling behaviour."
  (let ((errors 0)
	(warnings 0))

    (handler-bind ((error (lambda (condition)
			    (incf errors)
			    (invoke-restart 'vl::recover)))

		   (warning (lambda (condition)
			      (format t "~a~%" condition)
			      (incf warnings)
			      (muffle-warning condition))))

      (vl:with-new-frame
	(vl::make-module-environment '((clk      :width 1  :direction :in)
				       (addr-in  :width 32 :direction :in)
				       (data-out :width 32 :direction :out)
				       &key (size 256)))
	(let ((p (copy-tree '(let ((mem (make-array '((vl:>> size 2))
					 :element-type (unsigned-byte 8)))
				   b)
			      (setq b (aref mem addr-in))))))

	  (subtypep (vl:typecheck p)
		    '(unsigned-byte 8))
	  (vl:synthesise p))))

    (is (= errors 0))
    (is (> warnings 0))))
