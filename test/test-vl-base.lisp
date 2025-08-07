;; Tests of basic utilities of RTLisp
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


(defparameter emptyenv (vl::empty-environment))


;; ---------- Frames ----------

(test test-declare-variables
  "Test we can declare variables in a frame."
  (let ((env (vl::empty-environment)))
    (is (vl::variable-declared-in-environment-p 'a (vl::declare-environment-variable 'a '((:a 1) (:b 2)) env)))
    (is (vl::variable-declared-in-environment-p 'b (vl::declare-environment-variable 'b '((:a 6) (:b 4)) env)))

    ;; can't declare duplicates in the same frame
    (signals (vl::duplicate-variable)
      (vl::declare-environment-variable 'b '((:a 6) (:b 4)) env))

    ;; names
    (is (set-equal (vl::get-frame-names env)
		   '(b a)))
    (is (vl::variable-declared-in-frame-p 'a env))
    (is (not (vl::variable-declared-in-frame-p 'c env)))))


(test test-get-properties
  "Test we can retrieve properties from a frame."
  (let ((env (vl::empty-environment)))
    (vl::declare-environment-variable 'a '((:a 1) (:b 2)) env)
    (is (vl::get-frame-properties 'a env)
	'((:a 1) (:b 2)))

    (signals (vl::unknown-variable)
      (vl::get-frame-properties 'b env))))


(test test-get-property
  "Test we can retrieve a property."
  (let ((env (vl::empty-environment)))
    (vl::declare-environment-variable 'a '((:a 1) (:b 2)) env)
    (is (equal (vl::get-frame-property 'a :b env) 2))

    (is (null (vl::get-frame-property 'a :c env)))

    (vl::declare-environment-variable 'c '((:a 1) (:b 2)) env)
    (is (eql (vl::get-frame-property 'c :c env :default 'ttt)
	     'ttt))))


(test test-frame-declaring
  "Test we can find the frame declaring a variable."
  (let ((env1 (vl::empty-environment)))
    (vl::declare-environment-variable 'a '((:a 1) (:b 2)) env1)
    (vl::declare-environment-variable 'b '((:a 1) (:b 2)) env1)

    (let ((env2 (vl::add-frame env1)))
      (vl::declare-environment-variable 'b '((:a 1) (:b 2)) env2)

      (equal (vl::get-frame-declaring 'b env2) env2)
      (equal (vl::get-frame-declaring 'a env2) env1)
      (equal (vl::get-frame-declaring 'b env1) env1)

      (is (null (vl::get-frame-declaring 'c env2))))))


(test test-set-property
  "Test we can set frame properties."
  (let ((env (vl::empty-environment)))
    (vl::declare-environment-variable 'a '((:a 1) (:b 2)) env)
    (vl::declare-environment-variable 'b '((:a 1) (:b 2)) env) ; maximise sharing danger

    ;; update a property
    (vl::set-frame-property 'a :a 12 env)
    (is (equal (vl::get-frame-property 'a :a env) 12))
    (is (equal (vl::get-frame-property 'a :b env) 2))
    (is (equal (vl::get-frame-property 'b :a env) 1))

    ;; add a property
    (is (null (vl::get-frame-property 'a :c env)))
    (vl::set-frame-property 'a :c 99 env)
    (is (equal (vl::get-frame-property 'a :c env) 99))
    (is (null (vl::get-frame-property 'b :c env)))))


(test test-forget-frame
  "Test we can forget a variable in a frame."
  (let ((env (vl::empty-environment)))
    (vl::declare-environment-variable 'a '((:a 1) (:b 2)) env)
    (vl::declare-environment-variable 'b '((:a 13) (:b 24)) env)
    (vl::declare-environment-variable 'c '((:a 3) (:b 4)) env)

    (is (set-equal (vl::get-frame-names env)
		   '(a b c)))

    (vl::forget-frame-variable 'b env)
    (is (set-equal (vl::get-frame-names env)
		   '(a c)))

    (vl::forget-frame-variable 'c env)
    (is (set-equal (vl::get-frame-names env)
		   '(a)))

    (vl::forget-frame-variable 'a env)
    (is (null (vl::get-frame-names env)))))


;; ---------- Environments ----------

(test test-names
  "Test we can extract names from an environment."
  ;; empty environment has no names
  (is (null (vl::get-environment-names emptyenv)))

  (let ((env1 (vl::add-frame emptyenv)))
    (vl::declare-environment-variable 'a '((:a 1 :b 2)) env1)
    (is (set-equal (vl::get-environment-names env1)
		   '(a)))

    ;; nested frame
    (let ((env2 (vl::add-frame env1)))
      (vl::declare-environment-variable 'b '((:a 1 :b 2)) env2)
      (vl::declare-environment-variable 'c '((:a 1 :b 2)) env2)
      (is (set-equal (vl::get-environment-names env2)
		     '(a b c)))

      ;; nested again
      (let ((env3 (vl::add-frame env2)))
	(vl::declare-environment-variable 'b '((:a 4 :b 5)) env3)
	(is (set-equal (vl::get-environment-names env3)
		       '(a b c)))

	;; local (frame) property is retrieved correctly
	(is (equal (vl::get-frame-property 'b :a env3) 4))

	;; global (environment) property is the same
	(is (equal (vl::get-environment-property 'b :a env3) 4))

	;; shallower frame is undisturbed
	(is (equal (vl::get-frame-property 'b :a env2) 1))

	;; finding variables works as expected
	(is (not (vl::variable-declared-in-frame-p 'a env3)))
	(is (vl::variable-declared-in-environment-p 'a env3))

	;; variable can't be found from topmost frame
	(signals (vl::unknown-variable)
	  (vl::get-frame-properties 'a env3))

	;; ...but is found correctly globally
	(is (equal (vl::get-environment-property 'a :a env3) 1))))))


(test test-names-empty-env
  "Test that an empty environment has no names."
  (is (null (vl::get-environment-names (vl::empty-environment)))))


(test test-identifiers
  "Test legal and illegal identifiers."

  ;; legal identifiers
  (dolist (s '("a" "abc" "abc1" "a1Bc" "_a" "a_" "_123" "_camelCase" "_reg"))
    (is (null (vl::make-legal-identifier s)))

    ;; this is really just a synonym, but check it anyway
    (is (vl::legal-identifier-p s)))

  ;; illegal idenfiers (bad characters or arrangements)
  (dolist (from-to '(("0abc" "_0abc")
		     ("01" "_01")
		     ("test-me" "test_me")
		     ("1test-me" "_1test_me")))
    (destructuring-bind (from to)
	from-to
      (is (string-equal (vl::make-legal-identifier from) to))))

  ;; illegal identifiers (keywords)
  (dolist (from-to '(("reg" "_reg")
		     ("inout" "_inout")))
    (destructuring-bind (from to)
	from-to
      (is (string-equal (vl::make-legal-identifier from) to))))

  ;; legalisation of legal and illegal identifiers
  (dolist (s '("a" "abc" "abc1" "a1Bc" "_a" "a_" "_123" "_camelCase" "_reg"))
    (string-equal s (vl::ensure-legal-identifier s)))
  (dolist (from-to '(("0abc" "_0abc")
		     ("01" "_01")
		     ("test-me" "test_me")
		     ("1test-me" "_1test_me")))
    (destructuring-bind (from to)
	from-to
      (is (string-equal (vl::ensure-legal-identifier from) to))))

  ;; check signalling of really bad variable name choice
  (signals (vl::not-synthesisable)
    (vl::make-legal-identifier "_")))


(test test-test-filter-frame
  "Test we can filter a single frame."
  (flet ((filter-by-f (n env)
	   (if-let ((prop (vl::get-environment-property n :f env)))
	     (> prop 20))))

    (let ((env1 (vl::make-frame)))
      (vl::declare-environment-variable 'c '((:f 5)) env1)

      (let ((env2 (vl::add-frame env1)))
	(vl::declare-environment-variable 'a '((:f 10)) env2)
	(vl::declare-environment-variable 'b '((:f 30)) env2)

	(is (set-equal (vl::get-frame-names (vl::filter-frame #'filter-by-f env2))
		       '(b)))))))


(test test-filter-env
  "Test we can filter environments."
  (flet ((filter-by-f (n env)
	   (if-let ((prop (vl::get-environment-property n :f env)))
	     (> prop 20))))

    (let ((env1 (vl::empty-environment)))
      (mapc (lambda (decl)
	      (vl::declare-environment-variable (car decl) (cadr decl) env1))
	    '((a ((:f 4)))
	      (b ((:f 23) (:g 34)))
	      (c ((:f 12)))))

      (let ((env2 (vl::add-frame env1)))
	(mapc (lambda (decl)
		(vl::declare-environment-variable (car decl) (cadr decl) env2))
	      '((a ((:f 28)))
		(d ((:f 99)))))

	;; by name
	(let ((kept-names (vl::filter-environment (lambda (n env)
						     (equal n 'a))
						   env2)))
	  (is (set-equal (vl::get-environment-names kept-names)
			 '(a))))

	;; by property
	(let ((kept-props (vl::filter-environment #'filter-by-f
						   env2)))
	  (is (set-equal (vl::get-environment-names kept-props)
			 '(a d b))))))))


(test test-map-env
  "Test we can map across environments."
  (let ((env1 (vl::add-frame (vl::empty-environment))))
    (mapc (lambda (decl)
	    (vl::declare-environment-variable (car decl) (cadr decl) env1))
	  '((a ((:g 4)))
	    (b ((:f 23) (:g 34)))
	    (c ((:f 12)))))

    (let ((env2 (vl::add-frame env1)))
      (mapc (lambda (decl)
	      (vl::declare-environment-variable (car decl) (cadr decl) env2))
	    '((a ((:f 28)))
	      (d ((:g 99)))))

      (is (equal (vl::map-environment (lambda (n env)
					 (or (vl::get-environment-property n :f env)
					     0))
				       env2)
		 '(0 23 12 28 0))))))


;; ---------- Property access and update ----------

(test test-env-get-property
  "Test we can get a property."
  (let ((env1 (vl::add-frame emptyenv)))
    (vl::declare-environment-variable 'a '((:type (unsigned-byte 8))
					    (:width 8))
				       env1)
    (is (equal (vl::get-frame-property 'a :type env1)
	       '(unsigned-byte 8)))

    (let ((env2 (vl::add-frame env1)))
      (vl::declare-environment-variable 'a '((:type (unsigned-byte 16))
					     (:width 16))
					env2)
      (is (equal (vl::get-frame-property 'a :type env2)
		 '(unsigned-byte 16)))

      ;; underlying type is unchanged
      (is (equal (vl::get-frame-property 'a :type env1)
		 '(unsigned-byte 8))))))


(test test-env-set-property-frame
  "Test we can set a property in the shallowest frame."
  (let ((env1 (vl::add-frame emptyenv)))
    (vl::declare-environment-variable 'a '((:type (unsigned-byte 8)))
				       env1)
    (vl::declare-environment-variable 'b '((:type (unsigned-byte 12)))
				       env1)
    (vl::set-frame-property 'a :type '(unsigned-byte 12) env1)
    (is (equal (vl::get-frame-property 'a :type env1)
	       '(unsigned-byte 12)))

    (let ((env2 (vl::add-frame env1)))
      (vl::declare-environment-variable 'a '((:type (unsigned-byte 16)))
					 env2)

      (vl::set-frame-property 'a :type '(unsigned-byte 32) env2)
      (is (equal (vl::get-frame-property 'a :type env2)
		 '(unsigned-byte 32)))

      ;; deeper declaration is unaffected
      (is (equal (vl::get-frame-property 'a :type env1)
		 '(unsigned-byte 12))))))


(test test-env-set-property-env
  "Test we can set a property in a deeper frame."
  (let ((env1 (vl::add-frame emptyenv)))
    (vl::declare-environment-variable 'a '((:type (unsigned-byte 8)))
				       env1)
    (vl::declare-environment-variable 'b '((:type (unsigned-byte 12)))
				       env1)

    (let ((env2 (vl::add-frame env1)))
      (vl::declare-environment-variable 'a '((:type (unsigned-byte 16)))
					 env2)

      (vl::set-environment-property 'b :type '(unsigned-byte 32) env2)
      (is (equal (vl::get-environment-property 'b :type env2)
		 '(unsigned-byte 32)))

      ;; same declaration, not a new one
      (is (equal (vl::get-frame-property 'b :type env1)
		 '(unsigned-byte 32))))))


;; ---------- Ordering ----------

(test test-test-env-ordered
  "Test that declarations land in a frame in the same order as they're declared."
  (let ((env (vl::make-frame)))
    (vl::declare-environment-variable 'a '((:type (unsigned-byte 8))) env)
    (vl::declare-environment-variable 'b '((:type (unsigned-byte 8))) env)
    (vl::declare-environment-variable 'c '((:type (unsigned-byte 8))) env)
    (is (equal (vl::get-frame-names env) '(a b c)))))
