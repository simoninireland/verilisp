;;;; Tests of DSL builder macros
;;;;
;;;; Copyright (C) 2023--2026 Simon Dobson
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

(in-package :verilisp/test)
(in-suite verilisp/dsl)


;;; ---------- Private passes to prevent interference ----------

(define-pass-queue firstq)
(define-pass-queue secondq)
(define-pass-queue thirdq)

(defmacro with-no-passes (&body body)
  "Clear all the pass queues before running BODY."
  `(progn
     (dolist (queue '(firstq
		      secondq
		      thirdq))
       (dsl::clear-pass-queue queue))

     ,@body))


;;; ---------- Pass definitions and queues ----------

(test test-dsl-pass
  "Test we can define a pass."
  ;; single passes with different arguments, not on a queue
  (with-no-passes
    (defpass one (form))
    (is (null (dsl::get-pass-queue 'firstq)))
    (is (null (dsl::get-pass-queue 'secondq)))
    (is (null (dsl::get-pass-queue 'thirdq))))

  (with-no-passes
    (defpass one (form)
      (:documentation "Explicit queue, docstring, no body."))
    (is (null (dsl::get-pass-queue 'firstq)))
    (is (null (dsl::get-pass-queue 'secondq)))
    (is (null (dsl::get-pass-queue 'thirdq))))

  (with-no-passes
    (defpass one (form)
      (:queue firstq))
    (is (dsl::pass-on-pass-queue-p 'one 'firstq))
    (is (null (dsl::get-pass-queue 'secondq)))
    (is (null (dsl::get-pass-queue 'thirdq))))

  ;; several passes with queues and orderings
  (with-no-passes
    (defpass one (form)
      (:queue firstq))
    (defpass two (form)
      (:queue firstq))

    (is (equal (dsl::get-pass-queue 'firstq)
	       (mapcar #'dsl::pass-wrap-level-function-name '(one two)))))

  (with-no-passes
    (defpass one (form)
      (:queue firstq))
    (defpass two (form)
      (:queue firstq)
      (:queue-position :prepend))

    (is (equal (dsl::get-pass-queue 'firstq)
	       (mapcar #'dsl::pass-wrap-level-function-name '(two one)))))

  (with-no-passes
    (defpass one (form)
      (:queue firstq))
    (defpass two (form)
      (:queue firstq)
      (:queue-position :prepend))
    (defpass three (form)
      (:queue firstq)
      (:queue-position :append))

    (is (equal (dsl::get-pass-queue 'firstq)
	       (mapcar #'dsl::pass-wrap-level-function-name '(two one three)))))

  (with-no-passes
    (defpass one (form)
      (:queue firstq))
    (defpass two (form)
      (:queue firstq)
      (:queue-position :prepend))
    (defpass three (form)
      (:queue firstq)
      (:queue-position :append))
    (defpass four (form)
      (:queue secondq)
      (:queue-position :append))

    (is (equal (dsl::get-pass-queue 'firstq)
	       (mapcar #'dsl::pass-wrap-level-function-name '(two one three))))
    (is (equal (dsl::get-pass-queue 'secondq)
	       (mapcar #'dsl::pass-wrap-level-function-name '(four))))))


(test test-dsl-pass-pre-post
  "Check we can add pre- and post-processing code to a pass."
  (let (a)

    (with-no-passes
      (defpass one (form)
	(:queue firstq)
	(:pre (lambda (form) (setf a t) form)))
      (defpassmethod one ((n integer))
	(+ n 1))

      (is (null a))
      (is (= (one 5) 6))
      (is (null a))
      (is (= (dsl::run-pass-queue 'firstq '5) 6))
      (is (eql a t))))

  (let (a b)

    (with-no-passes
      (defpass one (form)
	(:queue firstq)
	(:pre (lambda (form) (setf a 1) form))
	(:post (lambda (form result) (setf b 2))))
      (defpassmethod one ((n integer))
	(+ n 1))

      (is (null a))
      (is (null b))
      (is (= (one 5) 6))
      (is (null a))
      (is (null b))
      (is (= (dsl::run-pass-queue 'firstq '5) 2)) ; value of post
      (is (eql a 1))
      (is (eql b 2)))))


;;; ---------- Pass methods ----------

(test test-dsl-pass-methods
  "Test we can define pass methods."
  (with-no-passes
    (defpass one (form))

    (defpassmethod one ((n integer))
      (+ n 1))
    (is (= (one 5) 6))

    (defpassmethod one (+ a b)
      (+ (one a) (one b)))
    (is (= (one '(+ 3 4)) 9))))


(test test-dsl-pass-methods-from-pass
  "Test we can add methods to the right generic functions from the pass definition."
  (with-no-passes
    (defpass one (form)
      (:passmethod ((n integer))
	(+ n 1))
      (:passmethod (+ a b)
	(+ (one a) (one b))))

    (is (= (one 5) 6))
    (is (= (one '(+ 3 4)) 9))))


;;; ---------- Pass recursion schemata ----------

(test test-dsl-schemata
  "Test we can add a default schema to a pass."
  (with-no-passes
    (defpass no-default (form))

    (signals unknown-form
      (no-default '(+ 1 2))))

  (with-no-passes
    (defpass into-args (form)
      (:schema into-arguments)
      (:passmethod (n)
	(+ n 1)))

    (is (equal (into-args '(+ 1 2)) '(+ 2 3))))

  (with-no-passes
    (defpass with-default (form)
      (:schema constant-form 5))

    (is (equal (with-default '(+ 1 2)) 5))))


(test test-dsl-methods-schemata
  "Test we can create methods with different schemata."
  (with-no-passes
    (defpass one (form))

    (defpassmethod one ((n integer))
      (+ n 1))

    (defpassmethod one (+ a b)
      (:schema into-arguments))

    (is (equal (one '(+ 1 2)) '(+ 2 3)))))


(test test-dsl-methods-same-as
  "Test we can set one method to be the same as another."
  (with-no-passes
    (defpass one (form)
      (:passmethod ((n integer))
	(+ n 1)))

    (defpassmethod one (+ a b)
      (+ (one a) (one b)))

    (defpassmethod one (* a b)
      (:same-as +))

    (is (= (one '(+ 1 (* 2 3))) 9))))


(test test-dsl-extra-args
  "Test we can construct a pass with extra arguments."
  (with-no-passes
    (defpass extraargs (form arg1 arg2)
      (:schema into-arguments)
      (:passmethod (+ a b)
	(+ a b arg1 arg2)))

    (defpassmethod extraargs (* a b)
      (+ a b arg1 arg2))

    (is (= (extraargs '(+ 1 2) 10 20) (+ 1 2 10 20)))
    (is (= (extraargs '(* 1 2) 10 20) (+ 1 2 10 20)))))
