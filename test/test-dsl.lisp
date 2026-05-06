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


(defmacro with-no-passes (&body body)
  "Clear all the pass queues before running BODY."
  `(progn
     (dolist (queue (list (vl::get-pass-queue 'pre-typing)
			  (vl::get-pass-queue 'typing)
			  (vl::get-pass-queue 'post-typing)
			  (vl::get-pass-queue 'synthesis)))
       (vl::clear-pass-queue queue))

     ,@body))


;;; ---------- Pass definition ----------

(test test-dsl-pass
  "Test we can define a pass."
  ;; single passes with different arguments
  (with-no-passes
    (vl::defpass/vl one ())
    (is (member 'one (vl::pass-queue-queue (vl::get-pass-queue 'post-typing)))))

  (with-no-passes
    (vl::defpass/vl one ()
      (:documentation "Explicit queue, docstring, no body."))
    (is (member 'one (vl::pass-queue-queue (vl::get-pass-queue 'post-typing)))))

  (with-no-passes
    (vl::defpass/vl one ()
      (:queue typing))
    (is (null (vl::pass-queue-queue (vl::get-pass-queue 'post-typing))))
    (is (member 'one (vl::pass-queue-queue (vl::get-pass-queue 'typing)))))

  ;; several passes with queues and orderings
  (with-no-passes
    (vl::defpass/vl one ())
    (vl::defpass/vl two ())

    (is (equal (vl::pass-queue-queue (vl::get-pass-queue 'post-typing)) '(one two))))

  (with-no-passes
    (vl::defpass/vl one ())
    (vl::defpass/vl two ()
      (:queue-position :prepend))

    (is (equal (vl::pass-queue-queue (vl::get-pass-queue 'post-typing)) '(two one))))

  (with-no-passes
    (vl::defpass/vl one ())
    (vl::defpass/vl two ()
      (:queue-position :prepend))
    (vl::defpass/vl three ()
      (:queue-position :append))

    (is (equal (vl::pass-queue-queue (vl::get-pass-queue 'post-typing)) '(two one three))))

  (with-no-passes
    (vl::defpass/vl one ()
      (:queue pre-typing))
    (vl::defpass/vl two ()
      (:queue pre-typing)
      (:queue-position :prepend))
    (vl::defpass/vl three ()
      (:queue pre-typing)
      (:queue-position :append))
    (vl::defpass/vl four ()
      (:queue-position :append))

    (is (equal (vl::pass-queue-queue (vl::get-pass-queue 'pre-typing)) '(two one three)))
    (is (member 'four (vl::pass-queue-queue (vl::get-pass-queue 'post-typing))))))


(test test-dsl-pass-methods
  "Test we can define pass methods."
  (with-no-passes
    (vl::defpass/vl one ())

    (vl::defpassmethod/vl one ((n integer))
      (+ n 1))
    (is (= (one 5) 6))

    (vl::defpassmethod/vl one (+ a b)
      (+ (one a) (one b)))
    (is (= (one '(+ 3 4)) 9))))


(test test-dsl-pass-methods-from-pass
  "Test we can add methods to the right generic functions from the pass definition."
  (with-no-passes
    (vl::defpass/vl one ()
      (:method ((n integer))
	(+ n 1))
      (:method (+ a b)
	(+ (one a) (one b))))

    (is (= (one 5) 6))
    (is (= (one '(+ 3 4)) 9))))


(test test-dsl-schemata
  "Test we can add a default schema to a pass."
  (with-no-passes
    (vl::defpass/vl no-default ())

    (signals unknown-form
      (no-default '(+ 1 2))))

  (with-no-passes
    (vl::defpass/vl into-args ()
      (:schema vl:into-arguments)
      (:method (n)
	(+ n 1)))

    (is (equal (into-args '(+ 1 2)) '(+ 2 3)))))


(test test-dsl-methods-schemata
  "Test we can create methods with different schemata."
  (with-no-passes
    (vl::defpass/vl one ())

    (vl::defpassmethod/vl one ((n integer))
      (+ n 1))

    (vl::defpassmethod/vl one (+ a b)
      (:schema into-arguments))

    (is (equal (one '(+ 1 2)) '(+ 2 3)))))
