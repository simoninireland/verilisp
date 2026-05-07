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
     (dolist (queue (list (dsl::get-pass-queue 'firstq)
			  (dsl::get-pass-queue 'secondq)
			  (dsl::get-pass-queue 'thirdq)))
       (dsl::clear-pass-queue queue))

     ,@body))


;;; ---------- Pass definitions and queues ----------

(test test-dsl-pass
  "Test we can define a pass."
  ;; single passes with different arguments, not on a queue
  (with-no-passes
    (defpass one ())
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'firstq))))
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'secondq))))
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'thirdq)))))

  (with-no-passes
    (defpass one ()
      (:documentation "Explicit queue, docstring, no body."))
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'firstq))))
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'secondq))))
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'thirdq)))))

  (with-no-passes
    (defpass one ()
      (:queue firstq))
    (is (member 'one (dsl::pass-queue-queue (dsl::get-pass-queue 'firstq))))
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'secondq))))
    (is (null (dsl::pass-queue-queue (dsl::get-pass-queue 'thirdq)))))

  ;; several passes with queues and orderings
  (with-no-passes
    (defpass one ()
      (:queue firstq))
    (defpass two ()
      (:queue firstq))

    (is (equal (dsl::pass-queue-queue (dsl::get-pass-queue 'firstq)) '(one two))))

  (with-no-passes
    (defpass one ()
      (:queue firstq))
    (defpass two ()
      (:queue firstq)
      (:queue-position :prepend))

    (is (equal (dsl::pass-queue-queue (dsl::get-pass-queue 'firstq)) '(two one))))

  (with-no-passes
    (defpass one ()
      (:queue firstq))
    (defpass two ()
      (:queue firstq)
      (:queue-position :prepend))
    (defpass three ()
      (:queue firstq)
      (:queue-position :append))

    (is (equal (dsl::pass-queue-queue (dsl::get-pass-queue 'firstq)) '(two one three))))

  (with-no-passes
    (defpass one ()
      (:queue firstq))
    (defpass two ()
      (:queue firstq)
      (:queue-position :prepend))
    (defpass three ()
      (:queue firstq)
      (:queue-position :append))
    (defpass four ()
      (:queue secondq)
      (:queue-position :append))

    (is (equal (dsl::pass-queue-queue (dsl::get-pass-queue 'firstq)) '(two one three)))
    (is (member 'four (dsl::pass-queue-queue (dsl::get-pass-queue 'secondq))))))


;;; ---------- Pass methods ----------

(test test-dsl-pass-methods
  "Test we can define pass methods."
  (with-no-passes
    (defpass one ())

    (defpassmethod one ((n integer))
      (+ n 1))
    (is (= (one 5) 6))

    (defpassmethod one (+ a b)
      (+ (one a) (one b)))
    (is (= (one '(+ 3 4)) 9))))


(test test-dsl-pass-methods-from-pass
  "Test we can add methods to the right generic functions from the pass definition."
  (with-no-passes
    (defpass one ()
      (:method ((n integer))
	(+ n 1))
      (:method (+ a b)
	(+ (one a) (one b))))

    (is (= (one 5) 6))
    (is (= (one '(+ 3 4)) 9))))


;;; ---------- Pass recursion schemata ----------


(test test-dsl-schemata
  "Test we can add a default schema to a pass."
  (with-no-passes
    (defpass no-default ())

    (signals unknown-form
      (no-default '(+ 1 2))))

  (with-no-passes
    (defpass into-args ()
      (:schema into-arguments)
      (:method (n)
	(+ n 1)))

    (is (equal (into-args '(+ 1 2)) '(+ 2 3)))))


(test test-dsl-methods-schemata
  "Test we can create methods with different schemata."
  (with-no-passes
    (defpass one ())

    (defpassmethod one ((n integer))
      (+ n 1))

    (defpassmethod one (+ a b)
      (:schema into-arguments))

    (is (equal (one '(+ 1 2)) '(+ 2 3)))))


(test test-dsl-methods-same-as
  "Test we can set one method to be the same as another."
  (with-no-passes
    (defpass one ()
      (:method ((n integer))
	(+ n 1)))

    (defpassmethod one (+ a b)
      (+ (one a) (one b)))

    (defpassmethod one (* a b)
      (:same-as +))

    (is (= (one '(+ 1 (* 2 3))) 9))))
