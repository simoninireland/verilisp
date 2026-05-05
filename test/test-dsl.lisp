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
     (dolist (queue (list vl::*pre-typing-passes*
			  vl::*typing-passes*
			  vl::*post-typing-passes*
			  vl::*synthesis-passes*))
       (vl::clear-pass-queue queue))

     ,@body))


;;; ---------- Pass definition ----------

(test test-dsl-pass
  "Test we can define a pass."
  ;; single passes with different arguments
  (with-no-passes
    (vl::defpass/vl one ())
    (is (member 'one (vl::pass-queue-queue  vl::*post-typing-passes*))))

  (with-no-passes
    (vl::defpass/vl one ()
      "Explicit queue, docstring, no body.")
    (is (member 'one (vl::pass-queue-queue  vl::*post-typing-passes*))))

  (with-no-passes
    (vl::defpass/vl :typing one ())
    (is (null (vl::pass-queue-queue  vl::*post-typing-passes*)))
    (is (member 'one (vl::pass-queue-queue  vl::*typing-passes*))))

  ;; several passes with orderings
  (with-no-passes
    (vl::defpass/vl one ())
    (vl::defpass/vl two ())

    (is (equal (vl::pass-queue-queue  vl::*post-typing-passes*) '(one two))))

  (with-no-passes
    (vl::defpass/vl one ())
    (vl::defpass/vl :prepend two ())

    (is (equal (vl::pass-queue-queue  vl::*post-typing-passes*) '(two one))))

  (with-no-passes
    (vl::defpass/vl one ())
    (vl::defpass/vl :prepend two ())
    (vl::defpass/vl :append three ())

    (is (equal (vl::pass-queue-queue  vl::*post-typing-passes*) '(two one three))))

  (with-no-passes
    (vl::defpass/vl :pre-typing one ())
    (vl::defpass/vl :pre-typing :prepend two ())
    (vl::defpass/vl :pre-typing :append three ())
    (vl::defpass/vl :append four ())

    (is (equal (vl::pass-queue-queue  vl::*pre-typing-passes*) '(two one three)))
    (is (member 'four (vl::pass-queue-queue  vl::*post-typing-passes*)))))


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
