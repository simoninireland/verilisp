.. _core-iteration:

Iteration
=========

Iteration in Verilisp is built using macros on top of
:ref:`core-tagbody`.


``do``
------

The most basic iteration construct is provided by Lisp's ``do`` macro,
which introduces variables with increment operations applied at each
turn of the iteration, plus a test for exiting the body that can
contain some code to be executed before exit if required.

.. code-block:: lisp

   (do ((i 0 (+1 i))
	(j 10 (1- j)))
     ((= i j) (incf b))

     (incf a))


The variables list may be empty.


``dotimes``
-----------

The ``dotimes`` macro behaves roughly like a simple "for" loop in
other languages:

.. code-block:: lisp

   (let ((sum 0))
     (dotimes (i 10)
       (incf sum i)))

iterates the value of ``i`` from zero to nine inclusive. if the bound
is less than zero then no iterations occur.


``while`` and ``until``
-----------------------

Two common cases of ``do`` are provided as their own structures, which
continue to iterate while (or until) a condition is true.

.. code-block:: lisp

   (while (< j 10)
     (incf j))


.. code-block:: lisp

   (until (>= j 10)
     (incf j))


In both cases the test occurs at the top of the loop, meaning that the
body does not execute at all if the condition is already fulfilled.
