.. _core-iteration:

Iteration
=========

Iteration in Verilisp is built using macros on top of
:ref:`core-tagbody`.


``do``
------

The most basic iteration construct is provided by Lisp's ``do`` macro,
which introduces variables with increment operations applied at each
turn of the iteration, plus a test for exiting the body.

.. code-block:: lisp

   (do ((i 0 (+1 i))
	(j 10 (1- j)))
     ((= i j) return)

     (incf a))


The variables list may be empty.


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
