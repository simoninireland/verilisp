.. _core-assignment:

Assignment
==========

Assignments to variables work exactly as in Lisp. The ``setq`` form
assigns values to variables. The ``setf`` form assigns to "generalised
places" that in Verilisp include variables, single a multiple bits in a
variable, and elements of arrays.


Examples
--------

Simple assignment to a variable:

.. code-block:: lisp

   (let ((a 12))
      (setq a (* a 2))

Assignment to a single bit within a variable:

.. code-block:: lisp

   (let ((a 12))
      (setf (bref a 0) 1))

Assignment to several slices of bits within a variable:

.. code-block:: lisp

   (let ((a 12))
      (declare (type (unsigned-byte 8) a)

      (setf (bref a 2) #2r101)
      (setf (bref a 7 :width 1) #2r1)
      (setf (bref a 2 :end 1) #2r10))

``(setq a 1)`` and ``(setf a 1)`` are equivalent. For the other
generalised places, see under their appropriate access operators.

.. note::

   It is not (yet) possible to define new generalised places within
   Verilisp.

There is also a :ref:`parallel assignment <core-psetq>` version of
``setq``, as well as operators to :ref:`increment/decrement in place
<core-incf-decf>`.
