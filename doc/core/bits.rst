.. _core-bits:

Bitwise access to data
======================

It is common in hardware to want to access bits within a variable.
Verilisp provides a single operator, ``bref``, for this purpose. By
analogy to ``aref`` which accesses :ref:`array elements
<core-arrays-element-access>`, ``bref`` allows individual bits or
sequences of bits to be extracted and set.

.. note::

   For more structured access to bitfields within a variable see
   :ref:`core-with-bitfields`.


Access
------

``bref`` takes two required and two keyword arguments, which we will
explain through their usage.

Suppose we have a variable ``a`` of type ``(unsigned-byte 16)``. We
can extract the least-significant bit from this variable with the
expression:

.. code:: lisp

   (bref a 0)

and the most-significant bit with:

.. code:: lisp

   (bref a 15)

Bits are numbered from the right, starting from zero. The type of both
these expressions is ``bit``.

To extract the three lowest-order bits we can use:

.. code:: lisp

   (bref a 2 :end 0)

which will extract bits two down to zero and have type
``(unsigned-byte 3)``. Alternatively we could use the expression:

.. code:: lisp

   (bref a 2 :width 3)

which has the same effect, extracting three bits starting at bit two.
At most one of the ``:end`` and ``:width`` keywords can appear in one
``bref`` expression: expressions with neither keyword extract a single
bit.

.. note::

   The values returned by ``bref`` always have type ``unsigned-byte``.
   To extract bits that should be treated as signed, :ref:`cast or
   coerce <core-coercion>` the result as required -- for example,

   .. code:: lisp

      (the '(signed-byte 8) (bref opcode 13 :width 8))


Assignment
----------

All these forms of ``bref`` form generalised places and can be
assigned to using ``setf`` (see :ref:`core-assignment`).


Restrictions
------------

There are some restrictions on the arguments that can be passed to
``bref`` which arise from the need to generate wires at the hardware
level. Essentially the compiler must be able to determine how many,
and which, bits are being accessed. Verilisp therefore imposes the
following restrictions:

1. If there are no ``:end`` or ``:width`` arguments, the expression
   always returns a single bit and the start bit may be a constant or
   an expression
2. If there is either a ``:end`` or a ``:width`` argument, both the
   start bit and the argument must be constants.


Compatibility
-------------

``bref`` is not an operator in Common Lisp. It can be implemented
using a combination of ``logand`` and ``ash``.
