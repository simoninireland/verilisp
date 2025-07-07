.. _core-with-bitfields:

Bitfields
=========

In defining hardware, and especially machine instructions, it is
common to split a single wide register into fixed-width fields. This
is so common that Verilisp provides a macro to perform the
decomposition.


``with-bitfields``
------------------

The ``with-bitfields`` macro takes a value and extracts fields from it
into variables.

.. code-block:: lisp

   (let ((opcode #2r1011010))
      (with-bitfields (a a a - - b b b)
	   opcode
	 (setf a #2r000)))

This code takes a variable ``opcoode`` and defines two variables ``a``
and ``b``, with ``a`` receiving bits 7 to 5 and ``b`` receiving the
lowest-order 3 bits; the two bits 3 and 4 are ignored. ``a`` and ``b``
are then available in the rest of the body forms.


Matching
--------

A bitfield pattern is simply a list consisting of variable names or
the values 0, 1, or -.

Matching occurs from the right, in the sense that the rightmost
element of the pattern matches the lowest-order bit.

Variables match against the corresponding bits, which must be
adjacent.

A value of - matches any bit, essentially ignoring the bit in that
position. A 0 or 1 matches only that bit in that position. If the bit
in that position is not as expected, the match fails and the body
forms are not executed.

.. note::

   Fixed-bit matching doesn't work yet.


Assigning to bitfields
----------------------

The variables created by ``with-bitfields`` are generalised places and
so can be assigned to. Assigning will change the corresponding bits in
the value that the bitfields are extracted from. For example after:

.. code-block:: lisp

   (let ((opcode 0))
      (with-bitfields (a a a a b b b b)
	   opcode
	 (setf a #2r111)
	 (setf b #2r1)))

the value of ``opcode`` will be #2r01110001.

.. warning::

   The variables created by ``with-bitfields`` are generalised places,
   so use ``setf`` to assign values to bitfields, not ``setq``.
