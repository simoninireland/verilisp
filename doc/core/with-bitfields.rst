.. _core-with-bitfields:

Bitfields
=========

In defining hardware, and especially machine instructions, it is
common to split a single wide register into fixed-width fields. This
is so common that Verilisp provides two "bitfields" macro to perform
the decomposition. These two macros perform :ref:`matching
<core-with-bitfields-matching>` on a pattern against a binary number,
usually binding variables against bits within the number according to
the pattern

.. note::

   See :ref:`core-bits` for unstructured access to bits within a value.


``if-let-bitfields``
--------------------

``if-let-bitfields`` works similarly to IF-LET in that if a
match succeeds it runs a form with variables bound, and if not runs
another form. For example,

.. code-block:: lisp

    (let ((opcode #2r1011010))
      (if-let-bitfields (a a a 1 1 b b b)
	   opcode
	 (progn
	   (setq g (+ a 2))
	   (setq valid 1))

	 (setq valid 0)))

This will check whether ``opcode`` has 1s in bits 3 and 4 and, if so,
will bind ``a`` and ``b`` to two three-bit fields and perform the
action in the ``progn`` form (which makes use of ``a`` in this case).
If ``opcode`` does *not* match the pattern (meaning the fixed 1 bits
aren't present) then matching fails and the program will execute the
other, "else", forms. As with ``IF`` and ``IF-LET`` the "then" arm is
a single form.


``with-bitfields``
------------------

The ``with-bitfields`` macro works analogously, but takes only a
multi-form "then" arm, doing nothing if matching fails.

.. code-block:: lisp

   (let ((opcode #2r1011010))
      (with-bitfields (a a a 1 1 b b b)
	   opcode
	 (setq g (+ a 2))
	 (setq valid 1)))


Assigning to bitfields
----------------------

The variables created by the bitfields macros are generalised places
and so can be assigned to. Assignment will change the corresponding
bits in the value that the bitfields are extracted from. For example
after:

.. code-block:: lisp

   (let ((opcode 0))
      (with-bitfields (a a a a b b b b)
	   opcode
	 (setf a #2r111)
	 (setf b #2r1)))

the value of ``opcode`` will be #2r01110001.

.. warning::

   Because the variables created by the bitfields macros are
   generalised places, use ``setf`` to assign values, not
   ``setq``.


.. _core-with-bitfields-matching:

Matching
--------

A bitfield pattern is simply a list consisting of:

- variable names;
- the values 0, 1, or -; or
- lists consisting of one of these followed by a length.

Matching occurs from the right, in the sense that the rightmost
element of the pattern matches the lowest-order bit.

Variables match against the corresponding bits, which must be
adjacent. The same variable cannot appear in non-adjacent positions.

A value of - matches any bit, essentially ignoring the bit in that
position. A 0 or 1 matches only that bit in that position.

A list such as ``(a 3)`` is equivalent to a pattern ``a a a``: a
pattern of length 3, bound to ``a``. Similarly, a pattern ``(1 3)``
will match three adjacent 1 bits, and ``(- 3)`` will ignore three
bits.

.. note::

   The lengths appearing in list sub-patterns must be statically
   known.
