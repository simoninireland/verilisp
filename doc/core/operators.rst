.. _core-operators:

Operators
=========

Verilisp includes a wide range of mathematical and logical operators.
In general these are the same as those in Common Lisp, and behave
identically.


Maths operators
---------------

+---------------+------------------------------+--------------------+
| Operator      | Example                      | Notes              |
+===============+==============================+====================+
| ``+``         + ``(+ 1 2 a)``                +                    |
+---------------+------------------------------+--------------------+
| ``-``         + ``(- 1 2 a)`` or ``(- 3)``   + Unary or n-ary     |
+---------------+------------------------------+--------------------+
| ``*``         + ``(* 1 2 a)``                +                    |
+---------------+------------------------------+--------------------+
| ``mod``       + ``(mod 15 4)``               +                    |
+---------------+------------------------------+--------------------+
| ``rem``       + ``(rem 12 a)``               +                    |
+---------------+------------------------------+--------------------+


Bitwise operators
-----------------

+---------------+------------------------------+
| Operator      | Example                      |
+===============+==============================+
| ``logand``    + ``(logand a #16rFF)``        |
+---------------+------------------------------+
| ``logior``    + ``(logior a b c)``           |
+---------------+------------------------------+
| ``logxor``    + ``(logxor a #16rFF)``        |
+---------------+------------------------------+
| ``lognot``    + ``(lognot a)``               |
+---------------+------------------------------+

There is also an additional operator, not needed in Common Lisp, that
converts a value that is true when non-zero.

+----------------+----------------------------+
| Operator       | Example                    |
+================+============================+
| ``asserted-p`` + ``(asserted-p v)``         |
+----------------+----------------------------+


Bitwise shift operators
-----------------------

Bitwise shifts in Common Lisp use the ``ash`` ("arithmetic shift")
function which shifts left or right depending on the sign of its
second argument. We replace this with two explicit operators, left and
right shifts, that take exactly two arguments.

+---------------+------------------------------+--------------------+
| Operator      | Example                      | Lisp equivalent    |
+===============+==============================+====================+
| ``<<``        + ``(<< a 5)``                 + ``(ash a 5)``      |
+---------------+------------------------------+--------------------+
| ``>>``        + ``(>> b 5)``                 + ``(ash b -5)``     |
+---------------+------------------------------+--------------------+

Left-shifting behaves the same for all fixed-width values, but
right-shifting a value is affected by whether the value is signed or
not. For example, after

.. code:: lisp

   (let (a b)
      (declare (type (unsigned-byte 8) a b))

      (setq a 8)
      (setq b (a >> 2)))

``b`` has value 2 (8 shifted right two places), but after

.. code:: lisp

   (let (a b)
      (declare (type (signed-byte 8) a b))

      (setq a -8)
      (setq b (a >> 2)))

``b`` has value -2: the sign is preserved across the shift.

.. note::

   This behaviour is the same as ``ash`` in Common Lisp, but is
   *different* to the behaviour of ``>>`` in Verilog, which performs a
   logical shift right that does not preserve sign. Sign-preservation
   in Verilog requires the use of a different operator, ``>>>``, which
   Verilisp does not need.

   If you for some reason want to logically right-shift a value whose
   type is ``signed-byte`` you should cast it before shifting:

   .. code:: lisp

   (let (a b)
      (declare (type (signed-byte 8) a)
	       (type (unsigned-byte 8) b))

      (setq a -8)
      (setq b ((the '(unsigned-byte 8) a) >> 2)))


Logical and comparison operators
---------------------------------

+---------------+------------------------------+
| Operator      | Example                      |
+===============+==============================+
| ``and``       + ``(and (> 3 a) (< 3 b))``    |
+---------------+------------------------------+
| ``or``        + ``(or (> 3 a) (< 3 b))``     |
+---------------+------------------------------+
| ``not``       + ``(not (> 3 a))``            |
+---------------+------------------------------+
| ``=``         + ``(= a 12)``                 |
+---------------+------------------------------+
| ``/=``        + ``(/= a 12)``                |
+---------------+------------------------------+
| ``>``         + ``(> a 10)``                 |
+---------------+------------------------------+
| ``>=``        + ``(>= a 10)``                |
+---------------+------------------------------+
| ``<``         + ``(< a 10)``                 |
+---------------+------------------------------+
| ``<=``        + ``(<= a 10)``                |
+---------------+------------------------------+


Additional operators
--------------------

There are some other :ref:`shortcut operators <core-maths-shortcuts>`
provided as macros. See also :ref:`core-bits`.
