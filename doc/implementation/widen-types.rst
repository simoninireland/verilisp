.. _implementation-widen-types:

Widen types
===========

A widen type lets the type system express the idea of a type that's
"the same" as another but wider. These occur when computing the widths
of addition operations, where the signedness of the final type is
determined by the least upper bound and its width is determined by the
upper-bound and the number of terms that were involved in the
calculation.

For example, in the expression:

.. code:: lisp

   (let ((a 3)
	 (b 14))
     (+ a b))

the "basic" type is ``(unsigned-byte 4)`` (the least upper bound of
the two terms) but the actual assigned type is ``(widen (or
(unsigned-byte 3) (unsigned-byte 4)) 1)`` because there are two terms,
which evaluates to ``(unsigned-byte 5)``, and adding them could push
the value over the width represented by the upper bound.

Widen types are never needed in programs, but are generated
internally by `maths operators <core_operators>`_.
