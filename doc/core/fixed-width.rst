.. _core-fixed-width:

Fixed-width types
=================

Verilisp re-uses the Lisp types as far as possible. The types
``unsigned-byte`` and ``signed-byte`` are used to represent
integers with a fixed bit-width, which may be unsigned or signed.

The type ``unsigned-byte`` contains all unsigned integers; the type
``(unsigned-byte 8)`` contains all 8-bit unsigned integers. The latter
is a sub-type of the former. The type ``unsigned-byte`` is a sub-type
of ``signed-byte``, while the type ``(unsigned-byte 7)`` is a sub-type
of ``(signed-byte 8)`` -- but ``(unsigned-byte 8)``is not.

The type ``bit`` corresponds to the type ``(unsigned-byte 1)``.


Compatibility
-------------

These types are identical to those in Lisp, although they will
typically make use of smaller bounds.
