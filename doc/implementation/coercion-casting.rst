.. _implementation-coercion:

Coercion and casting
====================

These two terms are often used interchangeably, but they actually
denote two subtly different operations.


Coercion
--------

A *coercion* of a value forces it to a particular type. Critically,
this may involve changing the representation of the value while
maintaining its meaning in some sense.

The ``coerce`` form is used to coerce a value. For example,

.. code-block:: lisp

   (let ((a (coerce 12 '(signed-byte 32))))
     ...)

coerces the value 12 to the type ``signed-byte 32``, a signed 32-bit
word. This involves changing the representation of 12, which would by
default need 4 bits: the value of the ``coerce`` form will have 32
bits, with the most significant being the sign.

.. note::

   In this particular case it might be clearer to write:

   .. code-block:: lisp

      (let ((a 12))
	(declare (type (unsigned-byte 32) a))

	...)

   which would have the same effect.

Coercion depends on the type of the value and the type it is being
coerced to, and can cause issues. For the fixed-width integer types
(``unsigned-byte`` and ``signed-byte``) there are twelve cases:

1. The value and type are both ``unsigned`` with the same width:
   Nothing happens.

2. The value and type are both ``unsigned``, but the value is wider
   than the type: Extract the lower-order bits from the value, with
   the same width as the type.

3. The value and type are both ``unsigned``, but the value is narrower
   than the type: 0-extend the value to be the width of the type.

4. The value and type are both ``signed`` with the same width:
   Nothing happens.

5. The value and type are both ``signed``, but the value is wider
   than the type: Extract the lower-order bits from the value,
   one less than the width of the type, and prepend the sign bit of
   the value.

6. The value and type are both ``signed``, but the value is narrower
   than the type:  Sign-extend the value with copies of its sign bit
   to match the width of the type.

7. The value is ``unsigned``, but the type is ``signed``: There are
   two sub-cases:

   1. The type is narrower than, or the same width as, the value:
      Truncate the value by extracting its lower-order bits, one less
      than the width of the type, and prepend a 0 bit to indicate that
      the resulting signed number is positive.

   2. The type is wider than the value: Extend the value by adding 0
      bits to it in the most significant positions, including a 0 in
      the sign bit to indicate that the number is positive.

8. The value is ``signed`` but the type is ``unsigned``: There are two
   sub-cases:

   1. The type is narrower than the value: Truncate the value by
      extracting its lower-order bits, one more than the width of the
      type. Take the absolute value of these bits, and then truncate
      this number to the width of the type.

   2. The type is wider than, or the same width as, the value: Take
      the absolute value, prepend as many 0 bits needed to match the
      width of the type.

The representation changes in four of these sub-cases, and in three of
them there is a potential loss of precision that is signalled.

The type of a ``coerce`` form is the type that it coerces to. It
synthesises the code needed to perform the conversion.

It's permissible to omit the width of the type being coerced to, for
example:

.. code-block:: lisp

   (let ((a (make-bitfields 1 1 0 0 1 1 0 1)))
     (let ((b (coerce a 'signed)))
       ...)

This will give ``a`` the type ``(unsigned 8)``, and then coerce it to
type ``(signed 8)``, with a warning of a possible loss of precision.


Casting
-------

A *cast* declares that a given value is of a given type, without
performing any change of representation.

The ``the`` form simply checks that a value can be interpreted as a
value of the type required, and takes this as its type. It signals an
error if the value can't be interpreted in this way.


.. note::

   ``the`` does not synthesise any device logic: it just works to help
   Verilisp interpret code correctly. ``coerce``, by contrast, will
   synthesise the device logic necessary to convert between
   representations.
