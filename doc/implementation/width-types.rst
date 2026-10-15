.. _implementation-width-types:

Width types
===========

When computing the widths of values it's often necessary to refer to
the widths of the types of *other* values within an expression. These
widths may not be known, because the terms' types may not have been
inferred or declared.

A width type is not actually a type at all: it's a term that can appear
inside a type expression denoting the width of a type. For example,
the type

.. code:: lisp

   (unsigned-byte (+ (width '(unsigned-byte 6)) 1))

denotes the type of unsigned integers whose representation requires one
more it than the width of ``(unsigned-byte 6)`` -- ``7``, in other
words. In this case the width is known, but might not be, for example:

.. code:: lisp

   (unsigned-byte (+ (width '(unsigned-byte 6)) 1))

where the type of ``a`` may not be inferred until later.

Since the ``width`` expression contains an expression, it is usual to
have is appear inside a :ref:`closure type <closure-type>` to capture
the correct environment in which to evaluate the expression.
