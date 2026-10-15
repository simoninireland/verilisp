.. _implementation-type-of-types:

Type-of types
=============

e often want to define a type with reference to the types of *other*
terms -- and those types may not be known yet. We therefore allow type
expressions to include the types of values. A ``type-of`` type has the
form:

.. code:: lisp

   (type-of a)

where ``a`` is the name of a variable. When this type expression is
evaluated, the type of ``a`` will be used.

Since the ``type-of`` type contains an expression, it is usual to have
is appear inside a :ref:`closure type <closure-type>` to capture the
correct environment in which to evaluate the expression.
