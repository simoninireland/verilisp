.. _implementation-member-types:

Member types
============

Some types are *container types*, in that their main purpose is to
hold elements of other types. Arrays and records are the canonical
examples, with an array holding a number of elements of the same type,
while a record holds a collection of values of different types
identified by name.

A member type lets Verilisp express the type of these members. They
are commonly used alongside `type-of types
<implementation-type-of-types>`_ to access the type of array
elements. For example, the member type:

.. code:: lisp

   (member (type-of 'a (current-environment)))

represents the type of members (elements) of ``a`` in the current
environment.

At present arrays are Verilisp's only container type.
