.. _implementation-subtypes:

.. cl:package:: verilisp/core

Type operations
===============

Verilisp re-uses Common Lisp's type system, which is very flexible and
provides a lot of features for defining and comparing types. It also
adds some type operations, and replaces others, to provide the
functionality Verilisp needs.


Sub-typing
----------

Unfortunately, the built-in :code:`subtypep` function for testing the
sub-type relationship is slightly misbehaving for our purposes.
Specifically it doesn't short-cut the evaluation of type tags for types
that aren't compound, and this can cause some subtle problems.

Rather than code-around these problems individually, Verilisp defines
a new function that provides exactly the sub-typing relationships we
need.

.. cl:function:: subtype-p

The actual tests are provided generically.

.. cl:generic:: subtype-type
   :nospecializers:

:code:`subtype-type` should only be accessed by calling :code:`subtype-p`.

The disadvantage of this approach is the Verilisp implementation needs
to use a non-standard type comparison function. The two functions'
names are also perhaps too similar for comfort. The advantage is that
there is better control over the type comparisons, which may reflect
slightly different constraints than those that appear in Lisp generally.


.. _implementation-complex-types:

Complex type specifiers and lattice types
-----------------------------------------

Many operations are easier when performed using types constructed from
other types. Common Lisp supports several such complex type
specifiers, and Verilisp supports:

- Union types (for example :code:`(or unsigned-byte (signed-byte
  12))`) that include all values that are members of one or more of
  the component types
- Intersection types (for example :code:`(and unsigned-byte
  (unsigned-byte 12))`) that include all values that are members of
  all of the component types
- Negation types (for example :code:`(not unsigned-byte)`) that
  include all values that are values on the component type

.. note::
   Common Lisp also includes two additional complex type specifiers,
   :code:`satisfies` and :code:`member`. These are used to determine
	 whether a particular value is a member of a type, and as such
	 are not (yet) needed or supported by Verilisp.

The type lattice is completed with the :code:`t` (top) and :code:`nil`
(bottom) types.


Least upper-bounds (LUBs) of types
----------------------------------

The LUB of two types is the smallest type that has both types as a
sub-type. This ides is closely related to the idea of union
types, in that an element of either type is an element of the LUB
type. In this sense the LUB is the "smallest OR" type that includes
both types.

Verilisp provides two functions for computing LUBs.

.. cl:function:: lub


.. cl:generic:: lub-type
   :nospecializers:


Note that since the LUB relationship is symmetrical, you should make
sure that methods can if necessary handle types in either position.


Type (de)construction
---------------------

Lisp type specifiers can take several forms, the most common being a
symbol designating the type of a list consisting of the designator
symbol and zero of more additional parameters. When we build generic
functions over types we need to specialise on the designator "tag", so
we provide two functions to regularise the handling of type
specifiers.

.. cl:function:: deconstruct-type


.. cl:function:: construct-type
