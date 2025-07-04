.. _core-declarations:

Declarations
============

Verilisp supports declarations to annotate bindings and other uses of
variables.

The most common usage is to declare the types of variables, for
example:

.. code-block:: lisp

   (let ((a 0)
	 (clk 0))
     (declare (type (unsigned-byte 32) a)
	      (type bit clk))
     ...)

This adds types to ``a`` and ``b``. ``width`` can be used as a synonym
for ``type`` when assigning unsigned integer types, for example:

.. code-block:: lisp

   (let ((a 0)
	 (clk 0))
     (declare (width 32 a)
	      (type bit clk))
     ...)

is equivalent to the declaration above.

The declaration ``as`` can be used to assign a representation to a
variable, and can take one of the symbols ``register``, ``wire``, and
``constant``.

The declaration ``direction`` assigns directions for variables in and
out of modules, and can take one of the symbols ``in``, ``out``, or
``inout``.

The declarations ``ignore`` and ``ignorable`` can be used as in Common
Lisp to indicate that a variable will not, or may not, be used in its
scope. This can suppress some warnings.

Declarations all add information to a program that the compiler can
use. Verilisp will in any case attempt to infer the various attributes
of variables, and issue warnings if the inferred types are
incompatible with any provided by ``declare``.


Compatibility
-------------

Verilisp's ``declare`` is entirely compatible with Common Lisp usage,
but supporting some different annotations. The annotations shared with
Common Lisp are:

- ``type``
- ``width``
- ``ignore`` and ``ignorable``

The new annotations are:

- ``as``
- ``direction``
