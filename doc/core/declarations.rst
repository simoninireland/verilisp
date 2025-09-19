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
	      (width 1 clk))
     ...)

is equivalent to the declaration above.


Declarations shared with Common Lisp
------------------------------------

As well as ``type``, Verilisp accepts the ``ignored`` and
``ignorable`` declarations to mark variables as either definitely or
possibly not used within their scope. This affects the warning
messages that are produced during type-checking.


Declarations not in Common Lisp
-------------------------------

The ``as`` declaration can be used to assign a representation to a
variable, and can take one of the symbols ``register``, ``wire``, and
``constant``.

The declaration ``direction`` assigns directions for variables in and
out of modules, and can take one of the symbols ``in``, ``out``, or
``inout``. It has no effect in other contexts.
