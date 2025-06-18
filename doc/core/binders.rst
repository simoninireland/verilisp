.. _core-binders:

Binders
=======

Verilisp's main binding form is ``let``. This behaves as it does in
Lisp, but also accepts additional keyword arguments in each assignment
clause the help code generation.


Example
-------

.. code-block:: lisp

   (let ((a 12 :type (unsigned-byte 8) :as :register))
      (setq a 34))

defines a ``let`` block that bring a variable ``a`` into scope in its
body. ``a`` is defined to have a type ``(unsigned-byte 8)`` (8-bit
unsigned), and will be represented as a register.

.. warning::

   In Verilisp the initial value of any variable is set when it is
   declared *and not again*. This sometimes means that you need to
   reset the value of a variable explicitly, rather than allowing the
   ``let`` form to be re-evaluated and expecting the variable to be
   reset.


Keyword arguments
-----------------

``:type`` *type*
  Specifies the Lisp type for the variable. This is closely related to
  the width: width defines the *representation* in terms of bits,
  while type defines the *interpretation* of those bits.

``:width`` *bits*
  Shortcut for ``:type (unsigned-byte *bits*)``.

``:as`` *representation*
  Specifies how the variable is represented. Options include
  ``:register`` for a register, ``:wire`` for wires, and ``:constant``
  for constants.


.. note::

   Verilisp also has :ref:`core-rep-spec-binders` to save entering
   representations for all variables.
