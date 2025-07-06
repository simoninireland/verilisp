.. _core-binders:

Binders
=======

Verilisp's main binding form is ``let``.


Example
-------

The code fragment

.. code-block:: lisp

   (let ((a 12))
      (setq a 34))

defines a ``let`` block that bring a variable ``a`` into scope in its
body.

.. note::

   It is possible (and sometimes necessary) to annotate variables
   using :ref:`core-declarations`. Verilisp will attempt to infer the
   information is needs for compilation, and will check that any
   explicit declarations match the requirements it infers.


Compatibility
-------------

``let`` is entirely compatible with Common Lisp.
