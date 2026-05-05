.. _core-binders:

Binders
=======

Verilisp's main binding form are ``let`` and ``let*``.


Example
-------

The code fragment

.. code-block:: lisp

   (let ((a 12))
      (setq a 34))

defines a ``let`` block that bring a variable ``a`` into scope in its
body. As usual, ``let`` can't refer to previous bindings: for that,
use `let*``.

.. code-block:: lisp

   (let ((a 12)
	 (b (+ a 26)))
      (setq a (+ b 12))


.. note::

   It is possible (and sometimes necessary) to annotate variables
   using :ref:`core-declarations`. Verilisp will attempt to infer the
   information is needs for compilation, and will check that any
   explicit declarations match the requirements it infers.
