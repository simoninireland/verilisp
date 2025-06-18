.. _core-conditionals:

Conditionals
============

Verilisp has two conditional forms supporting one- and two-armed general
conditions (``if``) and multi-way tests for value equality (``case``).

``if``
------

The ``if`` form works as in Lisp.

.. code-block:: lisp

   (if (= a 12)
      ;; true arm
      (setq b 0)

      ;; false arm
      (setq b 1))

As with Lisp, the true arm consists of a single form: wrap multiple
forms in a ``progn`` if needed (see :ref:`vl-control-flow`). The
false arm can consist of multiple forms, although some Lisp
programmers prefer to treat both arms the same.

``if`` can also appear as an expression, for example:

.. code-block:: lisp

   (let ((b (if (= a 12)
	       0
	       1)))
      ...)

.. warning::

   When used as an expression like this the arms of the ``if`` can
   only be simple expressions, not arbitrary code as can be done in
   Lisp.


``case``
--------

The ``case`` form tests a value for equality against different
options, with a default option if no values match.

.. code-block:: lisp

   (case a
	 (1 (setq b 34))
	 (2 (setq c (*a b)))
	 (t (setq a 0)))

The ``t`` branch is executed if none of the other branches is
triggered. The values guarding the arms must be constants, not
expressions: for a more flexible multi-armed conditional see
:ref:`core-cond`.
