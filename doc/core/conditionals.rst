.. _core-conditionals:

Conditionals
============

Verilisp has two conditional forms supporting one- and two-armed
general conditions (``if``) and multi-way tests for value equality
(``case``).


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
forms in a ``progn`` if needed (see :ref:`core-control-flow`). The
false arm can consist of multiple forms, although some Lisp
programmers prefer to treat both arms the same. There are also
:ref:`single-armed conditionals <core-when-unless>` for the special
cases where the other arm is empty.

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
options, with a default option if no values match. The guards can be
single values or lists of values.

.. code-block:: lisp

   (case a
      (1
       (setq b 34))
      ((2 3 4)
       (setq c (*a b)))
      (t
       (setq a 0)))

The ``t`` branch is executed if none of the other branches is
triggered. As in Common Lisp, the values guarding the arms must be
constants, not expressions: however, unlike in Common Lisp, Verilisp
allows bindings to be declared as constants, so this is legal:

.. code-block:: lisp

   (let ((one 1)
	 (two 2))
      (declare (as constant one two))

      (case a
	 (one
	  (setq b 1))
	 ...))

because the guard is explicitly declared to be constant.

For a more flexible multi-armed conditional see :ref:`core-cond`.
