.. _core-cond:

Multi-armed conditionals
========================

The ``cond`` macro expands a sequence of tests into nested ``if``
forms.

.. code-block:: lisp

   (cond ((= a 12)
	  (setq b 23))
	 ((> a 45)
	  (setq b 99))
	 ((< b a)
	  (setq a b))
	 (t
	  (setq a 0)
	  (setq b 0)))

The tests can be totally unrelated to each other, as shown above.

.. warning::

   While ``cond`` can be used as an expression (because it expands
   into ``if``), this needs care: each arm would need to return a
   value in a single expression in order to be accepted.
