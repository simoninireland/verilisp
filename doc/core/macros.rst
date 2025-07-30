.. _core-macros:

Macros
======

Macros are an integral part -- perhaps the *most defining* part -- of
Lisp, and Verilisp provides them as one way to extend the language.

Verilisp supports a range of macros built-in, and allows other to be
built as usual. However, Verilisp doesn't "inherit" macros from Lisp:
just because a macro is defined in Lisp doesn't mean that a Verilisp
program will pick it up and use it. Verilisp takes control of exactly
which macros are available, to make sure that the code they generate
will work with Verilisp.

Verilisp macros are written in full Lisp, but need to generate
Verilisp -- eventually. That is to say, the code they return should
either be Verilisp, or be code containing further macros that
themselves eventually generate Verilisp. At the end of the macro
expansion phase, all the code in the program should be Verilisp. But
it is handy that macros can use all of Lisp in their function, because
it means we can use features that Verilisp doesn't have -- such as
lists and lambda-terms -- in writing macros, as long as these features
are eliminated by macro expansion.


Defining macros
---------------

A macro is introduced using ``defmacro/vl``:

.. code-block:: lisp

   (defmacro/vl 1+ (x)
      `(+ x 1))

This is identical to the normal ``defmacro`` form in Lisp, and can
take flexible lambda-lists in the same way.

Even though this is a Verilisp macro, available only from Verilisp,
its body is written in Lisp. This means that Verilisp macros can make
use of *all* Lisp features.


Local macros
------------

A Verilisp macro can define local macros for use within its scope
using ``macrolet/vl``:

.. code-block:: lisp

   (defmacro/vl expand-things (a &body body)
      "Expand THING macro within BODY."
      (macrolet/vl ((thing (b)
		       `(+ ,a ,b)))
	 `(progn
	     ,@body)))

When ``expand-things`` is used in a program, its body can contain
references to ``thing`` that will be expanded according to the
declaration:

.. code-block:: lisp

   (let ((x 10)
	 (y 20))
      (expand-things 12
	 (setq x (thing x))))

will expand to:

.. code-block:: lisp

   (let ((x 10)
	 (y 20))
      (setq x (+ 12 x)))

but use of ``thing`` outside the body of ``expand-things`` will fail.

.. warning::

   If you use the standard ``macrolet`` instead of ``macrolet/vl``
   things will not work as expected. The ``/vl`` suffix is a clue that
   the code is intended for Verilisp.


Compatibility
-------------

There are some significant differences between Verilisp macros and
"normal" Lisp macros, both in the way they're implemented (which is
largely invisible to the Verilisp programmer) and the way they're
expanded (which can cause confusion in complicated cases). See
:ref:`writing-macros` for more details.
