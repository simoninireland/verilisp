.. _core-macros:

Macros
======

Macros are a core part -- perhaps *the* core part -- of Lisp, and
Verilisp provides them as one way to extend the language.

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


Importing macros
----------------

Some Lisp macros can be imported as-is into Verilisp, and so don't
have to be redefined:

.. code-block:: lisp

   (importmacro/vl cond)

This imports the normal ``cond`` macro for use in Verilisp.

.. warning::

   Be careful when importing macros: the code they expand to needs to
   (eventually) be Verilisp. This can be harder than it might appear.
   The Common Lisp standard under-specifies (from our perspective) the
   code to which even standard macros expand, leaving the details to
   implementations. This allows implementations to optimise in
   different ways, but also means that a macro may expand to code that
   makes use of implementation-dependent features. This is why
   Verilisp explicitly codes some standard macros (like ``do``), to
   ensure compatibility on *all* Lisps.


Compatibility
-------------

Verilisp macros behave essentially identically to those of Common
Lisp, although there are some differences in the way they are
implemented.
