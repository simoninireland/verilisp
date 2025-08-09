.. _naming:

A note on naming
================

Verilisp is a domain-specific Lisp dialect embedded into Common Lisp.
Being Lisp everywhere might be expected to cause confusion as to
whether a particular piece of code is Verilisp or Common Lisp.

Actually the situation is even more confused than this, because
Verilisp allows macros, written in Common Lisp, to be used in Verilisp
programs. So the boundary between the languages is quite fluid.

We have adopted the convention that all top-level functions intended
to introduce new pieces of Verilisp end with ``/vl``. This would
normally be considered bad form by many Lisp programmers, as it ties
the name of a function or macro to the name of a package. However,
the convention helps in several ways.

For modules introduced with ``defmodule/vl``, the naming convention
indicates clearly that the code within the macro body is Verilisp, not
Common Lisp. In this case the ``/vl`` suffix makes it clear what forms
are (and are not) allowed.

For macros introduced with ``defmacro/vl``, the code is written in
Common Lisp, and so can use all the Common Lisp forms and features as
long as, eventually, the code expands to core Verilisp. In this case
the ``/vl`` suffix makes it clear that the macro is intended for use
in Verilisp -- and can't be used in Common Lisp as it doesn't exist in
the latter's namespace. Similarly, ``macrolet/vl`` lets Verilisp
macros introduce other, local, Verilisp macros.

There is another reason for the naming convention. We are essentially
re-using ``defmacro`` in a different context: to define macros for the
embedded DSL. We will *also* want to the define ordinary Lisp macros,
and so will need the "normal" definition of ``defmacro`` to remain
available. Also, Common Lisp places a package lock on the
``COMMON-LISP`` package to prevent any symbols exported from it being
re-defined.

If this all sounds confusing -- well, it doesn't *seem* to be too
difficult in practice, but your opinion may vary!
