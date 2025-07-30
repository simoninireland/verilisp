.. _writing-macros:

Writing Verilisp macros
=======================

Verilisp uses :ref:`macros <core-macros>` just like Lisp does: as a
way to provide new features on top of the core language. There are
macros built into the core language, and you can define your own using
``defmacro/vl`` and ``macrolet/vl``.

There are a couple of crucial differences it's important to be aware
of.


Verilisp macros are written in Lisp
-----------------------------------

Lisp macros are written in Lisp. Verilisp macros are also written in
Lisp -- *not* in Verilisp. This makes sense if you think about it:
macros run at compile-time, and so run within Common Lisp and not on
the targeted device.

This does however have some important implications. Verilisp macros
can use all of Lisp's features: they can call functions, use lists, do
recursion, and so forth. As long as the code that they reduce to is
Verilisp, possibly containing macros that are then also reduced to
Verilisp, everything will be fine. (If you generate code that *can't*
be reduced to Verilisp you'll simply get a type-checking failure later.)


Verilisp macros are only defined in Verilisp
--------------------------------------------

A more subtle implication of the above is that Verilisp macros are
*only* defined in Verilisp code, and *not* in Common Lisp code -- so
not in the bodies of other Verilisp macros (although they can of
course appear in the code those macros produce). Again, this makes
sense, but can be confusing.

If you want to have a macro in *both* Verilisp and Common Lisp you can
always define your macro twice, once with ``defmacro/vl`` and once
with ``defmacro``. The two could even use different approaches to get
the same effect: in fact this is exactly the situation with Verilisp
core macros like ``do`` and ``when`` that replicate the corresponding
Common Lisp macros in Verilisp.
