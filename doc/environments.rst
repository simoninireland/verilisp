.. _environments:

.. cl:package:: verilisp/core


Lexical environments
====================

Verilisp defines new lexical environments in a nested way, following
the structure of ``let`` forms.


The global environment
----------------------

Modules and macros defined using ``defmodule/vl`` and ``defmacro/vl``
respectively are global (just like macros defined using ``defmacro``
in Common Lisp) and appear in Verilisp's *global environment*. This
means they are available from any other Verilisp module or macro.

If required, the global environment can be cleared.

.. cl:function:: clear-global-environment

Redefining modules and macros is permitted, which will overwrite the
previous definition and result in a DUPLICATE-MODULE or
DUPLICATE MACRO warning. Definitions can also be removed.

.. cl:function:: forget-variable/vl


The core environment
--------------------

Verilisp also has a *core environment* that contains the macros
declared within the core language. Entries in this environment are
available everywhere, like the global environment -- but unlike the
global environment, the core environment can't be cleared and
definitions can't be overridden.

.. note::

   In Common Lisp terminology, there is a *package lock* on the core
   environment.
