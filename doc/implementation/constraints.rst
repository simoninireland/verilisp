.. _implementation-constraints:

Constraints in compiling Lisp to Verilog
========================================

There are several constraints we need to observe when mapping Lisp to
Verilog, and these have shape the design of Verilisp.

The main difference between the two languages is that Lisp is a value-
or expression-oriented language: every form returns a value and for
the most part, with a few exceptions, any form can appear in any
position in a program. By contrast Verilog is statement-oriented
language that permits a more restricted style of coding, since only
some forms have values. This makes the syntax of Verilog a lot more
restrictive than that of Lisp.


Variables
---------

All variables in Verilog appear at the top level within a module, and
can't be buried lower-down in the syntax.

We resolve this by floating ``let`` forms outwards to directly inside
their surrounding module. We are careful to maintain lexical ordering,
so if a variable is assigned a value based on another variable, that
variable will have been floated above it (and so will be defined).

This does have the consequence that variables are initialised once (in
their floated position) rather than every time the ``let`` form would
be executed in its notional position. In practice this means assigning
the value of a variable using ``setf`` if its value needs to be fixed.
