.. _implementation-types:

.. cl:package:: verilisp/core

Type system and its implementation
==================================

Verilisp re-uses Common Lisp's type system, which is very flexible and
provides a lot of features for defining and comparing types. It also
adds some type operations, and replaces others, to provide the
functionality Verilisp needs.


A brief review of Lisp's numeric types
--------------------------------------

Lisp's numeric types are extremely flexible. Arithmetic and other
operations generally work over all number formats, so, for example,
one can add a small in
teger to a floating-point number (resulting in
another floating-point number, typically of type ``SINGLE-FLOAT``), or
divide one integer by another (generating a ``RATIO``). The exact
types generally don't matter since their representations are hidden
and they can be manipulated transparently.

Lisp even has several types of integers. The most general is
``INTEGER``, which represents integers of arbitrary precision. The
closest analogue to integers in "normal" languages is ``FIXNUM``,
representing integers represented with a compiler-defined number of
bits (typically 16, 32, or 64). An operation on ``FIXNUM`` values that
exceeded the representation would automatically be coerced into an
``INTEGER``, so the programmer never usually sees these details.

While ``FIXNUM`` has a fixed-width representation, Lisp also lets
programs state the widths to be used. The type ``UNSIGNED-BYTE``
represents binary-encoded unsigned numbers; ``(UNSIGNED-BYTE 16)``
represents unsigned 16-bit numbers; ``(SIGNED-BYTE 16)`` represents
signed 16-bit numbers;; and so forth.

All this flexibility is extremely valuable when working in software,
and makes it the compiler's responsibility (rather than the
programmers') to manage data representation efficiently.


The need for our own type system
--------------------------------

However, the requirements for hardware design are slightly different.
In particular, hardware needs to allocate specific numbers of bits
(and wires) to represent adescripnd move data around the chip. This means
keeping close track of the bit-widths of values as they pass through
expressions. For these purposes Lisp's generality is a *disadvantage*,
because it masks these details so well.

Moreover, the vast majority of Lisp's type-checking happens at
run-time -- at least as it appears to the programmer. In fact modern
Lisp compilers typically do a lot of compile-time analysis, but this is
not exposed within the language.

For Verilisp we want to avoid the generality of Lisp's types so we can
track and manipulate the representations of values. But we don't want
to sacrifice Lisp's great strengths in terms of general expression: we
don't want the *programmer* to have to care about representation As is
the case when writing Verilog), we want the *compiler* to deal with
it.


Type inference
--------------

The solution Verilisp adopts is to present itself to the programmer as
a Lisp-style language and type system, but have the Verilisp compiler
do more (and different) analyses to track the information we need.
The technical approach to this problem is *type inference*.

The basic idea of type inference is simple. Given an operation and
some values, we should be ale to determine (a) whether the values are
appropriate to the operation, and (b) what the resulting type of the
operation is. If we add two integers, for example, we know we should
get an integer as a result; if we add an integer and a string, we
should get a type error. In Lisp the latter case isn't always
determinable at compile time.

For hardware description we have even more information. If we add an
8-bit unsigned integer (of Lisp type ``(UNSIGNED-BYTE 8)``) to another
8-bit unsigned number, what is the type of the result? Clearly it will
be an ``UNSIGNED-BYTE`` -- there's no way to get a negative number,
for example -- but how many bits will it need? We only need to
consider the extremal cases:

- 0 + 0 = 0 (1 bit)
- 0 + 255 = 255 (8 bits)
- 255 + 255 = 510 (9 bits)

So the operation results in at most 9 bits of data: ``(UNSIGNED-BYTE
9)``.

What about several such numbers?

- 0 + 0 + 0 = 0 (1 bit)
- 0 + 0 + 255 = 255 (8 bits)
- 0 + 255 + 255 = 510 (9 bits)
- 255 + 255 + 255 = 765 (10 bits)

Or numbers with different widths?

- 8 + 255 = 263 (9 bits)

A pattern is starting to emerge. Adding two numbers together can
result in a number that is one bit wider than the widest of the two
numbers, because the result is always less than twice the larges
number, which requires one extra to represent it. Adding three numbers
means that the widest number can be at most doubled, and then doubled
again, meaning we would need two more bits. So the width needed when
adding *n* unsigned numbers is the width of the widest number plus
*(n - 1)* bits to avoid any loss of precision.

.. note::

   It is important to realise that these calculations use all the
   *potential* values of a given type, not any *specific* value that
   might appear. So for example a form like ``(+ 250 1)`` would get be
   given the type ``(UNSIGNED-BYTE 9)`` even though its value would
   never actually need 9 bits. (Actually the compiler could optimise
   this expression to a literal ``251`` and realise that its type
   could be ``(UNSIGNED-BYTE 8)``.)


Lisp types used in Verilisp
---------------------------


Types unique to Verilisp
------------------------

Verilisp'#s type system also includes a small number of types that are
unique to Verilisp.


.. toctree::
   :maxdepth: 1

   type-of-types
   width-types
   closure-types
