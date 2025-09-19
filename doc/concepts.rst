.. _core-concepts:

Concepts
========

.. glossary::

   bitfield
      One or more bits within the body of a larger sequence of bits
      that should be manipulated as a unit. See :ref:`core-bits` and
      :ref:`core-with-bitfields`.

   cast

      An assertion of the type of a value.

   coercion

      An assertion of the type of a value that changes the
      representation of that value if necessary. Unlike a
      :term:`cast`, a coercion may synthesise hardware to perform the
      change of representation, for example to perform :term:`sign
      extension`.

   constant

      A value whose value is always the same. This includes literal
      constants like 12 and variables declared to be constant (see
      :ref:`core-declarations`).

   fixed-width number

      A number that can be represented in a fixed number of bits. This
      implies that there is a maximum range of values that can be
      represented. Typical examples are bits, bytes, half- and
      full-words. Negative numbers are stored using :term:`twos
      complement`.

   sign extension

      Maintain the sign of a value when changing its width, typically
      as part of a :term:`coercion`.

   static constant

      A static constant is either a :term:`constant` or a keyword
      parameter to a module.

   static expression

      An expression consisting solely of :term:`constants <constant>`
      or :term:`static constants <static constant>`.

      The significance of static expressions (also referred to as
      *statically known*) is that their values are know at
      compile-time. This is required in some circumstances, such as in
      determining with width of a bitfield (see :ref:`core-bits`).

   twos complement

      The standard way of representing a positive or negative
      :term:`fixed-width number`.
