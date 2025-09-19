.. _index:

Verilisp: A very Lisp approach to hardware synthesis
====================================================

Modern chips are usually constructed using high-level synthesis, where
a specification in a language like Verilog or VHDL is compiled to a
gate array for use on a FPGA or ASIC. But the abstraction level of
these languages is quite low -- similar to C -- and they don't support
a lot of the language features one might like.

Verilisp is an experiment to perform high-level hardware synthesis at a
higher level. It is implemented as a domain-specific language embedded
into Common Lisp, with a view to using Lisp code to both specify
hardware and manipulate that specification.

As a driving challenge an immediate goal of Verlisp is to describe the
architectures of old-school processors (Z80, 6502, 6809) to allow
them to be simulated in software or synthesised on FPGAs. This
involves describing the basic components in a manner that's amenable
to both interpretations, from the same description.


.. toctree::
   :hidden:

   install
   verilisp
   using
   implementation
   concepts
   zbibliography
   acknowledgements
