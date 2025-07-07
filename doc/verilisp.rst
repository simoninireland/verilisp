.. _core:

Verilisp core
=============

Verilisp is a domain-specific language embedded into Common Lisp. It
represents the "synthesisable fragment" of Lisp: the sub-set of Lisp
forms that can be synthesised to an FPGA or similar hardware.
Essentially you can think of Verilisp as a Lisp-like alternative to
Verilog -- and indeed Verilisp works by synthesising a Verilog program
from the Verilisp code.

This may not seem very useful, but because Verilisp code is embedded
into Lisp we have all the power of Lisp available to generate Verilisp
code. As long as the result is Verilisp we can use *any* Lisp code to
generate it. Specifically we can define macros that expand Lisp code
into Verilisp code. The result is that we can easily extend Verilisp
with new language constructs, as long as they expand into the core
synthesisable fragment.


Types
-----

Verilisp leverages Lisp's type system by adding types appropriate for
hardware synthesis.

.. toctree::
   :maxdepth: 1

   core/fixed-width


Core forms
----------

The core of Verilisp covers all the constructs that can be directly
synthesised. It includes basic control flow, conditional, and maths
capabilities, as well as decomposition of code into modules and checks
on the types passed to operators.

.. toctree::
   :maxdepth: 1

   core/binders
   core/declarations
   core/operators
   core/conditionals
   core/cond
   core/assignment
   core/control-flow
   core/iteration
   core/tagbody
   core/arrays
   core/with-bitfields
   core/coercion-casting
   core/macros
