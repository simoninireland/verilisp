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

Verilisp leverages Lisp's type system using types appropriate for
hardware synthesis.

.. toctree::
   :maxdepth: 1

   core/fixed-width


Special forms
-------------

The Verilisp special forms cover all the constructs that can be
directly synthesised. They include basic control flow, conditional,
maths capabilities, and state machines, as well as decomposition of
code into modules.

.. toctree::
   :maxdepth: 1

   core/binders
   core/declarations
   core/operators
   core/conditionals
   core/assignment
   core/control-flow
   core/modules
   core/tagbody
   core/arrays
   core/coercion-casting


Extra forms
-----------

The extra forms build on the core language using macros.

.. toctree::
   :maxdepth: 1

   core/macros
   core/cond
   core/shortcuts
   core/iteration
   core/with-bitfields


Conditions
----------

.. toctree::
   :maxdepth: 1

   core/conditions


Other features

.. toctree::
   :maxdepth: 1

   environments
