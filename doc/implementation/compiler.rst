.. _implementation-compiler:

.. cl:package:: verilisp/core

Compiler structure
==================

Verilisp is structured as a nanopass compiler
:cite:`NanopassCompilerEducation` making use of functions over the
code tree. The nanopass approach has slightly lower performance than
more traditional compiler structures because it makes more passes over
the code tree. However it is conceptually simpler as a single pass
focuses on a single issue, and usually with a single generic function.


Top-level compilation stages
----------------------------

The compilation process is conducted by three top-level passes.

.. note::

   These functions are not usually called directly, but are called as
   part of :cl:macro:`defmacro/vl`.


.. cl:function:: expand/vl

.. cl:function:: typecheck/vl

.. cl:function:: elaborate/vl

Once compiled, Verilisp code is synthesised to Verilog using another
pass.

.. cl:function:: synthesise/vl


Nanopasses
----------

The first stage of compilation is *expansion*. This removes macros,
adds environment frames, and then populates these frames with details
extracted from an analysis of the code's syntactic structure.

.. cl:generic:: expand-macros
   :nospecializers:

.. cl:generic:: add-frames
   :nospecializers:

.. cl:generic:: compute-dependencies
   :nospecializers:

.. cl:generic:: infer-representation
   :nospecializers:

The second stage is *type checking and inference*, which combines
explicit types given in the code with inference based on variables'
uses.

.. cl:generic:: compute-type
   :nospecializers:

.. cl:generic:: apply-type-constraints
   :nospecializers:

The third stage is *elaboration*, which transforms the code the
programmer wrote into something more appropriate for synthesis.

.. cl:generic:: transform
   :nospecializers:

.. cl:generic:: float-let-blocks
   :nospecializers:

.. cl:generic:: simplify-progn
   :nospecializers:

The final stage is *synthesis*, which takes elaborated Verilisp and
generates equivalent Verilog ready for feeding into the FPGA
toolchain.

.. cl:generic:: synthesise
   :nospecializers:

An alternative synthesis step generates Lisp instead of Verilog,
intended for driving simulations (but also used internally when
evaluating static forms).

.. cl:generic:: lispify
   :nospecializers:


Code functions
--------------

The compiler nanopasses are supported by several functions defined
over the code forms that extract important features.

.. cl:generic:: read-variables
   :nospecializers:

.. cl:generic:: read-variables-setf
   :nospecializers:

.. cl:generic:: written-variables-setf
   :nospecializers:

.. cl:generic:: rewrite-variables
   :nospecializers:

.. cl:generic:: generalised-place-p
   :nospecializers:


Helper functions
----------------

.. cl:function:: add-dependencies

.. cl:function:: traverse-dependencies

.. cl:function:: mark-variable-as-read

.. cl:function:: mark-variable-as-written

.. cl:function:: variable-read-p

.. cl:function:: variable-written-p

.. cl:function:: add-local-frame-to-decls

.. cl:function:: get-local-frame

.. cl:macro:: with-local-frame
