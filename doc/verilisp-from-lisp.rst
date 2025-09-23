.. _verilisp-from-lisp:

.. cl:package:: verilisp/core


Verilisp from Lisp
==================

Verilisp is a domain-specific language embedded into Lisp. As such it
can be used from a normal interactive Lisp session to create code
for synthesis. It can also be used as a :ref:`traditional command-line
compiler <verilispc>` if you prefer.

The main interface to Verilisp is through a small set of operations.


.. cl:macro:: defmodule/vl


.. cl:macro:: defmacro/vl


.. cl:macro:: macrolet/vl


In some projects it may also be useful (or necessary) to import
existing IP written in Verilog. This can be done by defining a
Verilisp interface to the Verilog code that is then imported into the
FPGA toolchain.


.. cl:macro:: defmoduleinterface/vl
