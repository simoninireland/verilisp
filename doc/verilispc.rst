.. _verilispc:

The Verilisp compiler
=====================

``verilispc`` is the command-line compiler that transpiles Verilisp
into Verilog. It takes as input one or more Lisp source files that
should contain the code you want to compile using the :ref:`Lisp
interface <verilisp-from-lisp>` -- but can contain *any* Lisp code you
want. It then creates Verilog corresponding to the Verilisp
definitions and saves them ready for synthesis.


General behaviour
-----------------

By default ``verilispc`` takes a list of Lisp files, compiles them,
and generates corresponding Verilog files: a file ``test.lisp`` gives
rise to a file ``test.v`` containing the corresponding Verilog
modules. This can be changed with the ``-o`` option, which puts all
the Verilog into a single named file.

Warnings are displayed by default, but can be muffled using ``-W
none`` and then enabled individually using the ``-W`` option.
Similarly, warnings can be turned into fatal errors using the ``-F``
option. The ``--strict`` option makes *all* warnings fatal, and can be
used to force you to resolve all types and other issues by hand rather
than relying on Verilisp's solutions. By default no Verilog is
produced after an error: this can be changed using the
``--continue-on-error`` option.

The ``--elaborated-file`` option outputs the "elaborated" Verilisp to
the given file. This can be useful for debugging, letting you see the
core Verilisp the compiler *thinks* you intend, which you can then
compare to the Verilog it produces.


Command options
---------------

+--------------------------+---------------------------------------------------+
| Option                   | Example                                           |
+==========================+===================================================+
| ``-h``                   | Show options                                      |
| ``--help``               |                                                   |
+--------------------------+---------------------------------------------------+
| ``-v``                   | Show progress messages                            |
| ``--verbose``            |                                                   |
+--------------------------+---------------------------------------------------+
| ``--continue-on-fail``   | Create Verilog even after errors in Verilisp      |
+--------------------------+---------------------------------------------------+
| ``-W`` w [,w]*           | Show given warnings                               |
+--------------------------+---------------------------------------------------+
| ``-F `` w [,w]+          | Make given warnings fatal (errors)                |
+--------------------------+---------------------------------------------------+
| ``--strict``             | Make all warnings fatal (like ``-F all``          |
+--------------------------+---------------------------------------------------+
| ``--debug-on-error``     | Enter Lisp debugger on errors                     |
+--------------------------+---------------------------------------------------+
| ``-o`` fn                | Output all Verilog to file                        |
| ``--output-file`` fn     |                                                   |
+--------------------------+---------------------------------------------------+
| ``-e`` fn                | Output final elaborated Lisp file to file         |
| ``--elaborated-file`` fn |                                                   |
+--------------------------+---------------------------------------------------+


Warnings
--------

The mapping between warning conditions and names passed to ``-W`` and
``-F`` is:

+-------------------------------+----------+
| Condition                     | Flag     |
+===============================+==========+
| ``value-mismatch``            | value    |
+-------------------------------+----------+
| ``type-mismatch``             | type     |
+-------------------------------+----------+
| ``representation-mismatch``   | rep      |
+-------------------------------+----------+
| ``coercion-mismatch``         | coerce   |
+-------------------------------+----------+
| ``precision-mismatch``        | prec     |
+-------------------------------+----------+
| ``unrecognised-declaration``  | declare  |
+-------------------------------+----------+
| ``type-inferred``,            | infer    |
| ``direction-inferred``, and   |          |
| ``states-inferred``           |          |
+-------------------------------+----------+

Any other conditions are errors that can't be suppressed. See
:ref:`core-conditions` for an explanation of the different conditions.
