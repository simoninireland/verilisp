.. _core-modules:

Modules
========

A module is a self-contained block of code that can contain
:ref:`assignments <core-assignment>`, :ref:`variable declarations
<core-binders>`, and :ref:`sensitive blocks <core-control-flow>`.

Modules are introduced using the ``defmodule/vl`` macro:

.. code-block:: lisp

   (defmodule/vl alu (a b c opcode)
      (declare (type (unsigned-byte 16) a b c)
	       (type (unsigned-byte 3) opcode))
      (case opcode
	 (#2r000
	   (setq c (+ a b)))
	 (#2r001
	   (setq c (- a b)))

	 ...))

This defines a module ``alu`` that takes four arguments, that we
have decided to declare types for as we can for :ref:`variables
<core-binders>`. Verilisp will infer the types of arguments, as well
as their direction into or out of the module.


Optional arguments
------------------

Modules' lambda-lists can contain optional parameters, just like those
of Lisp functions.

.. code-block:: lisp

   (defmodule/vl tester (a b &optional c (d 25)) ...)

In this case ``d`` has been given a default value, while ``c`` will
get a default of 0 if not provided explicitly.


Keyword parameters
------------------

Modules can also take keyword parameters, again like Lisp -- but these
are treated somewhat differently. Arguments are synthesised to the
device and so must be representable; parameters, on the other hand,
are Lisp expressions and are *not* synthesised, but can instead be
used in expressions to parameterise the module.

.. code-block:: lisp

   (defmodule/vl alu (a b c opcode &key (width 16))
      (declare (type (unsigned-byte width a b c)))
      ...)

Again there is a default value provided.


Instanciating modules
---------------------

Modules are instanciated into other modules using ``make-instance``:

.. code-block:: lisp

   (defmodule/vl testbench (clk)

      (let (a b c opcode)

	 (let ((alu (make-instance :a a :b b :c c :opcode opcode)))
	    ...)))

In this code we construct an instance of the ``alu`` module, providing
the variables (or values) to bind to its arguments. (The names don;t
need to match, of course, they just happen to do so in this case.)
