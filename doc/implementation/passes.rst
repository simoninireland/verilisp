.. _implementation-passes:

.. cl:package:: verilisp/core


Compiler passes and functions over code
=======================================

The internals of the compiler are only important if you want to extend
the special forms of Verilisp: the use the system only the
:ref:`top-level interface functions <implementation-top-level>` are
needed.

The distinction between a *pass* and a *code function* is that a pass
re-writes the program's source code into a form that's then ingested
by future passes, while a code function returns some other data
extracted from the code.


Passes
------

.. cl:generic:: expand-macros
   :nospecializers:


.. cl:generic:: add-frames
   :nospecializers:


.. cl:generic:: float-let-blocks
   :nospecializers:

.. cl:generic:: simplify-progn
   :nospecializers:


Code functions
--------------

.. cl:generic:: compute-type
   :nospecializers:


.. cl:generic:: apply-type-constraints
   :nospecializers:


.. cl:generic:: generalised-place-p
   :nospecializers:


.. cl:generic:: read-variables
   :nospecializers:


..
  .. cl:generic:: read-variables-setf
     :nospecializers:


.. cl:generic:: synthesise
   :nospecializers:


.. cl:generic:: lispify
   :nospecializers:
