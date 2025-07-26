.. _implementation-top-level:

.. cl:package:: verilisp/core


Top-level compiler functions
============================

The top-level compiler interface is used to build the :ref:`compiler
tools <verilispc>` and the :ref:`interface from Lisp <from-lisp>`.
These functions then call the lower-level :ref:`compiler passes
<implementation-passes>` to process the Verilisp source code.


.. cl:function:: expand/vl


..
  .. cl:function:: typecheck/vl
  .. cl:function:: elaborate/vl


.. cl:function:: synthesise/vl
