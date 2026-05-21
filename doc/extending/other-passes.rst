.. _extending-other-passes::

Other passes and code functions
===============================

Analysing variable usage
------------------------

.. cl:generic:: read-variables
   :nospecializers:

.. cl:generic:: read-variables-setf
   :nospecializers:

.. cl:generic:: written-variables-setf
   :nospecializers:

.. cl:generic:: rewrite-variables
   :nospecializers:


Form properties
---------------

.. cl:generic:: generalised-place-p
   :nospecializers:

.. cl:generic:: simple-expression-for-p
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
