.. _core-shortcuts:

Shortcuts
=========

Several sets of macros provide quick shortcuts for common operations.


.. _core-maths-shortcuts:

Maths shortcuts
---------------

- ``(1+ v)`` -- add one to value
- ``(1- v)`` -- subtract one from value
- ``(2* v)`` -- double value
- ``(2/ v)`` -- halve value

These shortcuts use ``setf`` and so can be used to update generalised
places.


Comparison shortcuts
--------------------

- ``(0= v)`` -- test if value is zero
- ``(0/= v)`` -- test if value isn't zero


.. _core-incf-decf:

Increment/decrement
-------------------

- ``(incf place &optional value)`` -- increment the value at place
- ``(decf place &optional value)`` -- decrement the value at place

``value`` defaults to 1 for each macro.


.. _core-psetq:

Parallel update
---------------

- ``(psetq var1 val1 var2 val2 ...)`` -- update the variables in
  parallel, so all updates see the old versions of the variables
  (before they are updated)


.. _core-when-unless:

Single-armed conditionals
-------------------------

- ``(when condition body)`` -- execute body if condition is true
- ``(unless condition body)`` -- execute body if condition is false
