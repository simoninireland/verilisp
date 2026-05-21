.. _extending-passes

Passes
======

In our terminology, a **pass** is simply  function that is defined to
traverse a program recursively by structural induction: the function
"follows the shape" of the code, processing simple structures directly
and descending into more complex structures where required.

We include as passes both functions that transform the program and
those that traverse it to extract other information: a type-checking
pass is an example of the latter, computing a type while leaving the
program unchanged.


What do passes look like?
-------------------------


Pass functions and pass methods
-------------------------------

A pass is represented as a generic function, defined in the host
language over the forms of the guest language. A pass will always
accept a form in the guest, which might be an atom like :code:`1` or
an operation of program like  :code:`(progn (setf a (+ 24 b))
(return))`. Lisp-like languages have *only* these kinds of forms:
atoms or lists.


Defining passes
---------------

We define a pass using the ``defpass`` macro.

.. cl:macro:: defpass

We add methods to the pass using ``defpassmethod``.

.. cl:macro:: defpassmethod


Notice that the lambda list for the pass method include *only* the
guest form that it's processing. If the pass defines extra arguments
(which appear in ``defpass``'s lambda list) then these will be in scope
for the body of the method *even though they don't explicitly appear*.
