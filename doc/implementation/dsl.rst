.. _implementation-dsl::

Representing the DSL
====================

The Verilisp DSL is designed to simplify the construction of the
recursive functions at the heart of the compiler. It operates as macro
set for defining passes, which embed the passes' code into normal
Common Lisp generic functions. This page describes this encoding.


Signature of a pass
-------------------

A pass is a function from source code to something else -- typically
source code too, but not necessarily. Lisp code is especially
well-suited to this approach as it is simply lists, and so can be
manipulated using all the standard list-processing machinery.

Slightly simplified, a piece of Lisp source code consists of either:

- an atom, like a number, a string or a symbol; or
- a list, whose head is a symbol identifying the function being
  applied and whose tail is the argument list for that function.

The arguments to a function are described by a **lambda list**, which
is a pattern describing how the list of arguments should appear.
Lisp's lambda-lists are quite flexible and can include optional
arguments (``&optional``), keyword arguments (``&key``), and
variable-length lists of arguments (``&rest``) -- with some
restrictions, as described in the hyperspec.

A function that handles source code recursively therefore needs to be
able to deal with either an atom or a list, and to change *how* it
deals with specific atoms and lists, with the latter being defined by
the function at the head of the list. The behaviour may deal with the
source code directly, or (more commonly) will recursively apply the
*same* pass to parts of the form and combine the results.


Encoding the pass
-----------------

A pass therefore accepts a Lisp **form** (an atom or list) and varies
its behaviour based on exactly *which* form it receives. This
identifies a pass as a Lisp generic function whose overall form is
modified by **methods** that are selected dynamically as the function
is applied to different arguments.

Source code is always *either* an atom *or* a list. This suggests a
further decomposition into two generic functions, one for forms in
general and one for list forms. This has further advantages in terms
of pattern-matching that we'll describe a bit later.

For concreteness in what follows let's consider a pass that
manipulates maths operators [#sample-operator]_. This pass might be
declared as follows.

.. code-block:: lisp

   (defpass operate (form)
      (:documentation "Process an operator applied to arguments.))


Pass top-level functions
------------------------

The top-level generic function takes a single argument, a form to be
processed.

.. code-block:: lisp

   (defgeneric operate (form)
      (:documentation "Process an operator applied to arguments.))

(Note that this is the code that ``defpass`` generates, not what a
programmer writes.)


Adding methods to the pass
--------------------------

Methods on this function provide different behaviours for the
different possible forms. For example, we might want to handle numbers
by simply returning the number itself. We can do this directly within
the ``defpass`` form.

.. code-block:: lisp

   (defpass operate ()
      (:documentation "Process an operator applied to arguments.)
      (:passmethod ((n integer))
	 n)))

This approach is copied directly from the way ``defgeneric`` works. It
specialises the generic function on the type of the form.

We can also add methods stand-alone to any pass [#defined-passes]_
using the ``defpassmethod`` form.

.. code-block:: lisp

   (defpassmethod operate ((n integer))
      n)

In either case, the method on the pass is encoded as a method on the
top-level generic function.

Now suppose we want to add a method for handling an operator, for
example ``(+ 1 2)``. We add this to the pss by using this form as a
lambda-list-style argument list.

.. code-block:: lisp

   (defpassmethod operate (+ a b)
      (* (operate a) (operate b)))

In this case, when we call ``operate`` on a form, one of several
things may happen:

- If the form is an ``integer`` then it matches the first
  specialisation of the pass method
- If the term is of the form ``(+ a b)`` then it will match the second
  specialisation

In the first case, ``operate`` will return the integer's value; in the
second case it will return  the sum of the two arguments ``a`` and
``b``, whatever they may be. The recursive calls ensure that if, for
example, ``a`` is itself a form like ``(+ 3 4)``, this will be
processed by ``one`` and used in resolving the value.



.. rubric:: Footnotes

.. [#sample-operator] This is a simplified version of the ``lispify``
		      pass in Verilisp.

.. [#defined-passes] Unlike for Lisp generics, passes have to be
		     defined before methods can be added to them.
