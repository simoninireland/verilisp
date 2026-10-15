.. _implementation-closure-types:

Closure types
=============

Since the exact types of terms can't always be determined locally, the
inference system sometimes needs to defer a calculation until later in
the type-checking pass. Closure types allow this.

A *closure* is an expression together with an environment that defines
all the variables appearing free within it. By analogy, a *closure
type* is a type that contains a calculation together with the
environment in which that calculation should take place. The
calculation is only performed when the closure type is compared
against other types.

As an example, consider the following code:

.. code:: lisp

   (let ((a 7)
	 (b 2))
     (+ a b))

The type of ``(+ a b)``, and therefore the type of the ``let`` form
overall, depends on the types of ``a`` and ``b``, which are not fully
known at this point: they need to be inferred. But ``+`` also sets the
width of the result depending on the widths of the arguments.

We can set the type of the ``+`` form to be:


.. code:: lisp

   (unsigned-byte (+ (max (width '(type-of a))
			  (width '(type-of b)))
		     1))

The resulting value is has the width of the widest argument plus one
less than the number of arguments added together. But we run the risk
that ``a`` and ``b`` will not be known in the environment that
evaluates the type expression. For this reason we actually assign the
type:

.. code:: lisp

   (closure (unsigned-byte (+ (max (width '(type-of a))
				   (width '(type-of b)))
			      1))
	    (current-environment))

This captures the type expression and the environment in which it
should be evaluated.

Closure types are only needed when there is an expression containing a
variable to be evaluated within the type expression. Typically this
means in expressions with widths that need to be calculated late.
Wrapping a type *without* any such calculations in a closure type will
do no harm.
