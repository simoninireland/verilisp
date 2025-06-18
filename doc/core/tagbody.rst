.. _core-tagbody:

State machines (``tagbody`` / ``go``)
=====================================

Lisp provides the ``tagbody`` / ``go`` constructs to make it easier
to construct state machines. Since state machines are ubiquitous in
hardware, Verilisp provides them too.


Overview
--------

The body of a ``tagbody`` form consists of a sequence of states, each
tagged with a symbol and consisting of several forms. For example.

.. code-block:: lisp

   (let ((a 0))
      (@ (posedge clk)
	 (tagbody
	  waiting
	    (unless (= a 0)
	       (go waiting))

	  adding
	    (incf a)

	  testing
	    (if (= a 10)
		(go waiting)
		(go adding)))))

(This is a ridiculous example, obviously.) When the machine is entered
it is initially in the ``waiting`` state -- and will stay there until
and unless ``a`` is zero, at which point it will transition to the
``adding`` state. This increments ``a`` and then drops-through to the
``testing`` state, whose next state depends on the value of ``a``:
either we carry on dding, or go back to the waiting state.

The ``go`` form transitions the machine between states. It can jump
the machine to any state in the machine.


Nested machines
---------------


Notes
-----

.. warning::

   There are some slight differences between Common Lisp and Verilisp
   when it comes to ``tagbody`` / ``go``:

   1. In Common Lisp, ``tagbody`` and ``go`` are both *special forms*
      that are built-in to the language. In Verilisp, they are
      *macros*. This is purely an implementation detail that is
      unlikely to affect anything.
   2. In Common Lisp, evaluating a ``go`` form causes the program to
      immediately jump to the labelled block of code. In Verilisp,
      evaluating ``go`` jumps to that code *at the next turn of the
      state machine* and *not immediately*. That's why we need
      ``tagbody`` to appear inside a block like ``@`` above: the
      machine turns every time the block is entered, which is
      typically at each clock cycle.
