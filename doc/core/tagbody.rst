.. _core-tagbody:

State machines
==============

Lisp provides the ``tagbody`` / ``go`` constructs to make it easier
to construct state machines. Since state machines are ubiquitous in
hardware, Verilisp provides them too.

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
	    (when (< a 10)
	      (go adding)))))

(This is a ridiculous example, obviously.) When the machine is entered
it is initially in the ``waiting`` state -- and will stay there until
and unless ``a`` is zero, at which point it will transition to the
``adding`` state. This increments ``a`` and then drops-through to the
``testing`` state, whose next state depends on the value of ``a``:
either we carry on dding, or go back to the waiting state.

The ``go`` form transitions the machine between states. It can jump
the machine to any state.

In the final state of the machine, of control falls-through, it
falls-through to the initial state of the machine. This implies that
Verilisp state machines don't generally terminate. If you want to
avoid this behaviour, add a "passivating" state at the end:

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
	   (when (< a 10)
	     (go adding))

	 passive
	   (go passive))))

Now ``testing`` falls-through to ``passive`` (rather than to
``waiting``), and stays there.


.. warning::

   This is a small difference in the way that Verilisp handles state
   changes compared to Common Lisp: see
   :ref:`implementation-tagbody-differences-with-cl` for details. This
   may be important for code being ported from Common Lisp to Verilisp.


Nested state machines
---------------------

It's perfectly acceptable to have a state machine appearing in
the state of another. In this case, on entering the state of the outer
machine containing the inner machine, the inner machine will run and
the outer machine will remain blocked in this state until the inner
machine exits.

How should the inner machine exit? It should call ``go`` targeting a
state in the *outer* machine. The inner machine will then exit and (at
the next turn of the machine) execute the targeted state. In addition,
the inner machine's state will be reset to its own initial state, so
the next time it is entered (when the outer machine enter the state
containing it) it will always start in a predictable state.

Machines can be nested arbitrarily, and a machine can execute a ``go``
to a state in *any* machine that lexically surrounds it (so that the
label is in scope). Resetting of machines happens up the stack: if you
jump from an inner-inner machine to the state of the outer machine,
both the inner-inner and inner machines have their states reset to
their respective initial states.


.. warning::

   A common coding pattern is to have the inner machine appear within
   a ``let`` form that defines some variables for it to use. Bear in
   mind that the initial value of a variable is set when it is
   declared *and isn't reset*, so if you *want* to reset it every time
   the inner machine runs you should set it explicitly, for example:

   .. code-block:: lisp

      (tagbody
       start                  ; outer machine starts here
	 ...

       inner
	(let ((b 5))          ; declare state for inner machine
	  (tagbody
	   inner-start
	     (setq b 5)       ; put b in a known state

	   inner-delay
	     (decf b)
	     (when (> b 0)
	       (go inner-delay))

	   inner-exit
	     (go cont)))      ; escape to the outer machine,
			      ; resetting the inner machine
			      ; to inner-start

       cont                   ; outer machine continues here
	 ...)

   This is entirely consistent with the behaviour of ``let`` forms
   across Verilisp (see :ref:`core-binders`).
