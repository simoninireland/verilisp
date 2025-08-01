.. _core-tagbody:

State machines
==============

Lisp provides the ``tagbody`` / ``go`` constructs to make it easier
to construct state machines. Since state machines are ubiquitous in
hardware, Verilisp provides them too.

The body of a ``tagbody`` form consists of a sequence of states, each
tagged with a symbol and consisting of several forms. The ``tagbody``
executes a single state each time it is entered.

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
either we carry on adding, or go back to the waiting state.

The first state may have its label omitted.

It is perfectly permissible to nest state machines within other state
machines.

The ``go`` form transitions the machine between states. It can jump
the machine to any state that is in scope, meaning that a nested
machine can exit into its surrounding machine if required.

In the final state of the machine, control will fall-through to a
"passivating" state that will stop the machine. Once stopped, that
machine can't be re-started. This can be avoided by simply making sure
the machine always jumps back into another state and never
falls-through.


Difference with Common Lisp
---------------------------

Verilisp's ``tagbody`` is very similar to Common Lisp -- but not quite
identical, for two reasons.

The first concerns execution. A hardware state machine is
*synchronous*, meaning it proceeds according to clock ticks. In Common
Lisp a ``tagbody`` machine executes just like a ``progn``, jumping
between states as instructed. In Verilisp, each state change (whether
implicitly between adjacent states or explicitly using ``go``) causes
the machine to exit until it is re-entered, normally at the next clock
tick. Verilisp ``tagbody`` forms will typically appear in ``@`` blocks
for this reason.

This has some interesting side-effects. It means, for example, that
there can be /two/ (or more) state machines in a single ``@`` block,
executing simultaneously, as well as other code. For example:

.. code-block:: lisp

   (@ (posedge clk)
      ;; constant code
      (decf rx-clk-divider)
      (when (0= rx-clk-divider)
	 (setq rx-clk-divider clk-divide)
	 (decf rx-countdown))

      ;; first state machine
      (tagbody
       rx-idle
	 (setq receiving-p 0)
	 ...)

      ;; second state machine
      (tagbody
       tx-idle
	 (setq transmitting-p 0)
	 ...))

Each time through the block the constant code will run, as will a
state of the first machine and a state of the second machine -- all in
a single clock tick. (In Common Lisp the constant code would run, *then*
the first machine to completion, and then the second machine to
completion.)

The second concerns the number of states. In Common Lisp the states of
the machine are exactly those specified in the ``tagbody`` form. In
Verilisp, because of the constraints of direct hardware
implementation, ``tagbody`` may compile with *more* states than
specified. This is invisible in terms of the results of evaluation,
but may consume extra clock ticks. This may be an issue when timing is
critical: see :ref:`implementation-tagbody` for more details if needed.
