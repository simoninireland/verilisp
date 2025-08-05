.. _core-coercion:

Coercion and casting
====================

Sometimes an application needs to change or assert the type of some
value or expression.


Coercion
--------

Changing the representation of a value to match a given type is a
*coercion*, and is performed by the ``coerce`` form.

.. code-block:: lisp

   (setq a (+ a (coerce b '(signed-byte 32))))

This takes ``b`` and coerces it to the given type (``(signed-byte
32)``) for addition. If ``b`` is already known to have this type,
nothing happens. If it is (for example) an ``(unsigned-byte 16)`` or a
``(signed-byte 16)`` it will be sign-extended to the required width.
In the former case, the number will be padded by 0 bits; in the
latter, it will be padded by whatever the value of the highest-order
bit (bit 15 in this case) of the value is, so that if the value
happens to be negative the coerced value is *also* negative, with the
same value.

Coercing will signal a ``PRECISION-MISMATCH`` warning if there is a
danger that the value being coerced can't be guaranteed to fit into
the type requested.

.. note::

   The behaviour of ``coerce`` involves specific changes in
   representation that are left unspecified in Common Lisp. See the
   implementation note on :ref:`implementation-coercion` for details
   if required.

Casting
-------

Asserting the type of a value is a *cast*, and is performed by the
``the`` form.

.. code-block:: lisp

   (let ((Iimm (the '(signed-byte 12) (bref instr 31 :end 20))))
     ...)

This takes the 12 bits returned by ``bref`` and checks whether they
can be interpreted as a signed 12-bit number -- which they can, so the
operation succeeds. Had we done:

.. code-block:: lisp

   (the '(unsigned-byte 12) (bref instr 31 :end 20))

we would also have succeeded, but:

.. code-block:: lisp

   (the '(signed-byte 32) (bref instr 31 :end 20))

would have failed, because the 12 bits returned by ``bref`` can't be
interpreted as a 32-bit signed number: there aren't enough bits, so
the representation would have to be changed, which is something
``coerce`` can do but ``the`` can't.

This is the critical difference between a coercion and a cast: the
former changes the representation of the value to match the requested
type if possible, while the latter just checks whether the existing
representation can be interpreted as a value of the requested type.


How to choose between a coercion and a cast
-------------------------------------------

It might sound like ``coerce`` is a lot more useful than ``the``, but
there are circumstances where ``the`` is exactly the tool we need. To
see why, let's substitute ``coerce`` for ``the`` in our example above:

.. code-block:: lisp

   (let ((Iimm (coerce (bref instr 31 :end 20) '(signed-byte 12))))
     ...)

Verilisp would examine the value, determine that it has type
``(unsigned-byte 12)`` (which is how ``bref`` returns its values), and
coerce it to a ``(signed-byte 12)`` by extracting the bottom 11 bits
and composing them with a leading 0 bit indicating a positive number
(because it was originally unsigned) while signalling a warning about
possible loss of precision because of the truncation.

Of course there are circumstances where this is exactly the behaviour
we want -- but probably not in this case, where we just want to tell
Verilisp that a particular set of bits represents a signed number.
``the`` is the correct tool for this job.


.. note::

   As a rule of thumb, use ``the`` when extracting a bit-pattern from
   a larger set of bits, and ``coerce`` when changing the type of an
   entire variable.
