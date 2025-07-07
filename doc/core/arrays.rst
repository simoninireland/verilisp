.. _core-arrays:

Arrays
======


Instanciation
-------------

Arrays are constructed as the values of variables in ``let`` forms
using the `make-instance`` function.

.. code-block:: lisp

   (let ((a (make-array '(16) :element-type (unsigned-byte 8)))
	 (b 0))
      (setq b (aref a 0)))

The first argument to ``make-array`` is a shape list. The leading
quote is optional.

.. note::

   At the moment only one-dimensional arrays are supported.

The keyword arguments are:

``:element-type`` *type*
  The type of elements.

``:initial-element`` *val*
  The initial value assigned to each element.

``:initial-contents`` *data*
  The initial contents of all the elements (see below).


Initial contents
----------------

There are two ways to specify initial contents: as an inline list of
values, or as the contents of a file.

The inline form looks like:

.. code-block:: lisp

   (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8))

The initial quote before the data is optional, and is supported for
Lisp compatibility.

The file form looks like:

.. code-block:: lisp

   (make-array '(8) :type (fixed-width-unsigned 8)
		    :initial-contents (:file "test.bin" :radix 16))

The filename can be absolute or relative to the current directory. The
radix is optional and defaults to 10. The file should contain a list
of numbers matching the type of the array, whitespace-separated, and
are interpreted using the given radix. They can be split on different
lines is desired: any such formatting is ignored.


.. _core-arrays-element-access:

Element access
--------------

To access an element of an array use the ``aref`` form:

.. code-block:: lisp

   (let ((a (make-array '(16)))
	 (b 0))
      (setq b (aref a 0)))

The same form works as a generalised place for :ref:`core-assignment`:

.. code-block:: lisp

   (let ((a (make-array '(16))))
      (setf (aref a 0) 12))
