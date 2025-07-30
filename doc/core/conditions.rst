.. _core-conditions:


Conditions
==========

Conditions relating to syntax
-----------------------------

+-----------------------+---------------------------------------+
| Condition             | Meaning                               |
+=======================+=======================================+
| ``syntax-error``      | The Lisp code doesn't meet Verilisp's |
|                       | requirements, which are somewhat more |
|                       | limiting than Common Lisp             |
+-----------------------+---------------------------------------+
| ``unknown-form``      | A form has been encountered that's    |
|                       | recognised by Verilisp. Typically     |
|                       | is either a piece of Lisp that's not  |
|                       | part of Verilisp, or an unrecognised  |
|                       | macro that's been left unexpanded     |
+-----------------------+---------------------------------------+
| ``not-synthesisable`` | Some other condition has stopped      |
|                       | Verilisp from synthesising the code   |
+-----------------------+---------------------------------------+


Conditions relating to variable declaration
-------------------------------------------

+------------------------------+--------------------------------------+
| Condition                    | Meaning                              |
+==============================+======================================+
| ``unknown-variable``         | A variable is referenced when not    |
|                              | in scope                             |
+------------------------------+--------------------------------------+
| ``duplicate-variable``       | The same variable name appears twice |
|                              | in the same ``let`` or ``let*``      |
+------------------------------+--------------------------------------+
| ``unrecognised-declaration`` | An entry in a ``declare`` form       |
|                              | isn't recognised                     |
+------------------------------+--------------------------------------+


Conditions relating to type-checking and inference
--------------------------------------------------

+-----------------------------+-----------------------------------------+
| Condition                   | Meaning                                 |
+=============================+=========================================+
| ``type-inferred``           | A type has been inferred for a variable |
+-----------------------------+-----------------------------------------+
| ``type-mismatch``           | A value is being assigned to a          |
|                             | variable with an incompatible type      |
+-----------------------------+-----------------------------------------+
| ``value-mismatch``          | A given value isn't usable in the       |
|                             | context in which it appears             |
+-----------------------------+-----------------------------------------+
| ``representation-mismatch`` | An explicit representation has been     |
|                             | given that's incompatible with the      |
|                             | representation inferred                 |
+-----------------------------+-----------------------------------------+
| ``direction-mismatch``      | An explicit direction has been given    |
|                             | for a module argument that's            |
|                             | incompatible with the direction         |
|                             | inferred from the argument's usage      |
+-----------------------------+-----------------------------------------+
| ``coercion-mismatch``       | A value can't be coerced to the         |
|                             | given type                              |
+-----------------------------+-----------------------------------------+
| ``precision-mismatch``      | A value is being assigned to a          |
|                             | variable with a narrower type           |
+-----------------------------+-----------------------------------------+
| ``shape-mismatch``          | An index given to an array doesn't      |
|                             | match its shape                         |
+-----------------------------+-----------------------------------------+
| ``not-static``              | An operation needs a value that's       |
|                             | statically known                        |
+-----------------------------+-----------------------------------------+


Conditions relating to state machines
-------------------------------------

+---------------------+----------------------------------------------+
| Condition           | Meaning                                      |
+=====================+==============================================+
| ``unknown-state``   | A ``go`` form is targeting an unknown state  |
+---------------------+----------------------------------------------+
| ``duplicate-state`` | The same state label appears twice in        |
|                     | a state machine (possibly a surrounding one) |
+---------------------+----------------------------------------------+


Conditions relating to module declarations
------------------------------------------

+--------------------+---------------------------------------+
| Condition          | Meaning                               |
+====================+=======================================+
| ``unknown-module`` | The given module isn't defined        |
+--------------------+---------------------------------------+
| ``not-importable`` | A module import is being passed the   |
|                    | wrong arguments, or not being passed  |
|                    | non-optional ones                     |
+--------------------+---------------------------------------+
