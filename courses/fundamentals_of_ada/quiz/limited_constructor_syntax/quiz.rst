..

    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type T is limited record
       I : Integer;
    end record;

Which piece(s) of code is(are) a legal constructor for :ada:`T`?

A. | ``function F return T is``
   | ``begin``
   |    ``return T (I => 0);``
   | ``end F;``
B. | :answermono:`function F return T is`
   |    :answermono:`Val : Integer := 0;`
   | :answermono:`begin`
   |    :answermono:`return (I => Val);`
   | :answermono:`end F;`
C. | ``function F return T is``
   |    ``Ret : T := (I => 0);``
   | ``begin``
   |    ``return Ret;``
   | ``end F;``
D. | ``function F return T is``
   | ``begin``
   |    ``return (0);``
   | ``end F;``
