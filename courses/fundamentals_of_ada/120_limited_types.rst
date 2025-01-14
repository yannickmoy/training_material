***************
Limited Types
***************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

-------
Views
-------

* Specify how values and objects may be manipulated
* Are implicit in much of the language semantics

   - Constants are just variables without any assignment view
   - Task types, protected types implicitly disallow assignment
   - Mode :ada:`in` formal parameters disallow assignment

.. code:: Ada

   Variable : Integer := 0;
   ...
   -- P's view of X prevents modification
   procedure P( X :  in  Integer ) is
   begin
       ...
   end P;
   ...
   P( Variable );

-------------------------------
Limited Type Views' Semantics
-------------------------------

* Prevents copying via predefined assignment

   - Disallows assignment between objects
   - Must make your own `copy` procedure if needed

   .. code:: Ada

      type File is limited ...
      ...
      F1, F2 : File;
      ...
      F1 := F2; -- compile error

* Prevents incorrect comparison semantics

   - Disallows predefined equality operator
   - Make your own equality function `=` if needed

-------------------------------
Inappropriate Copying Example
-------------------------------

.. code:: Ada

   type File is ...
   F1, F2 : File;
   ...
   Open (F1);
   Write ( F1, "Hello" );
   -- What is this assignment really trying to do?
   F2 := F1;

-----------------------------
Intended Effects of Copying
-----------------------------

.. code:: Ada

   type File is ...
   F1, F2 : File;
   ...
   Open (F1);
   Write ( F1, "Hello" );
   Copy (Source => F1, Target => F2);

==============
Declarations
==============

----------
Examples
----------

.. include:: examples/120_limited_types/declarations.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/120_limited_types.html#declarations`

---------------------------
Limited Type Declarations
---------------------------

* Syntax

   - Additional keyword limited added to record type declaration

   .. code:: Ada

      type defining_identifier is limited record
          component_list
      end record;

* Are always record types unless also private

   - More in a moment...

---------------------------
Approximate Analog In C++
---------------------------

.. code:: C++

   class Stack {
   public:
     Stack();
     void Push (int X);
     void Pop (int& X);
     ...
   private:
     ...
     // assignment operator hidden
     Stack& operator= (const Stack& other);
   }; // Stack

-------------------
Spin Lock Example
-------------------

.. code:: Ada

   with Interfaces;
   package Multiprocessor_Mutex is
     -- prevent copying of a lock
     type Spin_Lock is limited record
       Flag : Interfaces.Unsigned_8;
     end record;
     procedure Lock  (This : in out Spin_Lock);
     procedure Unlock  (This : in out Spin_Lock);
     pragma Inline (Lock, Unlock);
   end Multiprocessor_Mutex;

-----------------------------
Parameter Passing Mechanism
-----------------------------

* Always "by-reference" if explicitly limited

   - Necessary for various reasons (:ada:`task` and :ada:`protected` types, etc)
   - Advantageous when required for proper behavior

* By definition, these subprograms would be called concurrently

   - Cannot operate on copies of parameters!

.. code:: Ada

   procedure Lock  (This : in out Spin_Lock);
   procedure Unlock (This : in out Spin_Lock);

-------------------------------------
Composites with Limited Types
-------------------------------------

* Composite containing a limited type becomes limited as well

   * Example: Array of limited elements

      - Array becomes a limited type

   * Prevents assignment and equality loop-holes

.. code:: Ada

   declare
     -- if we can't copy component S, we can't copy User_Type
     type User_Type is record -- limited because S is limited
       S : File;
       ...
     end record;
     A, B : User_Type;
   begin
     A := B;  -- not legal since limited
     ...
   end;

------
Quiz
------

.. include:: quiz/limited_syntax/quiz.rst

------
Quiz
------

.. include:: quiz/limited_operators/quiz.rst

------
Quiz
------

.. code:: Ada

   package P is
      type T is limited null record;
      type R is record
         F1 : Integer;
         F2 : T;
      end record;
   end P;

   with P;
   procedure Main is
      T1, T2 : P.T;
      R1, R2 : P.R;
   begin

Which assignment is legal?

   A. ``T1    := T2;``
   B. ``R1    := R2;``
   C. :answermono:`R1.F1 := R2.F1;`
   D. ``R2.F2 := R2.F2;``

.. container:: animate

   Explanations

   A. :ada:`T1` and :ada:`T2` are :ada:`limited types`
   B. :ada:`R1` and :ada:`R2` contain :ada:`limited` types so they are also :ada:`limited`
   C. Theses components are not :ada:`limited` types
   D. These components are of a :ada:`limited` type

=================
Creating Values
=================

----------
Examples
----------

.. include:: examples/120_limited_types/creating_values.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/120_limited_types.html#creating-values`

-----------------
Creating Values
-----------------

* Initialization is not assignment (but looks like it)!
* Via **limited constructor functions**

   - Functions returning values of limited types

* Via an **aggregate**

    - :dfn:`limited aggregate` when used for a :ada:`limited` type

.. code:: Ada

   type Spin_Lock is limited record
     Flag : Interfaces.Unsigned_8;
   end record;
   ...
   Mutex : Spin_Lock := (Flag => 0); -- limited aggregate

-------------------------------
Limited Constructor Functions
-------------------------------

.. container:: columns

 .. container:: column

    * Allowed wherever limited aggregates are allowed
    * More capable (can perform arbitrary computations)
    * Necessary when limited type is also private

       - Users won't have visibility required to express aggregate contents

 .. container:: column

    .. code:: Ada

       function F return Spin_Lock
       is
       begin
         ...
         return (Flag => 0);
       end F;

---------------------------------------
Writing Limited Constructor Functions
---------------------------------------

* Remember - copying is not allowed

.. code:: Ada

   function F return Spin_Lock is
     Local_X : Spin_Lock;
   begin
     ...
     return Local_X; -- this is a copy - not legal
      -- (also illegal because of pass-by-reference)
   end F;

.. code:: Ada

   Global_X : Spin_Lock;
   function F return Spin_Lock is
   begin
     ...
     -- This is not legal staring with Ada2005
     return Global_X; -- this is a copy
   end F;

-------------------
 "Built In-Place"
-------------------

* Limited aggregates and functions, specifically
* No copying done by implementation

   - Values are constructed in situ

.. code:: Ada

   Mutex : Spin_Lock := (Flag => 0);

.. code:: Ada

   function F return Spin_Lock is
   begin
     return (Flag => 0);
   end F;

------
Quiz
------

.. include:: quiz/limited_constructor_syntax/quiz.rst

------
Quiz
------

.. code:: Ada

   package P is
      type T is limited record
         F1 : Integer;
         F2 : Character;
      end record;
      Zero : T := (0, ' ');
      One : constant T := (1, 'a');
      Two : T;
      function F return T;
   end P;

Which is a correct completion of F?

A. :answermono:`return (3, 'c');`
B. | ``Two := (2, 'b');``
   | ``return Two;``
C. ``return One;``
D. ``return Zero;``

.. container:: animate

   :ada:`A` contains an "in-place" return. The rest all rely on
   other objects, which would require an (illegal) copy.

============================
Extended Return Statements
============================

----------
Examples
----------

.. include:: examples/120_limited_types/extended_return_statements.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/120_limited_types.html#extended-return-statements`

-------------------------------------
Function Extended Return Statements
-------------------------------------

.. admonition:: Language Variant

   Ada 2005

* :dfn:`Extended return`
* Result is expressed as an object
* More expressive than aggregates
* Handling of unconstrained types
* Syntax (simplified):

   .. code:: Ada

      return identifier : subtype [:= expression];

      return identifier : subtype
      [do
         sequence_of_statements ...
       end return];

----------------------------------
Extended Return Statements Example
----------------------------------

   .. code:: Ada

       --  Implicitly limited array
       type Spin_Lock_Array (Positive range <>) of Spin_Lock;

       function F return Spin_Lock_Array is
       begin
         return Result : Spin_Lock_Array (1 .. 10) do
           ...
         end return;
       end F;

------------------------------------
Expression / Statements Are Optional
------------------------------------

.. admonition:: Language Variant

   Ada 2005

* Without sequence (returns default if any)

   .. code:: Ada

      function F return Spin_Lock is
      begin
        return Result : Spin_Lock;
      end F;

* With sequence

   .. code:: Ada

      function F return Spin_Lock is
        X : Interfaces.Unsigned_8;
      begin
        --  compute X ...
        return Result : Spin_Lock := (Flag => X);
      end F;

-----------------------
Statements Restrictions
-----------------------

.. admonition:: Language Variant

   Ada 2005

* **No** nested extended return
* **Simple** return statement **allowed**

   - **Without** expression
   - Returns the value of the **declared object** immediately

.. code:: Ada

   function F return Spin_Lock is
   begin
     return Result : Spin_Lock do
       if Set_Flag then
         Result.Flag := 1;
         return;  --  returns 'Result'
       end if;
       Result.Flag := 0;
     end return; --  Implicit return
   end F;

------
Quiz
------

.. include:: quiz/limited_extended_return/quiz.rst

=====================================
Combining Limited and Private Views
=====================================

----------
Examples
----------

.. include:: examples/120_limited_types/combining_limited_and_private_views.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/120_limited_types.html#combining-limited-and-private-views`

-----------------------
Limited Private Types
-----------------------

* A combination of :ada:`limited` and :ada:`private` views

   - No client compile-time visibility to representation
   - No client assignment or predefined equality

* The typical design idiom for :ada:`limited` types
* Syntax

   - Additional reserved word :ada:`limited` added to :ada:`private` type declaration

   .. code:: Ada

      type defining_identifier is limited private;

------------------------------------
Limited Private Type Rationale (1)
------------------------------------

.. code:: Ada

   package Multiprocessor_Mutex is
     -- copying is prevented
     type Spin_Lock is limited record
       -- but users can see this!
       Flag : Interfaces.Unsigned_8;
     end record;
     procedure Lock (This : in out Spin_Lock);
     procedure Unlock (This : in out Spin_Lock);
     pragma Inline (Lock, Unlock);
   end Multiprocessor_Mutex;

------------------------------------
Limited Private Type Rationale (2)
------------------------------------

.. code:: Ada

   package MultiProcessor_Mutex is
     -- copying is prevented AND users cannot see contents
     type Spin_Lock is limited private;
     procedure Lock (The_Lock : in out Spin_Lock);
     procedure Unlock (The_Lock : in out Spin_Lock);
     pragma Inline (Lock, Unlock);
   private
     type Spin_Lock is ...
   end MultiProcessor_Mutex;

----------------------------------
Limited Private Type Completions
----------------------------------

* Clients have the partial view as :ada:`limited` and :ada:`private`
* The full view completion can be any kind of type
* Not required to be a record type just because the partial view is limited

.. code:: Ada

   package P is
     type Unique_ID_T is limited private;
     ...
   private
     type Unique_ID_T is range 1 .. 10;
   end P;

-----------------------------
Write-Only Register Example
-----------------------------

.. code:: Ada

   package Write_Only is
     type Byte is limited private;
     type Word is limited private;
     type Longword is limited private;
     procedure Assign (Input : in Unsigned_8;
                       To    : in out Byte);
     procedure Assign (Input : in Unsigned_16;
                       To    : in out Word);
     procedure Assign (Input : in Unsigned_32;
                       To    : in out Longword);
   private
     type Byte is new Unsigned_8;
     type Word is new Unsigned_16;
     type Longword is new Unsigned_32;
   end Write_Only;

--------------------------------
Explicitly Limited Completions
--------------------------------

* Completion in Full view includes word :ada:`limited`
* Optional
* Requires a record type as the completion

.. code:: Ada

   package MultiProcessor_Mutex is
     type Spin_Lock is limited private;
     procedure Lock (This : in out Spin_Lock);
     procedure Unlock (This : in out Spin_Lock);
   private
     type Spin_Lock is limited -- full view is limited as well
       record
         Flag : Interfaces.Unsigned_8;
       end record;
   end MultiProcessor_Mutex;

-------------------------------------------
Effects of Explicitly Limited Completions
-------------------------------------------

* Allows no internal copying too
* Forces parameters to be passed by-reference

.. code:: Ada

   package MultiProcessor_Mutex is
     type Spin_Lock is limited private;
     procedure Lock (This : in out Spin_Lock);
     procedure Unlock (This : in out Spin_Lock);
   private
     type Spin_Lock is limited record
       Flag : Interfaces.Unsigned_8;
     end record;
   end MultiProcessor_Mutex;

---------------------------------
Automatically Limited Full View
---------------------------------

* When other limited types are used in the representation
* Recall composite types containing limited types are :ada:`limited` too

.. code:: Ada

   package Foo is
      type Legal is limited private;
      type Also_Legal is limited private;
      type Not_Legal is private;
      type Also_Not_Legal is private;
   private
      type Legal is record
         S : A_Limited_Type;
      end record;
      type Also_Legal is limited record
         S : A_Limited_Type;
      end record;
      type Not_Legal is limited record
         S : A_Limited_Type;
      end record;
      type Also_Not_Legal is record
         S : A_Limited_Type;
      end record;
   end Foo;

.. container:: speakernote

   Also_Legal adds "limited" to the full view
   Not_Legal puts more limitations on full view than partial view
   Also_Not_Legal never shows the client that S is limited

------
Quiz
------

.. include:: quiz/limited_private/quiz.rst

------
Quiz
------

.. container:: latex_environment footnotesize

 .. container:: columns

  .. container:: column

   .. code:: Ada

      package P is
         type L1_T is limited private;
         type L2_T is limited private;
         type P1_T is private;
         type P2_T is private;
      private
         type L1_T is limited record
            Field : Integer;
         end record;
         type L2_T is record
            Field : Integer;
         end record;
         type P1_T is limited record
            Field : L1_T;
         end record;
         type P2_T is record
            Field : L2_T;
         end record;
      end P;

  .. container:: column

   What will happen when the above code is compiled?

   A. :answer:`Type P1_T will generate a compile error`
   B. Type P2_T will generate a compile error
   C. Both type P1_T and type P2_T will generate compile errors
   D. The code will compile successfully

   .. container:: animate

      The full definition of type :ada:`P1_T` adds additional
      restrictions, which is not allowed. Although :ada:`P2_T`
      contains a component whose visible view is :ada:`limited`,
      the internal view is not :ada:`limited` so :ada:`P2_T` is
      not :ada:`limited`.

========
Lab
========

.. include:: labs/120_limited_types.lab.rst

=========
Summary
=========

---------
Summary
---------

* Limited view protects against improper operations

   - Incorrect equality semantics
   - Copying via assignment

* Enclosing composite types are :ada:`limited` too

   - Even if they don't use keyword :ada:`limited` themselves

* Limited types are always passed by-reference
* Extended return statements work for any type

   - Ada 2005 and later

* Don't make types :ada:`limited` unless necessary

   - Users generally expect assignment to be available
