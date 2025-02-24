*************
Basic Types
*************

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

================
Introduction
================

----------------
Ada Type Model
----------------

* :dfn:`Static` Typing

   - Object type **cannot change**

* :dfn:`Strong` Typing

   - By **name**
   - **Compiler-enforced** operations and values
   - **Explicit** conversion for "related" types
   - **Unchecked** conversions possible

---------------
Strong Typing
---------------

* Definition of :dfn:`type`

   - Applicable **values**
   - Applicable :dfn:`primitive` **operations**

* Compiler-enforced

   - **Check** of values and operations
   - Easy for a computer
   - Developer can focus on **earlier** phase: requirement

----------------------
A Little Terminology
----------------------

* **Declaration** creates a **type name**

   .. code:: Ada

      type <name> is <type definition>;

* **Type-definition** defines its structure

   - Characteristics, and operations
   - Base "class" of the type

   .. code:: Ada

      type Type_1 is digits 12; -- floating-point
      type Type_2 is range -200 .. 200; -- signed integer
      type Type_3 is mod 256; -- unsigned integer

* :dfn:`Representation` is the memory-layout of an **object** of the type

-------------------------
Ada "Named Typing"
-------------------------

* **Name** differentiate types
* Structure does **not**
* Identical structures may **not** be interoperable

   .. code:: Ada

      type Yen is range 0 .. 100_000_000;
      type Ruble is range 0 .. 100_000_000;
      Mine : Yen;
      Yours : Ruble;
      ...
      Mine := Yours; -- not legal

---------------------
Categories of Types
---------------------

.. image:: types_tree.png

--------------
Scalar Types
--------------

* Indivisible: No components
* **Relational** operators defined (``<``,  ``=``, ...)

    - **Ordered**

* Have common **attributes**
* **Discrete** Types

  - Integer
  - Enumeration

* **Real** Types

  - Floating-point
  - Fixed-point

----------------
Discrete Types
----------------

* **Individual** ("discrete") values

   - 1, 2, 3, 4 ...
   - Red, Yellow, Green

* Integer types

   - Signed integer types
   - Modular integer types

      * Unsigned
      * **Wrap-around** semantics
      * Bitwise operations

* Enumeration types

   - Ordered list of **logical** values

-----------
Attributes
-----------

* Functions *associated* with a type

   - May take input parameters

* Some are language-defined

    - *May* be implementation-defined
    - **Built-in**
    - Cannot be user-defined
    - Cannot be modified

* See RM K.2 *Language-Defined Attributes*
* Syntax

  .. code:: Ada

    Type_Name'Attribute_Name;
    Type_Name'Attribute_With_Param (Param);

* **'** often named *tick*

========================
Discrete Numeric Types
========================

-----------
Examples
-----------

.. include:: examples/030_basic_types/discrete_numeric_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/030_basic_types.html#discrete-numeric-types`

----------------------
Signed Integer Types
----------------------

* Range of signed **whole** numbers

   - Symmetric about zero (-0 = +0)

* Syntax

   .. code:: Ada

      type <identifier> is range  <lower> .. <upper>;

* Implicit numeric operators

   .. code:: Ada

      -- 12-bit device
      type Analog_Conversions is range 0 .. 4095;
      Count : Analog_Conversions;
      ...
      begin
         ...
         Count := Count + 1;
         ...
      end;

--------------------------------
Specifying Integer Type Bounds
--------------------------------

* Must be **static**

   - Compiler selects **base type**
   - Hardware-supported integer type
   - Compilation **error** if not possible

--------------------------
Predefined Integer Types
--------------------------

* :ada:`Integer` **>= 16 bits** wide
* Other **probably** available

   - :ada:`Long_Integer`, :ada:`Short_Integer`, etc.
   - Guaranteed ranges: :ada:`Short_Integer` ``<=`` :ada:`Integer` ``<=`` :ada:`Long_Integer`
   - Ranges are all **implementation-defined**

* Portability not guaranteed

   - But may be difficult to avoid

--------------------------------
Operators for Any Integer Type
--------------------------------

* By increasing precedence

   :relational operator: :ada:`= | /= | < | <= | > | >=`
   :binary adding operator: :ada:`+ | -`
   :unary adding operator: :ada:`+ | -`
   :multiplying operator: :ada:`* | / | mod | rem`
   :highest precedence operator: :ada:`** | abs`

* *Note*: for exponentiation :ada:`**`

   - Result will be :ada:`Integer`
   - So power **must** be :ada:`Integer` ``>= 0``

* Division by zero |rightarrow| :ada:`Constraint_Error`

-----------------
Integer Overflows
-----------------

* Finite binary representation
* Common source of bugs

.. code:: Ada

   K : Short_Integer := Short_Integer'Last;
   ...
   K := K + 1;

    2#0111_1111_1111_1111#  = (2**16)-1

   +                    1

   =======================
    2#1000_0000_0000_0000#  = -32,768

-------------------------------
Integer Overflow: Ada vs others
-------------------------------

* Ada

   - :ada:`Constraint_Error` standard exception
   - Incorrect numerical analysis

* Java

   - Silently **wraps** around (as the hardware does)

* C/C++

   - **Undefined** behavior (typically silent wrap-around)

---------------
Modular Types
---------------

* Integer type
* **Unsigned** values
* Adds operations and attributes

    * Typically **bit-wise** manipulation

* Syntax

   .. code:: Ada

      type <identifier> is mod <modulus>;

* Modulus must be **static**
* Resulting range is  0 .. modulus-1

   .. code:: Ada

      type Unsigned_Word is mod 2**16; -- 16 bits, 0..65535
      type Byte is mod 256;            -- 8 bits, 0..255

------------------------
Modular Type Semantics
------------------------

* Standard :ada:`Integer` operators
* **Wraps-around** in overflow

   - Like other languages' unsigned types
   - Attributes :ada:`'Pred` and :ada:`'Succ`

* Additional bit-oriented operations are defined

   - :ada:`and`, :ada:`or`, :ada:`xor`, :ada:`not`
   - **Bit shifts**
   - Values as **bit-sequences**

--------------------------
Predefined Modular Types
--------------------------

* In :ada:`Interfaces` package

   - Need **explicit** import

* **Fixed-size** numeric types
* Common name **format**

   - :ada:`Unsigned_n`
   - :ada:`Integer_n`

.. code:: Ada

   type Integer_8 is range -2 ** 7 .. 2 ** 7 - 1;
   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   ...
   type Unsigned_8 is mod 2 ** 8;
   type Unsigned_16 is mod 2 ** 16;

---------------------------------------------
Integer Type (Signed and Modular) Literals
---------------------------------------------

* **Must not** contain a **fractional** part
* **No** silent promotion/demotion
* **Conversion** can be used

.. code:: Ada

   type Counter_T is range 0 .. 40_000; -- integer type
   OK : Counter_T := 0; -- Right type, legal
   Bad : Counter_T := 0.0 ; -- Promotion, compile error
   Legal : Counter_T := Counter_T (0.0); -- Conversion, legal

-----------------------------------
String Attributes For All Scalars
-----------------------------------

* :ada:`T'Image( input )`

   - Converts :ada:`T` |rightarrow| :ada:`String`

* :ada:`T'Value( input )`

   - Converts :ada:`String` |rightarrow| :ada:`T`

.. code:: Ada

   Number : Integer := 12345;
   Input  : String( 1 .. N );
   ...
   Put_Line( Integer'Image(Number) );
   ...
   Get( Input );
   Number := Integer'Value( Input );

----------------------------------
Range Attributes For All Scalars
----------------------------------

* :ada:`T'First`

  - First (**smallest**) value of type :ada:`T`

* :ada:`T'Last`

  - Last (**greatest**) value of type :ada:`T`

* :ada:`T'Range`

  - Shorthand for :ada:`T'First .. T'Last`

.. code:: Ada

   type Signed_T is range -99 .. 100;
   Smallest : Signed_T := Signed_T'First; -- -99
   Largest  : Signed_T := Signed_T'Last;  -- 100

-------------------------------------
Neighbor Attributes For All Scalars
-------------------------------------

* :ada:`T'Pred (Input)`

   - Predecessor of specified value
   - :ada:`Input` type must be :ada:`T`

* :ada:`T'Succ (Input)`

   - Successor of specified value
   - :ada:`Input` type must be :ada:`T`

.. code:: Ada

   type Signed_T is range -128 .. 127;
   type Unsigned_T is mod 256;
   Signed   : Signed_T := -1;
   Unsigned : Unsigned_T := 0;
   ...
   Signed := Signed_T'Succ( Signed ); -- Signed = 0
   ...
   Unsigned := Unsigned_T'Pred( Unsigned ); -- Signed = 255

------------------------------------
Min/Max Attributes For All Scalars
------------------------------------

* :ada:`T'Min (Value_A, Value_B)`

  - **Lesser** of two :ada:`T`

* :ada:`T'Max (Value_A, Value_B)`

  - **Greater** of two :ada:`T`

.. code:: Ada

   Safe_Lower : constant := 10;
   Safe_Upper : constant := 30;
   C : Integer := 15;
   ...
   C := Integer'Max (Safe_Lower, C - 1);
   ...
   C := Integer'Min (Safe_Upper, C + 1);

------
Quiz
------

What happens when you try to compile/run this code?

.. code:: Ada

   C1 : constant := 2 ** 1024;
   C2 : constant := 2 ** 1024 + 10;
   C3 : constant := C1 - C2;
   V  : Integer := C1 - C2;

A. Compile error
B. Run-time error
C. :answer:`V is assigned to -10`
D. Unknown - depends on the compiler

.. container:: animate

   Explanations

   - 2:superscript:`1024` too big for most run-times BUT
   - :ada:`C1`, :ada:`C2`, and :ada:`C3` are named numbers, not typed constants

      - Compiler uses unbounded precision for named numbers
      - Large intermediate representation does not get stored in object code

   - For assignment to :ada:`V`, subtraction is computed by compiler

      - :ada:`V` is assigned the value -10

====================
Enumeration Types
====================

-----------
Examples
-----------

.. include:: examples/030_basic_types/discrete_enumeration_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/030_basic_types.html#discrete-enumeration-types`

-------------------
Enumeration Types
-------------------

* Enumeration of **logical** values

    - Integer value is an implementation detail

* Syntax

   .. code:: Ada

      type <identifier> is ( <identifier-list> ) ;

* Literals

   - Distinct, ordered
   - Can be in **multiple** enumerations

   .. code:: Ada

      type Colors is (Red, Orange, Yellow, Green, Blue, Violet);
      type Stop_Light is (Red, Yellow, Green);
      ...
      -- Red both a member of Colors and Stop_Light
      Shade : Colors := Red;
      Light : Stop_Light := Red;

-----------------------------
Enumeration Type Operations
-----------------------------

* Assignment, relationals
* **Not** numeric quantities

   - *Possible* with attributes
   - Not recommended

.. code:: Ada

   type Directions is ( North, South, East, West );
   type Days is ( Mon, Tue, Wed, Thu, Fri, Sat, Sun );
   Heading : Directions;
   Today, Tomorrow : Days;
   ...
   Today := Mon;
   Today := North; -- compile error
   Heading := South;
   Heading := East + 1; -- compile error
   if Today < Tomorrow then ...

---------------
Character Types
---------------

* Literals

   - Enclosed in single quotes eg. :ada:`'A'`
   - Case-sensitive

* **Special-case** of enumerated type

   - At least one character enumeral

* System-defined :ada:`Character`
* Can be user-defined

   .. code:: Ada

      type EBCDIC is ( nul, ..., 'a' , ..., 'A', ..., del );
      Control : EBCDIC := 'A';
      Nullo : EBCDIC := nul;

-------------------------------
Language-Defined Type Boolean
-------------------------------

* Enumeration

   .. code:: Ada

      type Boolean is ( False, True );

* Supports assignment, relational operators, attributes

   .. code:: Ada

      A : Boolean;
      Counter : Integer;
      ...
      A := (Counter = 22);

* Logical operators :ada:`and`, :ada:`or`, :ada:`xor`, :ada:`not`

   .. code:: Ada

      A := B or ( not C ); -- For A, B, C boolean

------------------------------------
Why Boolean Isn't Just An Integer?
------------------------------------

.. container:: columns

 .. container:: column

    * Example: Real-life error

       - HETE-2 satellite **attitude control** system software (ACS)
       - Written in **C**

    * Controls four "solar paddles"

        - Deployed after launch

 .. container:: column

    .. image:: hete-2_satellite.jpeg

------------------------------------
Why Boolean Isn't Just An Integer!
------------------------------------

* **Initially** variable with paddles' state

    - Either **all** deployed, or **none** deployed

* Used :C:`int` as a boolean

   .. code:: C

      if (rom->paddles_deployed == 1)
        use_deployed_inertia_matrix();
      else
        use_stowed_inertia_matrix();

* Later :C:`paddles_deployed` became a **4-bits** value

    - One bit per paddle
    - :C:`0` |rightarrow| none deployed, :C:`0xF` |rightarrow| all deployed

* Then, :C:`use_deployed_inertia_matrix()` if only first paddle is deployed!
* Better: boolean function :C:`paddles_deployed()`

    - Single line to modify

---------------------------------------
Boolean Operators' Operand Evaluation
---------------------------------------

* Evaluation order **not specified**
* May be needed

  - Checking value **before** operation
  - Dereferencing null pointers
  - Division by zero

 .. code:: Ada

    if Divisor /= 0 and K / Divisor = Max then ... -- Problem!

-----------------------------
Short-Circuit Control Forms
-----------------------------

* **Short-circuit** |rightarrow| **fixed** evaluation order
* Left-to-right
* Right only evaluated **if necessary**

   - :ada:`and then`: if left is :ada:`False`, skip right

     .. code:: Ada

        Divisor /= 0 and then K / Divisor = Max

   - :ada:`or else`: if left is :ada:`True`, skip right

     .. code:: Ada

        Divisor = 0 or else K / Divisor = Max

------
Quiz
------

.. code:: Ada

   type Enum_T is ( Able, Baker, Charlie );

Which statement will generate an error?

A. ``V1 :  Enum_T := Enum_T'Value ("Able");``
B. ``V2 :  Enum_T := Enum_T'Value ("BAKER");``
C. ``V3 :  Enum_T := Enum_T'Value (" charlie ");``
D. :answermono:`V4 : Enum_T := Enum_T'Value ("Able Baker Charlie");`

.. container:: animate

   Explanations

   A. Legal
   B. Legal - conversion is case-insensitive
   C. Legal - leading/trailing blanks are ignored
   D. :ada:`Value` tries to convert entire string, which will fail at run-time

============
Real Types
============

-----------
Examples
-----------

.. include:: examples/030_basic_types/real_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/030_basic_types.html#real-types`

------------
Real Types
------------

* Approximations to **continuous** values

  - 1.0, 1.1, 1.11, 1.111 ... 2.0, ...
  - Finite hardware |rightarrow| approximations

* Floating-point

  - **Variable** exponent
  - **Large** range
  - Constant **relative** precision

* Fixed-point

  - **Constant** exponent
  - **Limited** range
  - Constant **absolute** precision
  - Subdivided into Binary and Decimal

* Class focuses on floating-point

------------------------------------------
Real Type (Floating and Fixed) Literals
------------------------------------------

* **Must** contain a fractional part
* No silent promotion

.. code:: Ada

   type Phase is digits 8; -- floating-point
   OK : Phase := 0.0;
   Bad : Phase := 0 ; -- compile error

--------------------------------
Declaring Floating Point Types
--------------------------------

* Syntax

    .. code:: Ada

       type <identifier> is
           digits <expression> [range constraint];

  - *digits* |rightarrow| **minimum** number of significant digits
  - **Decimal** digits, not bits

* Compiler choses representation

  - From **available** floating point types
  - May be **more** accurate, but not less
  - If none available |rightarrow| declaration is **rejected**

---------------------------------
Predefined Floating Point Types
---------------------------------

* Type :ada:`Float` ``>= 6`` digits
* Additional implementation-defined types

   - :ada:`Long_Float` ``>= 11`` digits

* General-purpose
* Best to **avoid** predefined types

   - Loss of **portability**
   - Easy to avoid

-------------------------------
Floating Point Type Operators
-------------------------------

* By increasing precedence

   :relational operator: :ada:`= | /= | < | >= | > | >=`
   :binary adding operator: :ada:`+ | -`
   :unary adding operator: :ada:`+ | -`
   :multiplying operator: :ada:`* | /`
   :highest precedence operator: :ada:`** | abs`

* *Note* on floating-point exponentiation ``**``

   - Power must be :ada:`Integer`

      + Not possible to ask for root
      + :ada:`X**0.5` |rightarrow| :ada:`sqrt(x)`

--------------------------------
Floating Point Type Attributes
--------------------------------

* *Core* attributes

   .. code:: Ada

      type Real is digits N;  -- N static

   - :ada:`Real'Digits`

      + Number of digits **requested** (N)

   - :ada:`Real'Base'Digits`

      + Number of **actual** digits

   - :ada:`Real'Rounding (X)`

      + Integral value nearest to :ada:`X`
      + *Note* :ada:`Float'Rounding (0.5) = 1`

* Model-oriented attributes

   - Advanced machine representation of the floating-point type
   - Mantissa, strict mode

------
Quiz
------

What is the output of this code?

.. code:: Ada

   declare
      F : Float := 7.6;
      I : Integer := 10;
   begin
      F := Float ( Integer(F) / I );
      Put_Line ( Float'Image ( F ) );
   end;

A. 7.6
B. Compile Error
C. 8.0
D. :answer:`0.0`

.. container:: animate

   Explanations

   A. Result of :ada:`F := F / Float(I);`
   B. Result of :ada:`F := F / I;`
   C. Result of :ada:`F := Float (Integer (F)) / Float (I);`
   D. Integer value of :ada:`F` is 8. Integer result of dividing that by 10 is 0. Converting to float still gives us 0

===============
Miscellaneous
===============

-----------------------------
 Checked Type Conversions
-----------------------------

* Between "closely related" types

   - Numeric types
   - Inherited types
   - Array types

* Illegal conversions **rejected**

   - Unsafe **Unchecked_Conversion** available

* Functional syntax

   - Function named :ada:`Target_Type`
   - Implicitely defined
   - **Must** be explicitely called

.. code:: Ada

   Target_Float := Float (Source_Integer);

-------------
Default Value
-------------

.. admonition:: Language Variant

   Ada 2012

* Not defined by language for **scalars**
* Can be done with an **aspect clause**

  - Only during type declarations
  - :code:`<value>` must be static

   .. code:: Ada

      type Type_Name is <type_definition>
           with Default_Value => <value>;

* Example

   .. code:: Ada

      type Tertiary_Switch is (Off, On, Neither)
         with Default_Value => Neither;
      Implicit : Tertiary_Switch; -- Implicit = Neither
      Explicit : Tertiary_Switch := Neither;

-------------------------------
Simple Static Type Derivation
-------------------------------

* New type from an existing type

  - **Limited** form of inheritance: operations
  - **Not** fully OOP
  - More details later

* Strong type benefits

  - Only **explicit** conversion possible
  - eg. :code:`Meters` can't be set from a :code:`Feet` value

* Syntax

   .. code:: Ada

      type identifier is new Base_Type [<constraints>]

* Example

   .. code:: Ada

      type Measurement is digits 6;
      type Distance is new Measurement
            range 0.0 .. Measurement'Last;

==========
Subtypes
==========

----------
Examples
----------

.. include:: examples/030_basic_types/subtypes.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/080_expressions.html#subtypes`

----------
Subtype
----------

* May **constrain** an existing type
* Still the **same** type
* Syntax

   .. code:: Ada

      subtype Defining_Identifier is Type_Name [constraints];

   - :ada:`Type_Name` is an existing :ada:`type` or :ada:`subtype`

* If no constraint |rightarrow| type alias

-----------------
Subtype Example
-----------------

* Enumeration type with :ada:`range` constraint

   .. code:: Ada

      type Days is (Sun, Mon, Tues, Wed, Thurs, Fri, Sat);
      subtype Weekdays is Days range Mon .. Fri;
      Workday : Weekdays; -- type Days limited to Mon .. Fri

* Equivalent to **anonymous** subtype

   .. code:: Ada

      Same_As_Workday : Days range Mon .. Fri;

----------------------
Kinds of Constraints
----------------------

* Range constraints on discrete types

   .. code:: Ada

      subtype Positive is Integer range 1 .. Integer'Last;
      subtype Natural is Integer range 0 .. Integer'Last;
      subtype Weekdays is Days range Mon .. Fri;
      subtype Symmetric_Distribution is
          Float range -1.0 .. +1.0;

* Other kinds, discussed later

------------------------
Effects of Constraints
------------------------

* Constraints only on values

   .. code:: Ada

      type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      subtype Weekdays is Days range Mon .. Fri;
      subtype Weekend is Days range Sat .. Sun;

* Functionalities are **kept**

   .. code:: Ada

      subtype Positive is Integer range 1 .. Integer'Last;
      P : Positive;
      X : Integer := P; -- X and P are the same type

---------------------------------
Assignment Respects Constraints
---------------------------------

* RHS values must satisfy type constraints
* :ada:`Constraint_Error` otherwise

.. code:: Ada

   Q : Integer  := some_value;
   P : Positive := Q; -- runtime error if Q <= 0
   N : Natural  := Q; -- runtime error if Q < 0
   J : Integer  := P; -- always legal
   K : Integer  := N; -- always legal

---------------------------
Range Constraint Examples
---------------------------

.. code:: Ada

   subtype Proper_Subset is Positive range 1 .. 10;
   subtype Same_Constraints is Positive
       range 1 .. Integer'Last;
   subtype Letter is Character range 'A' .. 'z';
   subtype Upper_Case is Letter range 'A' .. 'Z';
   subtype Lower_Case is Letter range 'a' .. 'z';
   subtype Null_Range is Integer
       range 1 .. 0;  -- silly when hard-coded...
   -- evaluated when subtype defined, not when object declared
   subtype Dynamic is Integer range Lower .. Upper;

------
Quiz
------

.. code:: Ada

   type Enum_T is (Sat, Sun, Mon, Tue, Wed, Thu, Fri);
   subtype Enum_Sub_T is Enum_T range Mon .. Fri;

Which subtype definition is valid?

   A. ``subtype A is Enum_Sub_T range Enum_Sub_T'Pred (Enum_Sub_T'First) .. Enum_Sub_T'Last;``
   B. ``subtype B is range Sat .. Mon;``
   C. :answermono:`subtype C is Integer;`
   D. ``subtype D is digits 6;``

.. container:: animate

   Explanations

   A. This generates a run-time error because the first enumeral specified is not in the range of :ada:`Enum_Sub_T`
   B. Compile error - no type specified
   C. Correct - standalone subtype
   D. :ada:`Digits 6` is used for a type definition, not a subtype

=====
Lab
=====

.. include:: labs/030_basic_types.lab.rst

=========
Summary
=========

--------------------------------------
 Benefits of Strongly Typed Numerics
--------------------------------------

* **Prevent** subtle bugs
* Cannot mix :ada:`Apples` and :ada:`Oranges`
* Force to clarify **representation** needs

    - eg. constant with or with fractional part

   .. code:: Ada

      type Yen is range 0 .. 1_000_000;
      type Ruble is range 0 .. 1_000_000;
      Mine : Yen := 1;
      Yours : Ruble := 1;
      Mine := Yours; -- illegal

------------------------------------
User-Defined Numeric Type Benefits
------------------------------------

* Close to **requirements**

   - Types with **explicit** requirements (range, precision, etc.)
   - Best case: Incorrect state **not possible**

* Either implemented/respected or rejected

   - No run-time (bad) suprise

* **Portability** enhanced

   - Reduced hardware dependencies

---------
Summary
---------

* User-defined types and strong typing is **good**

   - Programs written in application's terms
   - Computer in charge of checking constraints
   - Security, reliability requirements have a price
   - Performance **identical**, given **same requirements**

* User definitions from existing types *can* be good
* Right **trade-off** depends on **use-case**

   - More types |rightarrow| more precision |rightarrow| less bugs
   - Storing **both** feet and meters in :ada:`Float` has caused bugs
   - More types |rightarrow| more complexity |rightarrow| more bugs
   - A :ada:`Green_Round_Object_Altitude` type is probably **never needed**

* Default initialization is **possible**

   - Use **sparingly**
