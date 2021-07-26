.. |rightarrow| replace:: :math:`\rightarrow`

.. role:: c(code)
    :language: C

.. role:: ada(code)

    :language: Ada


****************************
Ada Buzzwords And Concepts
****************************

==============
Declarations
==============

------------------------------------
Identifiers, Comments, and Pragmas
------------------------------------

* Identifiers

  .. code:: Ada

    identifier ::= letter {[underline] letter_or_digit}

  * Character set **Unicode** 4.0
  * Case **not significant**
  * Reserved words are **forbidden**

* Reserved Words - language keywords

* Comments

  * Terminate at end of line (i.e., no comment terminator sequence)

* Pragmas

  * Compiler directives
  * Unrecognized pragmas have no effect (but should generate warnings)
  * Malformed pragmas are **illegal**

------------------
Numeric Literals
------------------

* Decimal Numeric Literals

  .. code::

    decimal_literal ::= numeral [.num] E [+numeral|-numeral]
    numeral ::= digit {[underline] digit}

  * Underscore is not significant
  * Examples

    .. code:: Ada

      12      0       1E6         123_456
      12.0    0.0     3.14159_26  2.3E-4

* Based Numeric Literals

  * Base can be 2 .. 16
  * Exponent is always a decimal number

* Comparison To C's Based Literals

  * Design in reaction to C issues
  * C has **limited** bases support
  * Zero-prefixed octal :code:`0nnn`

---------------------
Object Declarations
---------------------

* Declarations

  * Associate a **name** to an **entity**
  * Declaration **must precede** use
  * **Some** implicit declarations

* Object Declarations

  .. code:: Ada

    <name> : subtype_indication [:= <initial value>];

  * Variables and constants
  * Examples

    .. code:: Ada

      Z, Phase : Analog;
      Max : constant Integer := 200;
      -- variable with a constraint
      Count : Integer range 0 .. Max := 0;
      -- dynamic initial value via function call
      Root : Tree := F(X);

* Elaboration

  * Objects - memory allocation, initialization
  * Linear

-----------------
Universal Types
-----------------

* Universal Types

  * Implicitly defined
  * Entire *classes* of numeric types
  * Match any integer / real type respectively

* Numeric Literals Are Universally Typed

  * No need to type them
  * Compiler handles typing

* Literals Must Match "Class" of Context

  * `universal_integer` literals |rightarrow| **integer**
  * `universal_real` literals |rightarrow| **fixed** or **floating** point

---------------
Named Numbers
---------------

* Named Numbers

  * Associate a **name** with an **expression**

  .. code:: Ada

    <name> : constant := <static_expression>;

* Named Number Benefit

  * Evaluation at **compile time**

----------------------
Scope and Visibility
----------------------

* Scope and Visibility

  * **Scope** of a name - where name is *potentially* available
  * **Visibility** of a name - where name is *actually* available

* Introducing Block Statements

  * **Sequence** of statements

  .. code:: Ada

    My_Block: declare
      I : Integer;
    begin
      I := 1;
    end My_Block;

* Scope and "Lifetime"

  * Object in scope |rightarrow| exists
  * No *scoping* keywords

* Name Hiding

  * Caused by **homographs**
  * Overcoming hiding - add a **prefix**

----------------
Aspect Clauses
----------------

* Aspect Clauses

  * Define **additional** properties of an entity
  * Usage close to pragmas

* Boolean Aspect Clauses

  * **Boolean** aspects only
  * Longhand

    .. code:: Ada

      procedure Foo with Inline => True;

  * Aspect name only |rightarrow| **True**

    .. code:: Ada

      procedure Foo with Inline;

  * No aspect |rightarrow| **False**

    .. code:: Ada

      procedure Foo;

=============
Basic Types
=============

------------------------
Discrete Numeric Types
------------------------

* Signed Integer Types

  .. code:: Ada

    type Small_T is range -10 .. 100;

  * Range of signed **whole** numbers
  * Predefined Integer Types

    * :ada:`Integer` **>= 16 bits** wide

* Modular Types

  * Integer type - **Unsigned** values

* Operators for Any Integer Type

  * By increasing precedence
  * Division by zero |rightarrow| :ada:`Constraint_Error`

* Compile-Time Constraint Violation

  * *May* produce warnings and/or errors

* Range Check Failure

  * Compile-time rejection, else run-time **exception**

* String Attributes For All Scalars

  * :ada:`T'Image( input )`, :ada:`T'Value( input )`

* Range Attributes For All Scalars

  * :ada:`T'First`, :ada:`T'Last`, :ada:`T'Range`

----------------------------
Discrete Enumeration Types
----------------------------

* Enumeration Types

  * Enumeration of **logical** values
  * Literals

    * Distinct, ordered
    * Can be in **multiple** enumerations

    .. code:: Ada

      type Colors is (Red, Orange, Yellow, Green, Blue, Violet);
      type Stop_Light is (Red, Yellow, Green);
      -- Red both a member of Colors and Stop_Light
      Shade : Colors := Red;
      Light : Stop_Light := Red;

* Character Types

  * **Special-case** of enumerated type
  * Can be user-defined

* Language-Defined Character Types

  * :ada:`Character`, :ada:`Wide_Character`, :ada:`Wide_Wide_Character`

* Language-Defined Type Boolean

  * Supports assignment, relational operators, attributes
  * Logical operators :ada:`and`, :ada:`or`, :ada:`xor`, :ada:`not`

* Boolean Operators' Operand Evaluation

  * Evaluation order **not specified**
  * Short-Circuit Forms |rightarrow| **fixed** evaluation order

    * Left-to-right; right only evaluated **if necessary**

      .. code:: Ada

          Divisor /= 0 and then K / Divisor = Max
          Divisor = 0 or else K / Divisor = Max

* Order Attributes For All Discrete Types

  * **All discrete** types, mostly useful for enumerated types
  * :ada:`T'Pos (Input)`, :ada:`T'Val (Input)`

------------
Real Types
------------

* Real Types

  * Approximations to **continuous** values
  * Floating-point - variable exponent, relative precision
  * Fixed-point - constant exponent, absolute precision
  * Real Type (Floating and Fixed) Literals

    * **Must** contain a fractional part

* Declaring Floating / Fixed Point Types

  .. code:: Ada

    type Float_T is digits 6 range 0.0 .. 1_000.0;
    type Fixed_T is delta 0.1 range -10.0 .. 10.0;

----------
Subtypes
----------

* Subtype

  * May **constrain** an existing type
  * Still the **same** type

  .. code:: Ada

    type Integer_T is range 0 .. 1_000;
    subtype Subtype_T is Integer_T range 1 .. 1_000;
    subtype Alias_T is Integer_T;

* Effects of Constraints

  * Constraints only on values
  * Functionalities are **kept**

* Assignment Respects Constraints

  * RHS values must satisfy type constraints
  * :ada:`Constraint_Error` otherwise

============
Statements
============

------------------
Block Statements
------------------

.. code:: Ada

   declare
      U, V : Integer;
   begin
      Get (V);
      Get (U);
      if U > V then -- swap them
         Swap: declare
            Temp : Integer;
         begin
            Temp := U;
            U := V;
            V := Temp;
         end Swap;
         -- Temp does not exist here
      end if;
      Print (U);
      Print (V);
   end;

* Local **scope**
* Optional declarative part

-----------------
Null Statements
-----------------

* Null Statements

  * Explicit no-op statement
  * Constructs with required statement

.. code:: Ada

   case Today is
     when Monday .. Thursday =>
       Work (9.0);
     when Friday =>
       Work (4.0);
     when Saturday .. Sunday =>
       null;
   end case;

-----------------------
Assignment Statements
-----------------------

.. code:: Ada

  X := 1234;

* Value of expression is copied to target variable

* The type of the RHS must be same as the LHS

* Assignment Statements, Not Expressions

  * Not like C:

  .. code:: C

    int a = b = c = 1;
    while (line = readline(file))
      do_something(line);
    if ( a = 1 ) /* assigning 1 to a! */

* Implicit Range Constraint Checking

  .. code:: Ada

      procedure Demo is
        A    : Integer;
        B, C : Integer range 0 .. 100;
      begin
        ...
        B := A; -- checks generated here
        B := C; -- but not here
        ...
      end Demo;

------------------------
Conditional Statements
------------------------

* If-then-elsif-else Statements

  .. code:: Ada

    if A then
      Do_Something(1);
    elsif B then
      Do_Something(2);
    else
      Do_Something(3);
    end if;

  * At least one statement must be supplied
  * Sequential choice with alternatives (rather than nesting)
  * Optional (and multiple) :ada:`elsif` alternatives, tested in textual order
  * Optional :ada:`else` part

* Case Statements

  .. code:: Ada

    type Directions is  (Forward, Backward, Left, Right);
    Direction : Directions;
    ...
    case Direction is
      when Forward =>  Go_Forward (1);
      when Backward => Go_Backward (1);
      when Left =>  Go_Left (1);
      when Right => Go_Right (1);
    end case;

  * No fall-through between cases
  * **All** possible values must be covered once and only once
  *  `Others` Choice (all uncovered values) - must be in last position

-----------------
Loop Statements
-----------------

* Basic Loops and Syntax

  * All kind of loops can be expressed

  .. code:: Ada

    loop
       Put_Line ( "Hello" );
       delay 0.1;
    end loop;

* Loop Exit Statements

  .. code:: Ada

    loop
       I := I + 1;
       Put_Line ( "Hello" );
       if I > 100 then
           Put_Line ( "Goodbye" );
           exit; -- unconditional
       end if;
       exit when I = 99; -- conditional
       delay 0.1;
    end loop;

* While-loop Statements

  .. code:: Ada

    while I < Limit loop
        Do_Something(I);
        i := I + 1;
    end loop;

* For-loop Statements

  .. code:: Ada

    for I in 1 .. 10 loop
        Do_Something(I);
    end loop;

-----------------
GOTO Statements
-----------------

* Mostly discouraged

* May simplify control flow

* For example in-loop `continue` construct

* As always maintainability beats hard set rules

=============
Array Types
=============

-------------------------
Constrained Array Types
-------------------------

* Constrained Array Type Declarations

  .. code:: Ada

      type Vector_T is array ( 1 .. 100 ) of Integer;
      type Bits_T is array ( 0 .. 7 ) of Bit_T;
      type Location_T is range ( -10 .. 10 ) of Address;
      type Full_Week_T is array (Days) of Real;
      type Weekdays is array (Mon .. Fri) of Real;

* Multiple-Dimensioned Array Types

  .. code:: Ada

    type Three_D_Space_T is
        array ( -10..10, -10..10, -10..10 ) of Object_T;
    type Data_T is
        array ( 1 .. 10, 'a' .. 'z', Boolean ) of Object_T;

---------------------------
Unconstrained Array Types
---------------------------

.. code:: Ada

  type Index is range 1 .. Integer'Last;
  type CharList is array (Index range <>) of Character;

* Unconstrained Array Type Declarations

  * Do not specify bounds for objects

    * Different objects of same type may have different bounds

  * Bounds cannot change once set

* Supplying Index Constraints for Objects

  * Bounds set by declaration, initialization, etc.
  * Once set, bounds never change

* Bounds Must Satisfy Type Constraints

  .. code:: Ada

    Good : CharList(1..10);
    Bad  : CharList(0..1); -- runtime error

* "String" Types

  * Language-defined unconstrained array types

* No Unconstrained Component Types

  * Arrays: consecutive elements of the exact **same type** - so size must be *defined*

------------
Attributes
------------

* Array Attributes

  * Return info about array index bounds
  * Meaningfully applied to constrained array types
  * Meaningfully applied to array objects

* Attributes' Benefits

  * Allow code to be more robust
  * Optimizer can identify redundant checks

* Nth Dimension Array Attributes

  * Attribute parameter indicates dimension requested

    * ``T'Length(n)``
    * ``T'First(n)``
    * where n is the dimension required, including 1 (default)

    .. code:: Ada

       type Two_Dimensioned is array
          (1 .. 10, 12 .. 50) of T;
       TD : Two_Dimensioned;

    * :ada:`TD'First` (2) is 12
    * :ada:`TD'Last` (2) is 50
    * :ada:`TD'Length` (2) is 39
    * :ada:`TD'first` is 1 (same as :ada:`TD'first(1)`)
    * :ada:`TD'last` is 10 (same as :ada:`TD'last(1)`)

------------
Operations
------------

* Object-Level Operations

  * Assignment of array objects
  * Equality and inequality
  * Conversions

* Extra Object-Level Operations

  * *Only for 1-dimensional arrays!*
  * Concatenation
  * Relational (for discrete component types)
  * Logical (for Boolean component type)
  * Slicing

* Slicing

  * "Slices" out a contiguous part of an array
  * Allowed on any 1-dimensional array type

* "Membership" Tests

  * Shorthand for constraint checking
  * Uses reserved word :ada:`in`

------------------------------
Operations Added for Ada2012
------------------------------

* Default Initialization for Array Types

  * Supports constrained and unconstrained array types
  * Uses aspect `Default_Component_Value`

* Two High-Level For-Loop Kinds

  * For arrays, containers, iterator objects

* Array/Container For-Loops

  * Work in terms of elements within an object

   .. code:: Ada

      Numbers : constant array (1 .. 5) of Integer :=
           (2, 3, 5, 7, 11);

      -- component-based looping
      for N of Numbers loop
         Put_Line (Integer'Image (N));
         N := N + 1; -- we can modify the component
      end loop;

      -- index-based looping
      for I in Numbers'range loop
         Put_Line (Integer'Image (Numbers(I)));
      end loop;

------------
Aggregates
------------

* Literals for composite types

  .. code:: Ada

    type Array_T is array ( Boolean ) of Integer;
    -- Positional Aggregate
    P : Array_T := ( 100, 200 );
    -- Named  Aggregate
    N : Array_T := ( True => 100, False => 200 );

* Aggregates Are True Literal Values

  * Used any place a value of the type may be used

* Aggregate Consistency Rules

  * Must always be complete
  * Must provide only one value per index position
  * Compiler rejects incomplete or inconsistent aggregates

--------
Slices
--------

* Specifies a contiguous subsection of an array

  .. code:: Ada

    declare
      S1 : String (1 .. 9) := "Hi Adam!!";
      S2 : String := "We love    !";
    begin
      Put_Line (S1 (4..6));
      S2 (9..11) := S1 (4..6);
      S2 (12) := '?';
      Put_Line (S2);

* Slicing With Explicit Indexes

  .. code:: Ada

     declare
       Full_Name : String (1 .. 20);
     begin
       Put_Line (Full_Name);
       Put_Line (Full_Name (1..10));  -- first half of name
       Put_Line (Full_Name (11..20)); -- second half of name

* Slicing With Named Subtypes for Indexes

  .. code:: Ada

    declare
      subtype First_Name is Positive range 1 .. 10;
      subtype Last_Name is Positive range 11 .. 20;
      Full_Name : String(First_Name'First..Last_Name'Last);
    begin
      Put_Line(Full_Name(First_Name)); -- Full_Name(1..10)
      if Full_Name (Last_Name) = SomeString then ...

==============
Record Types
==============

------------------
Components Rules
------------------

* Characteristics of Components

  * Heterogeneous types allowed
  * Referenced by name
  * May be no components, for empty records
  * No anonymous types (e.g., arrays) allowed
  * No constant components
  * Multiple component declarations allowed
  * No recursive definitions
  * Default initial expressions allowed

* "Dot" Notation for Components Reference

------------
Aggregates
------------

* Aggregates

  * Literal values for composite types
  * Can use both **named** and **positional**

  .. code:: Ada

    type R1_T is record
        A : Integer;
        B : Character;
        C : Float;
    end record;
    type R2_T is record
        X : integer;
        Y : Character;
        Z : R1_T;
    end record;
    R : R2_T := ( 111,
                  '2',
                  Z => ( 123, B => 'b', C => 12.34 ) );

----------------
Default Values
----------------

* Default Component Value Evaluation

  * Occurs when object is elaborated
  * Not evaluated if explicitly overridden

* Defaults Within Record Aggregates

  * Specified via the ``box`` notation
  * Value for the component is thus taken as for a stand-alone object declaration
  * Can only be used with "named association" form

.. code:: Ada

  type R_T is record
    A : Integer := F(1);
    B : Integer := F(2);
    C : Integer := F(3);
  end record;

  X : R_T; -- Calls 'F' three times
  Y : R_T := ( 1, 2, 3 ); -- Never calls 'F'
  Z : R_T := ( 100, C => 200, others => <> ); -- Calls 'F' once
    
-----------------
Variant Records
-----------------

* Record type where

   * Different objects have different sets of components
   * Given object itself may be *unconstrained*

* Variant record offers a kind of storage overlaying

* Special field (*discriminant*) - value indicates which variant present

  * When field in variant is selected, run-time checks discriminant value is consistent
  * Can only read discriminant (as any other field), not write

.. code:: Ada

  type Person_Tag is (Student, Faculty);
  type Person (Tag : Person_Tag) is record
     Name : String (1 .. 10);
     case Tag is
        when Student => -- 1st variant
           Gpa  : Float range 0.0 .. 4.0;
           Year : Integer range 1 .. 4;
        when Faculty => -- 2nd variant
           Pubs : Integer;
     end case;
  end record;

* Cannot assign `Student` object to `Faculty` object

=============
Subprograms
=============

-------------------------
Declarations and Bodies
-------------------------

* Subprogram Declarations

  * Define the external (user) interface
  * Required in some circumstances
  * Optional in other circumstances

* Subprogram Bodies

  * Provide the implementation
  * Define execution behavior

.. code:: Ada

   procedure Do_Something;
   function Do_Another_Thing return return Boolean is
   begin
      Do_Something;
   end Do_Another_Thing;
   procedure Do_Something is
   begin
      null;
   end Do_Something;

------------
Parameters
------------

* *Actual* parameters are values passed to a call

* *Formal* parameters are defined by specification

* Parameter Associations In Calls

  * Associate formal parameters with actuals
  * Traditional "positional association" is allowed
  * "Named association" also allowed

* Parameter Modes

  * Mode :ada:`in` |rightarrow| Only reading allowed
  * Mode :ada:`out` |rightarrow| Writing expected (but reading allowed)
  * Mode :ada:`in out` |rightarrow| Initial value set by caller, may be modified

* Parameter Defaults May Be Specified

  * Mode :ada:`in` formals only
  * Callers may omit corresponding actual for calls

.. code:: Ada

   procedure P ( Formal : in Integer := 0 );
   procedure Q ( Formal : in out Character );

-----------------
Null Procedures
-----------------

* Shorthand for a procedure body that does nothing

.. code:: Ada

  procedure Longhand is
  begin
    null;
  end Longhand;
  procedure Shorthand ( Flag : Boolean ) is null;

* Explicitly indicates nothing to be done, rather than an accidental removal of statements

* Typical Use for Null Procedures: OOP

  * When you want a method to be concrete, rather than abstract, but don't have anything for it to do

--------------------
Nested Subprograms
--------------------

* Subprograms within Subprograms

  * Subprograms can be placed in any declarative block
  * Useful for performing sub-operations without passing parameter data

.. code:: Ada

  procedure Outside ( Arry : Array_T ) is
    procedure Inside ( Component : Component_T ) is
    begin
      Do_Something ( Component );
    end Inside;
  begin
    for C of Arry loop
      Inside ( C );
    end loop;
  end Inside;

--------------------
Function Specifics
--------------------

* Return Statements In Functions

  * Must have at least one
  * Returns a value of the specified (sub)type

* No Path Analysis Required By Compiler

  * Running to the end of a function without hitting a :ada:`return` statement raises :ada:`Program_Error`
  * Compilers can issue warning if they suspect that a :ada:`return` statement will not be hit

.. code:: Ada

  function F ( Flag : Integer ) return Boolean is
  begin
    if Flag > 0 then
      return True;
    elsif Flag < 0 then
      return False;
    end if;
    -- runtime error (no return if Flag = 0)
   end F;
   
----------------------
Expression Functions
----------------------

* Shorthand for declaring functions whose implementations are only "expressions"

  .. code:: Ada

    function Square ( X : Integer )
      return Integer is ( X**2 );

* Typical Uses for Expression Functions

  * May be part of general (ADT) implementation
  * May exist only for sake of pre/postconditions

--------------------
Potential Pitfalls
--------------------

* Unassigned :ada:`out` parameters

* "Side Effects" - modification of global data

  * Any effect upon external objects or external environment

* Order-Dependent Code

  * Order of evaluation of parameters in subprogram call is not specified in language

* Parameter Aliasing

  * When there are multiple names for an actual parameter inside a subprogram body

=============
Expressions
=============

------------------
Membership Tests
------------------

* "Membership" Operation

  .. code:: Ada

     X : Integer := ...
     B : Boolean := X in 0 .. 5;
     C : Boolean := X not in 1 .. 10;

  * Acts like a boolean function
  * Usable anywhere a boolean value is allowed

* Testing Constraints via Membership

  .. code:: Ada

    type Calendar_Days  is (Mon, Tues, Wed, Thur, Fri, Sat, Sun);
    subtype Weekdays is Calendar_Days range Mon .. Fri;
    ...
    if Day in Weekdays then ... - same as above

* Testing Non-Contiguous Membership

  .. code:: Ada

    declare
      M : Month_Number := Month (Clock);
    begin
      if M in 9 | 4 | 6 | 11 then
        Put_Line ("31 days in this month");
      elsif M = 2 then
        Put_Line ("It's February, who knows?");
      else
        Put_Line ("30 days in this month");
      end if;

-------------------------
Conditional Expressions
-------------------------

* Conditional Expressions

  * Ultimate value depends on a controlling condition

  * Allowed wherever an expression is allowed

  * Similar intent as in other languages

* *If Expressions*

  * Syntax looks like an if-statement without :ada:`end if`

  .. code:: Ada

    Put_Line ("Self-destruct in" & Sec'Img &
         (if Sec = 1 then " second" else " seconds"));

* *Case Expressions*

  .. code:: Ada

    for M in Months loop
      End_Of_Month (M):=
        (case M is
         when Sep | Apr | Jun | Nov => 30,
         when Feb => (if Leap(Y) then 29 else 28),
         when others => 31);
    end loop;

* Quantified Expressions

  * Check if a condition is true on a set

  .. code:: Ada

    A : array ( 1 .. 10 ) of Integer := ( ... );
    All_Even : boolean := ( for all X of A => I mod 2 = 0 );
    Any_Even : boolean := ( for some X of A => I mod 2 = 0 );

=============
Overloading
=============

-------------------------
Enumerals and Operators
-------------------------

* Overloading Enumerals

  * Each is treated as if a function name (identifier)
  * Thus same rules as for function identifier overloading

* Overloadable Operator Symbols

  * Only those defined by the language already
  * Note that assignment (:=) is not an operator

* Parameters for Overloaded Operators

  * Must not change syntax of calls
  * Infix calls use positional parameter associations
  * Named parameter associations allowed but ugly

-----------------
Call Resolution
-----------------

* Call Resolution

  * Compilers must reject ambiguous calls
  * Resolution is based on the calling context

* Profile Components Used

  * Significant components appear in the call itself
  * Insignificant components might not appear at call

* Manually Disambiguating Calls

  * Qualification can be used
  * Named parameter association can be used

.. code:: Ada

   type Complex is ...
   function "+" (L, R : Complex) return Complex;
   A, B : Complex := some_value;
   C : Complex := A + B;  -- user-defined operator
   D : Real := A + B;     -- illegal
   E : Real := 1.0 + 2.0; -- predefined operator

-------------------
Visibility Issues
-------------------

* Inherently Ambiguous Declarations

  .. code:: Ada

    procedure P (X : in Natural) is ...
    procedure P (A : in out Positive) is ...

  * Profile appears multiple times within a single scope
  * Are illegal since all calls would be ambiguous - compile error

* Profile Hiding

  .. code:: Ada

    procedure P (X : in Natural) is
       procedure P (A : in out Positive) is ...
    begin

  * Subprograms can hide others with same profile

-----------------------
User-Defined Equality
-----------------------

* User-Defined Equality

  * Allowed like any other operator
  * May have any parameter and result types

* User-Defined `=` Returning Boolean

  * Implicitly declares ``/=``
  * Thus negation has consistent meaning
  * No explicit declaration of ``/=`` returning Boolean

* User-Defined Equality Example

  * Especially useful for composite types
  * Predefined ``=`` is bit-wise comparison over entire structure

.. code:: Ada

  type List is array (1 .. 100) of Integer;
  type Stack is record
    Values : List;
    Top : Natural := 0;
  end record;

  function "=" (Left, Right : Stack) return Boolean is
  begin
    if Left.Top /= Right.Top then -- not same size
      return False;
    else -- compare values
      return ( for all K in 1 .. Left.Top =>
                 Left.Values(K) = Right.Values(K) );
    end if;
  end "=";

===============
Library Units
===============

---------------
Library Units
---------------

* Library Units

  * Those not nested within another program unit
  * Candidates

   - Subprograms
   - Packages
   - Generic Units
   - Generic Instantiations
   - Renamings

* Illustration

  .. code:: Ada

    package Operating_System is
      procedure Foo( ... );
      procedure Bar( ... );
      package Process_Manipulation is
        ...
      end Process_Manipulation;
      package File_System is
        ...
      end File_System;
    end Operating_System;

  * `Operating_System` is library unit
  * `Foo`, `Bar`, etc - not library units

------------------
Object Lifetimes
------------------

* Declared Object "Lifetimes"

  * Same as their enclosing declarative region
  * No ``static`` etc. directives as in C
  * Objects declared in subprogram exist while subprogram executes
  * Objects in library packages exist as long as program executes
  * Objects in non-library packages exist as long as region enclosing the package

* Program "Lifetime"

  * Run-time library is initialized
  * All (any) library packages are elaborated
  * Main program's declarative part is elaborated
  * Main program's sequence of statements executes
  * Program executes until all threads terminate
  * All objects in library packages cease to exist
  * Run-time library shuts down

------------------
Main Subprograms
------------------

  * Must be library subprograms
  * No special program unit name required
  * Can be many per program library
  * Always can be procedures
  * Can be functions if implementation allows it

--------------
Dependencies
--------------

*  `with` Clauses

  * Specify the library units that a compilation unit depends upon

.. code:: Ada

   with Ada.Text_IO; -- dependency
   procedure Hello is
   begin
     Ada.Text_IO.Put ("Hello World");
   end Hello;

* What To Import

  * Need only name direct dependencies
  * Will not cause compilation of referenced units

==========
Packages
==========

--------------
Declarations
--------------

* Package Declarations

  * Required in all cases
  * Describe the client's interface
  * When changed, requires clients recompilation

  .. code:: Ada

    package Float_Stack is
      Max : constant := 100;
      procedure Push (X : in Float);
      procedure Pop (X : out Float);
    end Float_Stack;

* Compile-Time Visibility Control

  * Items in the declaration are visible to users
  * Items in the body are never externally visible

  .. code:: Ada

    package body Float_Stack is
      Stack : array ( 1 .. Max ) of Float;

* Referencing Exported Items

  .. code:: Ada

    with Float_Stack;
    procedure Test is
      X : Float;
    begin
      ...
      Float_Stack.Push (12.0);

--------
Bodies
--------

* Package Bodies

  * Dependent on corresponding package specification
  * Clients need only to relink if body changed
  * Required when specification contains declarations requiring completions it cannot contain

* Bodies Are Never Optional

  * Either required for a given spec or not allowed at all

  .. code:: Ada

    package body Float_Stack is
      Stack : array ( 1 .. Max ) of Float;
      Next : Integer := 1;
      procedure Push (X : in Float) is
      begin
         Stack(Next) := X;
         Next := Next + 1;
      end Push;
      procedure Pop (X : out Float) is
      begin
         Next := Next - 1;
         X := Stack(Next);
      end Push;
    end Float_Stack;

------------------
Executable Parts
------------------

* Optional Executable Part

* Executable Part Semantics

  * Executed only once, when package is elaborated
  * Ideal when statements are required for initialization

--------
Idioms
--------

* Named Collection of Declarations

  * Types, Constants, Global Data

* Controlling Data Visibility Using Packages

  * Divides global data into separate package bodies
  * Visible only to procedures and functions declared in those same packages
  * Global change effects are much less likely

* Abstract Data Machines

  * Export operations, queries
  * No direct user access to data

===============
Private Types
===============

--------------------------------------------
Implementing Abstract Data Types via Views
--------------------------------------------

* Implementing Abstract Data Types

  * A combination of constructs in Ada
  * Constituent parts

    * Packages, with "private part" of package spec
    * "Private types" declared in packages
    * Subprograms declared within those packages

* Package Visible and Private Parts for Views

  * Declarations in visible part are exported to users
  * Declarations in private part are hidden from users

* Partial and Full Views of Types

  * Private type declaration defines a partial view
  * Full type declaration defines the full view
  * Operations available depend upon one's view

* Benefits of Views

  * Users depend only on visible part of specification
  * Changes to implementation don't affect users

----------------------
Private Part Example
----------------------

* Supplier

  .. code:: Ada

    package Stack_Pkg is
      type Stack_T is private; -- partial declaration
      procedure Push ( S : in out Stack_T;
                       I : Integer );
      procedure Pop  ( S : in out Stack_T;
                       I : out Integer );
    private
      -- This type is hidden from users
      type Array_T is array (1..100) of Integer;
      type Stack_T is record -- full declaration
        Top   : integer := 0;
        Stack : Array_T;
      end record;
    end Stack_Pkg;

* Client

  .. code:: Ada

    with Stack_Pkg;
    procedure User is
      S : Stack_Pkg.Stack_T;
      A : Stack_Pkg.Array_T; -- Illegal!
      I : Integer;
    begin
      Stack_Pkg.Push ( S, 1234 );
      Stack_Pkg.Pop ( S, I );
      S.Top := 1; -- illegal!
    end User;

---------------------------
Private Part Construction
---------------------------

* Private Part Location

  * Must be in package specification, not body
  * Body usually compiled separately after declaration
  * Users can compile their code before the package body is compiled or even written

* Private Part and Recompilation

  * Private part is part of the specification
  * Thus changes to private part require user recompilation

* Declarative Regions

  * Declarative region of the spec extends to the body

-----------------
View Operations
-----------------

* View Operations

  * A matter of inside versus outside the package

* Designer View Sees Full Declaration - allows all operations

* Users Have the Partial View

  * Since they are outside package
  * Basic operations
  * Exported subprograms

* User View's Activities

  * Declarations of objects
  * Assignment, equality and inequality, conversions
  * Designer's declared subprograms
  * User-declared subprograms

------------------------------------
When To Use or Avoid Private Types
------------------------------------

* When To Use Private Types

  * Implementation may change
  * Normally available operations do not "make sense"
  * Users have no "need to know"

* When To Avoid Private Types

  * If the abstraction is too simple to justify the effort
  * If normal user interface requires representation-specific operations that cannot be provided

===============
Limited Types
===============

--------------
Declarations
--------------

* Limited Type Declarations

  .. code:: Ada

    type Spin_Lock_T is limited record
       ID   : Interfaces.Unsigned_8;
       Time : Ada.Calendar.Time;
    end record;

  * Are always record types unless also private

* Parameter Passing Mechanism

  * Always "by-reference" if explicitly limited
  * By definition, these subprograms would be called concurrently

* Composites with Limited Types

  * Composite containing a limited type becomes limited as well

-----------------
Creating Values
-----------------

* Initialization is not assignment (but looks like it)!

* Via **limited aggregates**

  * Only Mode `in` for Limited Aggregates

    * Aggregates are not variables, so no place to put the returning values for :ada:`out` or :ada:`in out` formals

  .. code:: Ada

    Lock : Spin_Lock_T :=
      ( ID => 1, Time => Ada.Calendar.Clock );

* Via **limited constructor functions**

  .. code:: Ada

    Object : Spin_Lock_T := ( ID => 1, Time => Ada.Calendar.Clock );
    function Illegal ( ID : Interfaces.Unsigned_8 )
        return Spin_Lock_T is ( Object ); -- Copy not allowed!

    function New_Lock ( ID : Interfaces.Unsigned_8 )
        return Spin_Lock_T is
      ( ( ID => 1, Time => Ada.Calendar.Clock ) );

----------------------------
Extended Return Statements
----------------------------

* Function Extended Return Statements

  * Result is expressed as an object
  * More expressive than aggregates
  * Handling of unconstrained types

.. code:: Ada

  --  Implicitely limited array
  type Spin_Lock_Array (Positive range <>) of Spin_Lock;
       
  function F return Spin_Lock_Array is
  begin
    return Result : Spin_Lock_Array (Unsigned_8 range 1 .. 10)
    do
      for I in Result'Range loop
        Result(I) := New_Lock ( I );
      end loop;
    end return;      
  end F;

-------------------------------------
Combining Limited and Private Views
-------------------------------------

* Limited Private Types

  * A combination of :ada:`limited` and :ada:`private` views
  * The typical design idiom for :ada:`limited` types

  .. code:: Ada

    package Spin_Lock is
      type Spin_Lock_T is limited private;
      function New_Lock ( ID : Interfaces.Unsigned_8 ) return Spin_Lock_T;
    private
      type Spin_Lock_T is limited record
        ID   : Interfaces.Unsigned_8;
        Time : Ada.Calendar.Time;
      end record;
    end Spin_Lock;

* Explicitly Limited Completions (Optional)

  * Completion in Full view includes word :ada:`limited`
  * Requires a record type as the completion
  * Allows no internal copying too

* Automatically Limited Full View

  * When other limited types are used in the representation

===================
Program Structure
===================

-------------------
Building A System
-------------------

* What is a System?

  * Also called Application or Program or ...

  * Collection of library units

* Library Units Review

  * Those units not nested within another program unit
  * Dependencies between library units via :ada:`with` clauses

------------------------
"limited with" Clauses
------------------------

* Handling Cyclic Dependencies

  * Elaboration must be linear
  * Package declarations cannot depend on each other

    .. code:: Ada

      with Department;
      package Personnel is
         ...
      end Personnel;
     
      with Personnel;
      package Department is
         ...
      end Department;

  * Which package elaborates first?

* Body-Level Cross Dependencies Are OK

  * Bodies only depend on other packages' declarations
  * Declarations are already elaborated by time bodies are elaborated

* `limited with` Clauses

  * Solve the cyclic declaration dependency problem
  * Provide a "limited" view of the specified package

    * Only see type names, not implementation
    * Thus typically involves some advanced features

* Full `with` clause on the package body

----------------------------
Hierarchical Library Units
----------------------------

* Problem: Packages Are Not Enough

  * Extensibility is a problem for private types
  * Should be something "bigger" than packages

* Solution: Hierarchical Library Units

  * Can extend packages with visibility to parent private part
  * Visibility of parent's private part is protected

* Programming By Extension

  * Parent unit

    .. code:: Ada

      package Complex is
        type Number is private;
        function "*" ( Left, Right : Number ) return Number;
        function "/" ( Left, Right : Number ) return Number;
      ...
      private
        type Number is record
          Real_Part, Imaginary_Part : Float;
        end record;
      end Complex;

  * Extension created to work with parent unit

    .. code:: Ada

      package Complex.Utils is
        procedure Put (C : in Number);
        function As_String (C : Number) return String;
        ...
      end Complex.Utils;

  * Extension Can See Private Section of Parent(s)

------------------------------------
Hierarchical Library Unit Examples
------------------------------------

* Subsystem Approach

  .. code:: Ada

    with Interfaces.C;
    package OS is -- Unix and/or POSIX
      type File_Descriptor is new Interfaces.C.int;
    end OS;

    package OS.Mem_Mgmt is
      ...
      procedure Dump ( Requested_Location : System.Address;
                       Requested_Size     : Interfaces.C.Size_T );
      ...
    end OS.Mem_Mgmt;

    package OS.Files is
      ...
      function Open ( Device : Interfaces.C.char_array )
                      return File_Descriptor;
      ...
    end OS.Files;

* Predefined Hierarchies

  * Standard library facilities are children of `Ada`

    * `Ada.Text_IO`
    * `Ada.Calendar`
    * et cetera

  * Other root packages are also predefined

    * `Interfaces.C`
    * `System.Storage_Pools`
    * et cetera

-------------------------
Hierarchical Visibility
-------------------------

* Visibility behaves as if child is nested in parent

  * `with` clauses for ancestors are implicit
  * `with` clauses for siblings are required if needed

* Parents do not know their children!

  * Children grant themselves access to ancestors' private parts
  * Alternative is to grant access when declared
  * Note: Parent body can reference children

------------------
Private Children
------------------

* Intended as implementation artifacts
* Only available within subsystem

* Rules Preventing Private Child Visibility

  * Only available within immediate family
  * Public unit declarations have import restrictions

    * Not allowed to have :ada:`with` clauses for private units

  * Public unit bodies have no import restrictions
  * Private units can import anything

* Some Public Children Are Trustworthy

  * Would only use a private sibling's exports privately
  * But rules disallow :ada:`with` clause
  * Use :ada:`private with` clause

    .. code:: Ada

      private with Parent.Private_Child;
      package Parent.Sibling is ...
         ...
      private
         type T is new Parent.Private_Child.Something;

* Completely Hidden Declarations

  * Anything in a package body is completely hidden

* Child units can be subprograms

  * Separate declaration required if private
  * Only library packages can be parents

============
Visibility
============

---------------------------------------
"use" vs "use type" vs "use all type"
---------------------------------------

.. code:: Ada

   package Types is
      type My_Integer is record
         I, J : Integer := 0;
      end record;
      function "+" (L, R : My_Integer) return My_Integer;
      function "-" (L, R : My_Integer) return My_Integer;
      procedure Do_Something (P : in out My_Integer);
   end Types;

* :ada:`use Types;`

  * Provides direct visibility into exported items of :ada:`Types`

    * :ada:`My_Integer`, :ada:`"+"`, :ada:`"-"`, :ada:`Do_Something`

* :ada:`use type Types.My_Integer;`

  * Provides direct visibility into primitive operations on :ada:`My_Integer`

    * :ada:`"+"`, :ada:`"-"`

* :ada:`use all type Types.My_Integer;`

  * Provides direct visibility into all operations on :ada:`My_Integer`

    * :ada:`"+"`, :ada:`"-"`, :ada:`Do_Something`

-----------------------------------------------
"use" vs "use type" vs "use all type" Example
-----------------------------------------------

* :ada:`use` makes everything visible (not always a good thing)

  .. code:: Ada

    with Types; use Types;
    procedure Test_Use is
       I, J : Types.My_Integer;
       K    : My_Integer;
    begin
       I := J + K;
       Do_Something (I);
    end Test_Use;

* :ada:`use type` makes primitive operations visible

  .. code:: Ada

    with Types;
    use type Types.My_Integer;
    procedure Test_Use_Type is
       I, J : Types.My_Integer;
       K    : My_Integer; -- This is not OK
    begin
       I := J + K; -- This is OK
       Do_Something (I); -- This is not OK
    end Test_Use_Type;

* :ada:`use all type` makes all operations visible

  .. code:: Ada

    with Types;
    use all type Types.My_Integer;
    procedure Test_Use_All_Type is
       I, J : Types.My_Integer;
       K    : My_Integer; -- This is not OK
    begin
       I := J + K; -- This is OK
       Do_Something (I); -- This is OK
    end Test_Use_All_Type;

-------------------------
Dealing with Long Names
-------------------------

* Three Positives Make a Negative

  * Good Coding Practices ...

    * Descriptive names
    * Modularization
    * Subsystem hierarchies

  * Can result in cumbersome references

    .. code:: Ada

      -- use cosine rule to determine distance between two points,
      -- given angle and distances between observer and 2 points
      -- A**2 = B**2 + C**2 - 2*B*C*cos(A)
      Observation.Sides (Viewpoint_Types.Point1_Point2) :=
        Math_Utilities.Trigonometry.Square_Root
          (Observation.Sides (Viewpoint_Types.Observer_Point1)**2 +
           Observation.Sides (Viewpoint_Types.Observer_Point2)**2 +
           2.0 * Observation.Sides (Viewpoint_Types.Observer_Point1) *
             Observation.Sides (Viewpoint_Types.Observer_Point2) *
             Math_Utilities.Trigonometry.Cosine
               (Observation.Vertices (Viewpoint_Types.Observer)));

* Writing Readable Code - Part 1

  * We could use :ada:`use` on package names to remove some dot-notation

    .. code:: Ada

      Observation.Sides (Point1_Point2) :=
        Square_Root
          (Observation.Sides (Observer_Point1)**2 +
           Observation.Sides (Observer_Point2)**2 +
           2.0 * Observation.Sides (Observer_Point1) *
             Observation.Sides (Observer_Point2) *
             Cosine (Observation.Vertices (Observer)));

-------------------
Renaming Entities
-------------------

* The `renames` Keyword

  * Certain entities can be renamed within a declarative region

* Writing Readable Code - Part 2

   .. code:: Ada

      begin
         Side1          : Base_Types.Float_T renames Observation.Sides (Viewpoint_Types.Observer_Point1);
         Side2          : Base_Types.Float_T renames Observation.Sides (Viewpoint_Types.Observer_Point2);
         Angles         : Viewpoint_Types.Vertices_Array_T renames Observation.Vertices;
         Required_Angle : Viewpoint_Types.Vertices_T renames Viewpoint_Types.Observer;
         Desired_Side   : Base_Types.Float_T renames
           Observation.Sides (Viewpoint_Types.Point1_Point2);
         package Math renames Math_Utilities;
         package Trig renames Math.Trigonometry;
         function Sqrt (X : Base_Types.Float_T) return Base_Types.Float_T
           renames Math.Square_Root;
      begin
         Side1                   := Sensors.Read;
         Side2                   := Sensors.Read;
         Angles (Required_Angle) := Sensors.Read;
         -- use cosine rule to determine distance between two points, given angle
         -- and distances between observer and 2 points A**2 = B**2 + C**2 -
         -- 2*B*C*cos(A)
         Desired_Side := Sqrt (Side1**2 + Side2**2 +
                               2.0 * Side1 * Side2 * Math.Cosine (Angles (Required_Angle)));
      end;

==============
Access Types
==============

--------------
Access Types
--------------

* Java references, or C/C++ pointers are called access type in Ada

* An object is associated to a pool of memory

* Different pools may have different allocation / deallocation policies

* Null values

  * A pointer that does not point to any actual data has a null value

  * Without an initialization, a pointer is :ada:`null` by default

  * :ada:`null` can be used in assignments and comparisons

* Dereferencing pointers

  * :ada:`.all` does the access dereference

  * :ada:`.all` is optional for components of arrays and records

  .. code:: Ada

    type R is record
      F1, F2 : Integer;
    end record;
    type A_Int is access Integer;
    type A_R is access R;
    V_Int    : A_Int := new Integer;
    V_R      : A_R := new R;

    V_Int.all := 0;
    V_R.all := (0, 0);
    V_R.F1 := 1; -- similar to V_R.all.F1 := 1;

----------------------------
Pool-Specific Access Types
----------------------------

* Pool-Specific access type

  * An access type is a type
  * Conversion is needed to move an object pointed by one type to another (pools may differ)
  * You can not do this kind of conversion with a pool-specific access type

* Allocations

  * Objects are created with the :ada:`new` reserved word
  * The created object must be constrained
  * The object can be created by copying an existing object - using a qualifier

* Deallocations

  * Deallocations are unsafe
  * As soon as you use them, you lose the safety of your pointers
  * But sometimes, you have to do what you have to do ...

----------------------
General Access Types
----------------------

* General access types

  * Can point to any pool (including stack)
  * Still distinct type
  * Conversions are possible

* Referencing the stack

  * By default, stack-allocated objects cannot be referenced
  * :ada:`aliased` declares an object to be referenceable through an access value
  * :ada:`'Access` attribute gives a reference to the object

  .. code:: Ada

    type Acc is access all Integer;
      V : Acc;
      I : aliased Integer;
    begin
      V := I'Access;
      V.all := 5; -- Same a I := 5

* Using Pointers For Recursive Structures

  * It is not possible to declare recursive structure
  * But there can be an access to the enclosing type

  .. code:: Ada

    type Cell; -- partial declaration
    type Cell_Access is access all Cell;
    type Cell is record -- full declaration
      Next       : Cell_Access;
      Some_Value : Integer;
    end record;

------------------------
Anonymous Access Types
------------------------

* Anonymous Access Parameters

  * Parameter modes are of 4 types: `in`, `out`, `in out`, `access`
  * The access mode is called **anonymous access type**
  * When used:

    * Any named access can be passed as parameter
    * Any anonymous access can be passed as parameter

* Anonymous Access Types

  * Other places can declare an anonymous access

    .. code:: Ada

      function F return access Integer;
      V : access Integer;
      type T (V : access Integer) is record
        C : access Integer;
      end record;
      type A is array (Integer range <>) of access Integer;

* Anonymous Access Constants

  * :ada:`constant` (instead of :ada:`all`) denotes an access type through which the referenced object cannot be modified
  * :ada:`not null` denotes an access type for which null value cannot be accepted

============
Genericity
============

--------------------
What is a Generic?
--------------------

* A generic unit is a unit that does not exist
* It is a pattern based on properties
* The instantiation applies the pattern to certain parameters

-------------------
Creating Generics
-------------------

* What Can Be Made Generic?

  * Subprograms and packages can be made generic
  * Children of generic units have to be generic themselves

* How Do You Use A Generic?

  * Generic instantiation is creating new set of data where a generic package contains library-level variables:

--------------
Generic Data
--------------

* Generic Types Parameters

  * A generic parameter is a template
  * It specifies the properties the generic body can rely on
  * Actual parameter must provide at least as many properties as formal
  * The usage in the generic has to follow the contract

* Possible Properties for Generic Types

  .. code:: Ada

    type T1 is (<>); -- discrete
    type T2 is range <>; -- integer
    type T3 is digits <>; -- float
    type T4 (<>); -- indefinite
    type T5 is tagged;
    type T6 is array ( Boolean ) of Integer;
    type T7 is access integer;
    type T8 (<>) is [limited] private;

* Generic Parameters Can Be Combined

  .. code:: Ada

     generic
        type T (<>) is limited private;
        type Acc is access all T;
        type Index is (<>);
        type Arr is array (Index range <>) of Acc;
     procedure P;

---------------------
Generic Formal Data
---------------------

* Generic Constants and Variables Parameters

  * Variables can be specified on the generic contract
  * The mode specifies the way the variable can be used:

    * :ada:`in` |rightarrow| read only
    * :ada:`in out` |rightarrow| read write

    .. code:: Ada

       generic
          type T is private;
          X1 : Integer;  -- constant
          X2 : in out T; -- variable
       procedure P;

* Subprograms can be defined in the generic contract

  .. code:: Ada

    generic
      with procedure Callback;
    procedure P;

* A generic unit can depend on the instance of another generic unit

  .. code:: Ada

    generic
      type T1 is private;
      type T2 is private;
    package Base is [...]

    generic
      with package B is new Base (Integer, <>);
      V : B.T2;
    package Other [...]

=============
Inheritance
=============

------------
Primitives
------------

* The Notion of a Primitive

  * A type is characterized by data structure and set of operations that applies to it

    * Operations: **methods** in C++ or **Primitive Operations** in Ada

* General Rule For a Primitive

  * Subprogram `S` is primitive of type `T` if `S` is declared in scope of `T` and `S` has at least one parameter / return value of type `T`

    * A subprogram can be a primitive of several types

    .. code:: Ada

      package P is
        type T1 is range 1 .. 10;
        type T2 is (A, B, C);
        procedure P1 (V : T1);
        procedure P2 (V1 : Integer; V2 : T1);
        -- primitive of both T1 and T2
        function F (X : T2) return T1;
      end P;

* Implicit Primitive Operations

  * For scalar types, primitives implicitly created if not explicitly given
  * These primitives can be used just as any others

-------------------
Simple Derivation
-------------------

* Simple Type Derivation

  * In Ada, any (non-tagged) type can be derived

    .. code:: Ada

      type Child is new Parent;

  * Child is distinct type that inherits parent's data representation and primitives
  * Conversions are possible for non-primitive operations

* Simple Derivation and Type Structure

  * The structure of the type has to be kept
  * Scalar ranges can be reduced
  * Constraints on unconstrained types can be specified

  .. code:: Ada

    type Int is range -100 .. 100;
    type Arr is array (Integer range <>) of Integer;
    type Rec (Size : Integer) is record
       Elem : Arr (1 .. Size);
    end record;

    type Nat is new Int range 0 .. 100;
    type Pos is new Nat range 1 .. 100;
    type Ten_Elem_Arr is new Arr (1 .. 10);
    type Ten_Elem_Rec is new Rec (10);

* Operations can be overridden can overridden, added, or removed

   .. code:: Ada

      type Root is range 1 .. 100;
      procedure Prim1 (V : Root);
      procedure Prim2 (V : Root);

      type Child is new Root;
      overriding procedure Prim (V : Child); -- overridden
      not overriding procedure Prim3 (V : Child); -- added
      overriding procedure Prim2 (V : Child) is abstract; -- removed

-------------------
Tagged Derivation
-------------------

* Tagged Derivation

  * Simple derivation cannot change the structure of a type
  * Tagged derivation allows fields to be added

    .. code:: Ada

       type T1 is tagged record
         Member1 : Integer;
       end record;
       procedure Get_1 (This : T1);
       type T2 is new T with record
         Member2 : Integer;
       end record;
       procedure Get_1 (This : T2);
       procedure Get_2 (This : T2);
       type T3 is new T1 with null record;

* Primitives

  * Primitives implicitly inherited and can be overridden
  * A child can add new primitives
  * Parameter of which subprogram is primitive called controlling parameter
  * All controlling parameters must be of the same type

* Tagged Aggregate

  * Regular aggregate works - values must be given to all fields
  * Aggregate extension allows using copy of parent instance
  * :ada:`with null record` can be used when no additional components

  .. code:: Ada

    V  : T1 := (Member1 => 0);
    V2 : T2 := (V with Member2 => 1);
    V3 : T3 := (V with null record);

* Prefix Notation

  * Primitives of tagged types can be called like any other
  * *Distinguished receiver* when first parameter is controlling parameter

    .. code:: Ada

       Get_1 ( V ); -- procedure reference
       V2.Get_1;    -- "distinguished receiver"

==============
Polymorphism
==============

------------------
Classes of Types
------------------

* Classes

  * In Ada, a Class denotes an inheritance subtree
  * Class of :ada:`T` is the class of :ada:`T` and all its children
  * Type :ada:`T'Class` can designate any object typed after :ada:`T`
  * Objects of type :ada:`T'Class` have at least the properties of T

* Class-types Declaration

  * A class wide type is an **indefinite** type
  * Properties and constraints of indefinite types apply

.. code:: Ada

   procedure Main is
      type T is tagged null record;
      type D is new T with null record;
      procedure P (X : in out T'Class) is null;
      Obj : D;
      Dc  : D'Class := Obj;
      Tc1 : T'Class := Dc;
      Tc2 : T'Class := Obj;
   begin
      P (Dc);
      P (Obj);
   end Main;

* Abstract Types

  * A tagged type can be declared :ada:`abstract`
  * Then, :ada:`abstract tagged` types:

    * Cannot be instantiated
    * Can have abstract subprograms (with no implementation)
    * Non-abstract derivation of an abstract type must override and implement abstract subprograms

------------
Class Tags
------------

* Tag Attribute

  * Tagged types all have a tag
  * Accessed through the `'Tag` attribute
  * Applies to **both objects and types**
  * Membership check against a :ada:`'Tag` or :ada:`'Class`

.. code:: Ada

   type Parent is tagged null record;
   type Child is new Parent with null record;
   Parent_Obj : Parent; -- Parent_Obj'Tag = Parent'Tag
   Child_Obj  : Child;  -- Child_Obj'Tag = Child'Tag
   Parent_Class_1 : Parent'Class := Parent_Obj;
                    -- Parent_Class_1'Tag = Parent'Tag
   Parent_Class_2 : Parent'Class := Child_Obj;
                    -- Parent_Class_2'Tag = Child'Tag
   Child_Class    : Child'Class := Child(Parent_Class_2);
                    -- Child_Class'Tag  = Child'Tag

   B1 : Boolean := Parent_Class_1 in Parent'Class;       -- True
   B2 : Boolean := Parent_Class_1'Tag = Child'Class'Tag; -- False
   B3 : Boolean := Child_Class'Tag = Parent'Class'Tag;   -- False
   B4 : Boolean := Child_Class in Child'Class;           -- True


-------------------------------
Dispatching and Redispatching
-------------------------------

* Any subprogram expecting a T object can be called with a :ada:`T'Class` object
* The *actual* type of the object is not known at compile time
* The *right* type will be selected at runtime

.. code:: Ada

   procedure P1 (V : Root) is
      V_Class : Root'Class renames
                Root'Class (V); -- naming of a view
   begin
      P2 (V);              -- static: uses the definite view
      P2 (Root'Class (V)); -- dynamic: (redispatching)
      P2 (V_Class);        -- dynamic: (redispatching)

      -- Ada 2005 "distinguished receiver" syntax
      V.P2;                -- static: uses the definite view
      Root'Class (V).P2;   -- dynamic: (redispatching)
      V_Class.P2;          -- dynamic: (redispatching)
   end P1;

============
Exceptions
============

-----------------------
What Is An Exception?
-----------------------

* Error condition (like a signal or interrupt)
* Textual separation from normal processing
* Rigorous Error Management

   - Cannot be ignored, unlike status codes from routines
   - Example: running out of gasoline in an automobile

* Exceptions become active by being *raised*

   - Failure of implicit language-defined checks
   - Explicitly by application

* Normal execution abandoned when they occur

   - Error processing takes over in response
   - Response specified by **exception handlers**
   - *Handling the exception* means taking action in response
   - Other threads need not be affected

-----------------------
What Is An Exception?
-----------------------

.. code:: Ada

   package body Automotive is
     ...
     procedure Consume_Fuel (Car : in out Vehicle) is
     begin
       if Car.Fuel_Quantity <= Car.Fuel_Minimum then
         raise Fuel_Exhausted; -- raise exception
       else -- decrement quantity
         Car.Fuel_Quantity := Car.Fuel_Quantity -
                              Current_Consumption;
       end if;
     end Consume_Fuel;
     ...
   end Automotive;

   procedure Joy_Ride is
     ...
   begin
     while not Bored loop
       Steer_Aimlessly (Bored);
       Consume_Fuel (Hot_Rod);
     end loop;
     Drive_Home;
   exception -- handle specified exception
     when Fuel_Exhausted =>
       Push_Home;
   end Joy_Ride;

----------
Handlers
----------

* Exception Handler Part

  * Contains the exception handlers within a frame
  * Separates normal processing code from abnormal
  * Starts with the reserved word :ada:`exception`
  * Optional

* Similar To Case Statements

   .. code:: Ada

      ...
      exception
        when Constraint_Error | Storage_Error | Program_Error =>
        ...
        when Tasking_Error =>
        ...
        when others =>
        ...
      end;

  * Handlers Don't "Fall Through" (as in case statements)

* When An Exception Is Raised

    * Normal processing is abandoned
    * Handler for active exception is executed, if any
    * Control then goes to the caller
    * If handled, caller continues normally, otherwise repeats the above

---------------------------------------------
Implicitly and Explicitly Raised Exceptions
---------------------------------------------

* Implicitly-Raised Exceptions

  * Correspond to language-defined checks
  * Can happen by statement execution
  * Can happen by declaration elaboration
  * Several checks and exceptions are language-defined

  * :ada:`Constraint_Error`

    * Caused by violations of constraints on range, index, etc.
    * The most common exceptions encountered

  * :ada:`Program_Error`

    * When runtime control structure is violated
    * When implementation detects bounded errors

  * :ada:`Storage_Error`

    * When insufficient storage is available
    * Potential causes

* Explicitly-Raised Exceptions

  * Raised by application via :ada:`raise` statements

-------------------------
User-Defined Exceptions
-------------------------

* Behave like predefined exceptions

* An important part of the abstraction

* Designer specifies how component can be used

.. code:: Ada

   package Stack is
     Underflow, Overflow : exception;
     procedure Push (Item : in Integer);
     procedure Pop (Item : out Integer);
     ...
   end Stack;

   package body Stack is
     procedure Push (Item : in Integer) is
     begin
       if Top = Index'Last then
         raise Overflow;
       end if;
       Top := Top + 1;
       Values (Top) := Item;
     end Push;

     procedure Pop (Item : out Integer) is
     begin
       if Top = 0 then
         raise Underflow;
       end if;
       Item := Values (Top);
       Top := Top - 1;
     end Pop;
   end Stack;

-------------
Propagation
-------------

* Control does not return to point of raising

* When a handler is not found in a block statement

   - Re-raised immediately after the block

* When a handler is not found in a subprogram

   - Propagated to caller at the point of call

* Propagation is dynamic, back up the call chain

   - Not based on textual layout or order of declarations

* Propagation stops at the main subprogram

   - Main completes abnormally unless handled

-----------------------
Exceptions as Objects
-----------------------

* Exceptions Are Not Objects

  * May not be manipulated
  * Some differences for scope and visibility

* But You Can Treat Them As Objects

  * For raising and handling, and more
  * Standard Library - :ada:`Ada.Exceptions` package

* `Exception_Occurrence` Query Functions

  * `Exception_Name`
  * `Exception_Message`
  * `Exception_Information`

---------------------
*Raise Expressions*
---------------------

* Expressions, of type defined by enclosing context
* Evaluation at run-time raises specified exception
* Syntax mimics :ada:`raise` statements

* As parts of conditional expressions, but when an exception is appropriate too

.. code:: Ada

   procedure Demo (X : Integer) is
     Error : exception;
     M : Integer;
     Foo : constant Integer := ( case X is
                                 when 1 => 10,
                                 when 2 => 20,
                                 when others => raise Error);
    begin
     M := (if Foo = 10 then 100 else
           raise Error with "Foo is not 10!");
     ...
   end Demo;

======================
Interfacing with C++
======================

-----------------
Import / Export
-----------------

* :ada:`Pragma Import` allows a C implementation to complete an Ada specification

  * Ada view

    .. code:: Ada

      procedure C_Proc;
      pragma Import (C, C_Proc, "c_proc");

  * C implementation

    .. code:: C++

      void c_proc (void) { printf("Hello World\n"); }

* :ada:`Pragma Export` allows an Ada implementation to complete a C specification

  * Ada implementation

    .. code:: Ada

      procedure Ada_Proc;
      pragma Export (C, Ada_Proc, "ada_proc");
      procedure Ada_Proc is
      begin
        Ada.Text_IO.Put_Line ("Hello World\n");
      end Ada_Proc;

  * C view

    .. code:: C++

      extern void ada_proc (void);
      void proc (void) { ada_proc(); }

* You can also import/export variables

* Import / Export in Ada 2012

  .. code:: Ada

    procedure C_Proc with Import,
                          Convention    => C,
                          External_Name => "c_proc";

-------------------
Parameter Passing
-------------------

* Parameter Passing to/from C

  * Mechanism used to pass subprogram parameters and results depends on:

    * The type of the parameter
    * The mode of the parameter
    * The Convention applied on the Ada side of the subprogram declaration.

* Passing Scalar Data as Parameters

  * C types are defined by the Standard
  * Ada types are implementation-defined
  * GNAT standard types are compatible with C types

* Passing Structures as Parameters

  * An Ada record that is mapping on a C struct must:

    * Be marked as convention C to enforce a C-like memory layout
    * Contain only C-compatible types

* Parameter modes

  * :ada:`in` scalar parameters passed by copy
  * :ada:`out` and :ada:`in out` scalars passed using temporary pointer on C side
  * By default, composite types passed by reference on all modes except when the type is marked `C_Pass_By_Copy`

--------------
Interfaces.C
--------------

* Interfaces.C Hierarchy

  * Ada supplies a subsystem to deal with Ada/C interactions

  * `Interfaces.C` - contains typical C types and constants, plus some simple Ada string to/from C character array conversion routines

=========
Tasking
=========

-------
Tasks
-------

* Rendezvous Definitions

  * **Server** declares several :ada:`entry`
  * Client calls entries
  * Server :ada:`accept` the client calls
  * At each standalone :ada:`accept`, server task **blocks**

* Rendezvous Entry Calls

  * Upon calling an :ada:`entry`, client **blocks**

.. code:: Ada

  task type Msg_Box_T is
    entry Start;
    entry Receive_Message (S : String);
    entry Stop;
  end Msg_Box_T;

  task body Msg_Box_T is
  begin
    accept Start; -- wait here until we're started
    -- now handle other entries
    select
      -- process the message and go back to waiting
      accept Receive_Message (V : String) do
        Put_Line ("Message : " & String);
      end Receive_Message;
    or
      -- we're done
      accept Stop;
      exit;
    end select;
  end Msg_Box_T;

-------------------
Protected Objects
-------------------

* **Passive** objects state

* Protected objects are :ada:`limited` types

* Protected: Functions and Procedures

  * A :ada:`function` can **get** the state
  * A :ada:`procedure` can **set** the state
  * In case of concurrency, other callers get **blocked**

.. code:: Ada

   protected type Protected_Value is
      procedure Set (V : Integer);
      function Get return Integer;
   private
      Value : Integer;
   end Protected_Value;

   protected body Protected_Value is
      procedure Set (V : Integer) is
      begin
         Value := V;
      end Set;

      function Get return Integer is
      begin
         return Value;
      end Get;
   end Protected_Value;

--------
Delays
--------

* :ada:`delay` keyword part of tasking
* Blocks for a time
* Relative: Blocks for at least :ada:`Duration`
* Absolute: Blocks until a given :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. code:: Ada

    Relative : Duration := Seconds(5.0);
    delay Relative;

    Absolute : Time := Time_Of (2030, 10, 30);
    delay until Absolute;

--------------------------
Task and Protected Types
--------------------------

* Task Activation

  * An instantiated task starts running when **activated**

  * On the stack

     * Activated when **enclosing** declarative part finishes its **elaboration**

  * On the heap

     * Activated **immediately** at instanciation

* Scope Of a Task

  * Tasks can be nested in **any** declarative block
  * A **subprogram** finishes **only** when all its **nested task** bodies are over
  * The **program** terminates when all **library-level tasks** finish

