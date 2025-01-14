-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2009, AdaCore                  --
--                                                                   --
-- Labs is free  software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Real_Time;                     use Ada.Real_Time;

with Display;                           use Display;
with Display.Basic;                     use Display.Basic;
with Libm_Single;                       use Libm_Single;


procedure Main is
   -- define type Bodies_Enum_T as an enumeration of Sun, Earth, Moon, Satellite
   type Bodies_Enum_T is (Sun, Earth, Moon, Satellite);

   -- define type Parameters_Enum_T as an enumeration of parameter X, Y, Radius, Speed, Distance, Angle
   type Parameters_Enum_T is (X, Y, Radius, Speed, Distance, Angle);

   -- define type Bodies_Array_T as an array of float indexed by bodies and parameters
   type Bodies_Array_T is array (Bodies_Enum_T, Parameters_Enum_T) of Float;

   -- define type Colors_Array_T as an array of color (RGBA_T) indexed by bodies
   type Colors_Array_T is array (Bodies_Enum_T) of RGBA_T;

   -- declare variable Bodies which is an instance of Bodies_Array_T
   Bodies : Bodies_Array_T;

   -- declare variable Colors which is an instance of Colors_Array_T
   Colors : Colors_Array_T;

   -- declare a variable Next of type Time to store the Next step time
   Next : Time;

   -- declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
   Period  : constant Time_Span := Milliseconds (40);

   -- reference to the application window
   Window : Window_ID;

   -- reference to the graphical canvas associated with the application window
   Canvas : Canvas_ID;

begin

   -- Create a window 240x320
   Window := Create_Window(Width  => 240,
                           Height => 320,
                           Name   => "Solar System");

   -- Retrieve the graphical canvas from the window
   Canvas := Get_Canvas (Window);

   -- initialize Bodies variable with parameters for each body using an aggregate
   --    Sun Distance = 0.0, Angle = 0.0, Speed = 0.0, Radius = 20.0;
   --    Earth Distance = 50.0, Angle = 0.0, Speed = 0.02, Radius = 5.0;
   --    Moon Distance = 15.0, Angle = 0.0, Speed = 0.04, Radius = 2.0;
   --    Satellite Distance = 8.0, Angle = 0.0, Speed = 0.1, Radius = 1.0;
   Bodies := (Sun => (Distance => 0.0,
                      Speed => 0.0,
                      Radius => 20.0,
                      X => 0.0,
                      Y => 0.0,
                      Angle => 0.0),
              Earth => (Distance => 50.0,
                        Speed => 0.02,
                        Radius => 5.0,
                        X => 0.0,
                        Y => 0.0,
                        Angle => 0.0),
              Moon => (Distance => 15.0,
                       Speed => 0.04,
                       Radius => 2.0,
                       X => 0.0,
                       Y => 0.0,
                       Angle => 0.0),
              Satellite => (Distance => 8.0,
                            Speed => 0.1,
                            Radius => 1.0,
                            X => 0.0,
                            Y => 0.0,
                            Angle => 0.0));

   -- initialize Colors variable with Sun is Yellow, Earth is Blue, Moon is White, Satellite is Red
   Colors := (Sun => Yellow,
              Earth => Blue,
              Moon => White,
              Satellite => Red);


   --  initialize the Next step time begin the current time (Clock) + the period
   Next := Clock + Period;


   while not Is_Killed loop

      --  create a loop to update each body position and angles
      --    the position of an object around (0,0) at distance d with an angle a
      --    is (d*cos(a), d*sin(a))
      --  update angle parameter of each body adding speed to the previous angle

      for B in Earth .. Satellite loop
         --  This solution illustrates the use of a block statement with
         --  local constants to reduce repetition and improve readability
         --  in the loop body.
         declare
            This_Pred : constant Bodies_Enum_T := Bodies_Enum_T'Pred(B);
            D         : constant Float := Bodies (B, Distance);
            A         : constant Float := Bodies (B, Angle);
         begin
            Bodies (B, X) := Bodies (This_Pred, X) + D * Cos (A);
            Bodies (B, Y) := Bodies (This_Pred, Y) + D * Sin (A);
            Bodies (B, Angle) := A + Bodies (B, Speed);
         end;
      end loop;

      --  loop to draw every objects

      for B in Bodies_Enum_T loop
         Draw_Sphere(Canvas   => Canvas,
                     Position => (Bodies (B, X), Bodies (B, Y), 0.0),
                     Radius   => Bodies (B, Radius),
                     Color    => Colors(B));
      end loop;

      --  update the screen using procedure Swap_Buffers
      Swap_Buffers(Window);

      -- wait until Next
      delay until Next;

      --  update the Next time adding the period for the next step
      Next := Next + Period;

   end loop;

end Main;
