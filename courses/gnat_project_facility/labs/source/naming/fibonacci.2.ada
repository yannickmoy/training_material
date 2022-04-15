with Ada.Text_IO; use Ada.Text_IO;
package body Fibonacci is
   procedure Perform (Count : Positive) is

      package Io is new Ada.Text_Io.Integer_Io (Long_Long_Integer);
      use Io;

      Prev, Next : Long_Long_Integer;
      Result     : Long_Long_Integer;

   begin
      Prev := 0;
      Next := 1;
      for K in 1 .. Count
      loop
         Result := Prev + Next;
         Put (Long_Long_Integer(K));
         Put ("  ");
         Put (Result);
         New_Line;
         Prev := Next;
         Next := Result;
      end loop;
   end Perform;
end Fibonacci;
