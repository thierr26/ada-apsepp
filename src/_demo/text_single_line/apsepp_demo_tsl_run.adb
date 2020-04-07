-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO,
     Apsepp.Text_Class.RO.Single_Line.Create;

package body Apsepp_Demo_TSL_Run is

   ----------------------------------------------------------------------------

   procedure Run is

      use Ada.Text_IO,
          Apsepp.Text_Class.RO.Single_Line;

      -----------------------------------------------------

      procedure Put_String (S : String) is
      begin
         Put_Line ('_' & S & '_');
      end Put_String;

      -----------------------------------------------------

      Empty : constant RO_Text_Single_Line := Create ("");
      Hello : constant RO_Text_Single_Line := Create ("Hello world!");

   begin

      Put_Line ("'Empty':");
      Put_String (Empty.To_String);

      New_Line;

      Put_Line ("'Hello':");
      Put_String (Hello.To_String);

      New_Line;

      Put_Line ("'Hello', via iterator:");
      for A of Hello loop
         Put_String (String (A));
      end loop;

      New_Line;

      declare

         use all type Apsepp.Text_Class.RO.Cursor;

      begin

         Put_Line ("'Hello', via iterator (with cursor loop parameter):");
         for K in Hello.Iterate loop
            Put_String (String (Line (K).all));
         end loop;

      end;

      New_Line;

      declare

         use all type Apsepp.Text_Class.RO.Cursor;

         Start : constant Apsepp.Text_Class.RO.Cursor := Hello.To_Cursor;

      begin

         Put_Line ("'Hello', via iterator (with cursor loop parameter and "
                   & "start cursor):");
         for K in Hello.Iterate (Start) loop
            Put_String (String (Line (K).all));
         end loop;

      end;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Demo_TSL_Run;
