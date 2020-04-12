-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO,
     Apsepp.Text_Class.R.Single_Line_From_String;

package body Apsepp_Demo_TSL_Run is

   ----------------------------------------------------------------------------

   procedure Run is

      use Ada.Text_IO,
          Apsepp.Text_Class.R;

      -----------------------------------------------------

      procedure Put_String (S : String) is
      begin
         Put_Line ('_' & S & '_');
      end Put_String;

      -----------------------------------------------------

      Default : RO_Text_Single_Line;

      -- Here 'From_String' is used to create the 'RO_Text_Single_Line'
      -- objects. 'Apsepp.Text_Class.R.Create_Single_Line' may have been used
      -- instead. It takes a 'Apsepp.Text_Class.Character_Array' argument
      -- instead of a 'String' argument, but string literals like '""' and
      -- '"Hello world!"' are viewed as values of type 'String' or array
      -- subaggregates (which don't have a type) and are valid values for any
      -- 'Character' array.
      -- REF: ARM 2.6(1). <2020-04-10>
      -- REF: Annotated ARM 4.3.3(6.a). <2020-04-10>
      Empty : constant RO_Text_Single_Line
        := Single_Line_From_String ("");
      Hello : constant RO_Text_Single_Line
        := Single_Line_From_String ("Hello world!");

   begin

      Put_Line ("'Default':");
      Put_String (Default.To_String);

      New_Line;

      Put_Line ("'Empty':");
      Put_String (Empty.To_String);

      New_Line;

      Put_Line ("'Hello':");
      Put_String (Hello.To_String);

      New_Line;

      declare

         use Apsepp.Text_Class; -- Makes 'Character_Array' use visible.

         Position       : constant Cursor          := Hello.To_Cursor;
         Hello_Position : constant Character_Array := Hello(Position);

      begin

         Put_Line ("'Hello(Position)':");
         Put_String (String (Hello_Position));

         -- QUEST: The following line does not compile ("ambiguous operand in
         -- conversion"). Don't understand why. <2020-04-09>
         -- Put_String (String (Hello(Position)));

      end;

      New_Line;

      declare

         Hello_1 : constant Apsepp.Text_Class.Character_Array := Hello(1);

      begin

         Put_Line ("'Hello(1)':");
         Put_String (String (Hello_1));

         -- QUEST: The following line does not compile ("ambiguous operand in
         -- conversion"). Don't understand why. <2020-04-09>
         -- Put_String (String (Hello(1)));

      end;

      New_Line;

      Put_Line ("'Hello', via iterator:");
      for A of Hello loop
         Put_String (String (A));
      end loop;

      New_Line;

      Put_Line ("'Hello', via iterator (with cursor loop parameter):");
      for K in Hello.Iterate loop
         Put_String (String (Line (K).all));
      end loop;

      New_Line;

      declare

         Start : constant Cursor := Hello.To_Cursor;

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
