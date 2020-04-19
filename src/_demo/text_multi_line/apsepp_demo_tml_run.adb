-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO,
     Apsepp.Text_Class.R.Multi_Line_From_String;

package body Apsepp_Demo_TML_Run is

   ----------------------------------------------------------------------------

   procedure Run is

      use Ada.Text_IO,
          Apsepp.Text_Class,
          Apsepp.Text_Class.R;

      -----------------------------------------------------

      function Delimiter (Length : Natural) return String
        is (if Length = 0 then
               "_"
            else
               "");

      -----------------------------------------------------

      procedure Put_String (S : String) is
         Delim : constant String := Delimiter (S'Length);
      begin
         Put_Line (Delim & S & Delim);
      end Put_String;

      -----------------------------------------------------

      procedure Put_Text_Line (Line : Character_Array) is
         Delim : constant String := Delimiter (Line'Length);
      begin
         Put_Line (Delim & String (Line) & Delim);
      end Put_Text_Line;

      -----------------------------------------------------

      Default : RO_Text_Multi_Line;

      -- Here 'From_String' is used to create the 'RO_Text_Multi_Line'
      -- objects. 'Apsepp.Text_Class.R.Create_Multi_Line' may have been used
      -- instead. It takes a 'Apsepp.Text_Class.Character_Array' argument
      -- instead of a 'String' argument, but string literals like '""' and
      -- '"Hello world!"' are viewed as values of type 'String' or array
      -- subaggregates (which don't have a type) and are valid values for any
      -- 'Character' array.
      -- REF: ARM 2.6(1). <2020-04-17>
      -- REF: Annotated ARM 4.3.3(6.a). <2020-04-17>

      Empty_Line : constant RO_Text_Multi_Line
        := Multi_Line_From_String ("");

      Hello : constant RO_Text_Multi_Line
        := Multi_Line_From_String ("Hello world!");

      Poem_Intro : constant RO_Text_Multi_Line
        := Multi_Line_From_String
             (Joined_Lines => "Here is a poem by:"
                              & "Walt Whitman (1819-1892)",
              New_Line     => (1, 19, 19, 43));

      Poem : constant RO_Text_Multi_Line
        := Multi_Line_From_String
             (Joined_Lines =>
                "Beginning my studies"
                & "Beginning my studies the first step pleas'd me so much,"
                & "The mere fact consciousness, these forms, the power of "
                                                                    & "motion,"
                & "The least insect or animal, the senses, eyesight, love,"
                & "The first step I say awed me and pleas'd me so much,"
                & "I have hardly gone and hardly wish'd to go any farther,"
                & "But stop and loiter all the time to sing it in ecstatic "
                                                                    & "songs.",
              New_Line     => (21, 21, 76, 138, 193, 245, 300));

   begin

      Put_Line ("'Default' ('Is_Empty' primitive is "
                &  Boolean'Image (Default.Is_Empty)
                & "):");
      Put_String (Default.To_String);

      New_Line;

      Put_Line ("'Empty_Line' ('Is_Empty' primitive is "
                &  Boolean'Image (Empty_Line.Is_Empty)
                & "):");
      Put_String (Empty_Line.To_String);

      New_Line;

      Put_Line ("'Hello' ('Is_Empty' primitive is "
                &  Boolean'Image (Hello.Is_Empty)
                & "):");
      Put_String (Hello.To_String);

      for K in Poem_Intro.Iterate loop
         Put_Text_Line (Line (K));
      end loop;

      New_Line;

      for A of Poem loop
         Put_Text_Line (A);
      end loop;

      New_Line;

      declare

         Position      : constant Cursor          := Poem.To_Cursor (4);
         Poem_Position : constant Character_Array := Poem(Position);

      begin

         Put_Line ("'Poem(Position)':");
         Put_Text_Line (Poem_Position);

         -- QUEST: The following line does not compile ("ambiguous operand in
         -- conversion"). Don't understand why. <2020-04-19>
         -- Put_String (String (Poem(Position)));

      end;

      New_Line;

      declare

         Poem_5 : constant Character_Array := Poem(5);

      begin

         Put_Line ("'Poem(5)':");
         Put_Text_Line (Poem_5);

         -- QUEST: The following line does not compile ("ambiguous operand in
         -- conversion"). Don't understand why. <2020-04-19>
         -- Put_String (String (Poem(1)));

      end;

      New_Line;

      declare

         Start : constant Cursor := Poem.To_Cursor (6);

      begin

         Put_Line ("'Poem', via iterator (with cursor loop parameter and "
                   & "start cursor):");
         for K in Poem.Iterate (Start) loop
            Put_Text_Line (Line (K));
         end loop;

      end;

   end Run;

   ----------------------------------------------------------------------------

end Apsepp_Demo_TML_Run;
