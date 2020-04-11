-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Text_Class.R.Create_Single_Line;

function Apsepp.Text_Class.R.Single_Line_From_String
  (Line : String) return RO_Text_Single_Line is

   -- Sliding makes the conversion to 'Character_Array' below safer. (For the
   -- case where 'Positive'Pos (Line'Last)
   --               >
   --             Character_Index'Pos (Character_Index'Last'.)
   Slided_Line : constant String (1 .. Line'Length) := Line;

begin

   return Create_Single_Line (Character_Array (Slided_Line));

end Apsepp.Text_Class.R.Single_Line_From_String;
