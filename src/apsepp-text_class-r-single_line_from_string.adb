-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Text_Class.R.Create_Single_Line;

function Apsepp.Text_Class.R.Single_Line_From_String
  (Line : String) return RO_Text_Single_Line is

begin

   if Positive'Pos (Line'Last)
        >
      Character_Index'Pos (Character_Index'Last) then
      -- A conversion of 'Line' to type 'Character_Array' would raise
      -- 'Constraint_Error'. Sliding solves the issue. We know, thanks to the
      -- pre-condition, that type 'Character_Array' can accomodate 'Line'.

      declare
         Slided_Line : constant String (1 .. Line'Length) := Line;
      begin
         return Create_Single_Line (Character_Array (Slided_Line));
                                                               -- Early return.
      end;

   end if;

   -- We get there only if the conversion of 'Line' to type 'Character_Array'
   -- is safe.

   return Create_Single_Line (Character_Array (Line));

end Apsepp.Text_Class.R.Single_Line_From_String;
