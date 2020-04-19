-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Text_Class.R.Create_Multi_Line
  (A        : Character_Array;
   New_Line : New_Line_Index_Array := Null_New_Line_Index_Array)
  return RO_Text_Multi_Line is

begin

   return (Ada.Finalization.Controlled
             with A => new Character_Array'(A),
                  N => new New_Line_Index_Array'(New_Line));

end Apsepp.Text_Class.R.Create_Multi_Line;
