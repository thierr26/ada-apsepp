-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Text_Class.R.Create_Single_Line
  (A : Character_Array) return RO_Text_Single_Line is

begin

   return (Ada.Finalization.Controlled
             with A => new Character_Array'(A));

end Apsepp.Text_Class.R.Create_Single_Line;
