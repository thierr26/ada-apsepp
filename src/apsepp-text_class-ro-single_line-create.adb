-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

function Apsepp.Text_Class.RO.Single_Line.Create
  (Line : String) return RO_Text_Single_Line is

   Slided_Line : constant Character_Array (1 .. Line'Length)
     := Character_Array (Line);

begin

   return (Ada.Finalization.Controlled
             with A => new Character_Array'(Slided_Line));

end Apsepp.Text_Class.RO.Single_Line.Create;
