-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- TODOC: No search for end of lines sequences, escape sequences or any other
-- special sequences. <2020-04-10>
function Apsepp.Text_Class.RO.Single_Line.Create
  (A : Character_Array) return RO_Text_Single_Line;
