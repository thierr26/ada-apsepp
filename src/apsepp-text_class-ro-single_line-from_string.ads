-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- Force run-time pre-condition check in this unit.
pragma Assertion_Policy (Pre => Check);

-- TODOC: The only input checking done is the pre-condition evaluation. No
-- search for end of lines sequences, escape sequences or any other special
-- sequences. <2020-04-10>
function Apsepp.Text_Class.RO.Single_Line.From_String
  (Line : String) return RO_Text_Single_Line
  with Pre => Line'Length <= Character_Count'Last;
