-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body;

private package Apsepp.Test_Node_Class.Private_Suite_Run_Body is

   procedure Run_Children (Obj     :     Test_Node_Interfa'Class;
                           Outcome : out Test_Outcome);

   procedure Run_Body
     is new Generic_Case_And_Suite_Run_Body (Work => Run_Children);

end Apsepp.Test_Node_Class.Private_Suite_Run_Body;
