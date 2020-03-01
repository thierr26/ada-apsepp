-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   with procedure Work (Obj     :     Test_Node_Interfa'Class;
                        Outcome : out Test_Outcome);

procedure Apsepp.Test_Node_Class.Generic_Case_And_Suite_Run_Body
  (Obj     :     Test_Node_Interfa'Class;
   Outcome : out Test_Outcome;
   Kind    :     Run_Kind;
   Cond    :     not null access function return Boolean);
