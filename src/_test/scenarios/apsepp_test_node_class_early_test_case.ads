-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Test_Node_Class;          use Apsepp.Test_Node_Class;
with Apsepp.Abstract_Early_Test_Case; use Apsepp.Abstract_Early_Test_Case;

package Apsepp_Test_Node_Class_Early_Test_Case is

   type Apsepp_Test_Node_Class_E_T_C
     is limited new Early_Test_Case with null record;

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Node_Class_E_T_C) return Test_Routine;

end Apsepp_Test_Node_Class_Early_Test_Case;
