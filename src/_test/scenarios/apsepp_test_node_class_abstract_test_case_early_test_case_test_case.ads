-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Early_Test_Case;
  use Apsepp.Test_Node_Class.Abstract_Early_Test_Case;

package Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case_Test_Case is

   type Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case_T_C
     is limited new Early_Test_Case with null record;

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case_T_C)
     return not null access procedure;

end Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case_Test_Case;
