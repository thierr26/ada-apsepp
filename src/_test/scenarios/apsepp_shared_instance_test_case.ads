-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Generic_Assert; use Apsepp.Test_Node_Class;
with Apsepp.Abstract_Test_Case;             use Apsepp.Abstract_Test_Case;

package Apsepp_Shared_Instance_Test_Case is

   type Apsepp_Shared_Instance_T_C
     is limited new Test_Case with null record;

   overriding
   procedure Setup_Routine (Obj : Apsepp_Shared_Instance_T_C);

   overriding
   function Routine_Array (Obj : Apsepp_Shared_Instance_T_C)
     return Test_Routine_Array;

   overriding
   procedure Run
     (Obj     : in out Apsepp_Shared_Instance_T_C;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind                   := Assert_Cond_And_Run_Test);

private

   procedure Assert is new Apsepp.Test_Node_Class.Generic_Assert
     (Test_Node_Tag => Apsepp_Shared_Instance_T_C'Tag);

end Apsepp_Shared_Instance_Test_Case;
