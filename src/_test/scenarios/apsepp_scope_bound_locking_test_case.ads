-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Test_Case;
  use Apsepp.Test_Node_Class.Abstract_Test_Case;
  use Apsepp.Test_Node_Class;

private with Apsepp.Scope_Bound_Locking,
             Apsepp.Test_Node_Class.Abstract_Test_Case.Generic_Assert,
             Apsepp_Scope_Bound_Locking_Test_Fixture.Restricted_Access;

package Apsepp_Scope_Bound_Locking_Test_Case is

   type Apsepp_Scope_Bound_Locking_T_C
     is limited new Test_Case
     with null record;

   overriding
   function Routine_Array
     (Obj : Apsepp_Scope_Bound_Locking_T_C) return Test_Routine_Array;

   overriding
   procedure Run
     (Obj     : in out Apsepp_Scope_Bound_Locking_T_C;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test);

private

   procedure Assert
     is new Apsepp.Test_Node_Class.Abstract_Test_Case.Generic_Assert
     (Test_Case_Tag => Apsepp_Scope_Bound_Locking_T_C'Tag);

   use Apsepp_Scope_Bound_Locking_Test_Fixture.Restricted_Access
     .Apsepp_Scope_Bound_Locking_T_F_Restr;

   SBL_TF_LA : not null access Fixture_Lock
     := Fixture_Instance_Lock'Access;

end Apsepp_Scope_Bound_Locking_Test_Case;
