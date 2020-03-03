-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Test_Case;
  use Apsepp.Test_Node_Class.Abstract_Test_Case;
  use Apsepp.Test_Node_Class;

private with Apsepp.Scope_Bound_Locking.Generic_Lock_Holder_Array,
             Apsepp.Test_Node_Class.Abstract_Test_Case.Generic_Assert,
             Apsepp_Scope_Bound_Locking_Test_Fixture.Restricted_Access;

package Apsepp_Scope_Bound_Locking_Test_Case is

   type Apsepp_Scope_Bound_Locking_T_C
     is limited new Test_Case with null record;

   overriding
   function Routine_Array
     (Obj : Apsepp_Scope_Bound_Locking_T_C) return Test_Routine_Array;

   overriding
   procedure Run
     (Obj     : in out Apsepp_Scope_Bound_Locking_T_C;
      Outcome :    out Test_Outcome;
      Kind    :        Run_Kind := Assert_Cond_And_Run_Test);

private

   use Apsepp.Scope_Bound_Locking;

   procedure Assert
     is new Apsepp.Test_Node_Class.Abstract_Test_Case.Generic_Assert
     (Test_Case_Tag => Apsepp_Scope_Bound_Locking_T_C'Tag);

   type Lock_Identifier is (SBL);

   package SBL_T_F is

      use Apsepp_Scope_Bound_Locking_Test_Fixture,
          Apsepp_Scope_Bound_Locking_Test_Fixture.Restricted_Access
            .Apsepp_Scope_Bound_Locking_T_F_Restr;

      L_H : aliased Lock_Holder (L => Fixture_Instance_Lock'Access);

      function I_A return not null access Apsepp_Scope_Bound_Locking_T_F
        is (Fixture_Instance_Access (L_H));

   end SBL_T_F;

   package Lock_Holder_Array_Package
     is new Generic_Lock_Holder_Array (Index_Type => Lock_Identifier);

   use Lock_Holder_Array_Package;

   L_H_A : Lock_Holder_Array := (SBL => SBL_T_F.L_H'Access);

end Apsepp_Scope_Bound_Locking_Test_Case;
