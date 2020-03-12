-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Early_Test_Case;
  use Apsepp.Test_Node_Class.Abstract_Early_Test_Case;
  use Apsepp.Test_Node_Class;

private with Apsepp.Scope_Bound_Locking.Generic_Lock_Holder_Array,
             Apsepp_Testing_System_Test_Fixture.Restricted_Access;

package Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case is

   type Apsepp_Test_Node_Class_Abstract_Test_Case_E_T_C
     is limited new Early_Test_Case with null record;

   overriding
   function Early_Routine
     (Obj : Apsepp_Test_Node_Class_Abstract_Test_Case_E_T_C)
     return not null access procedure;

private

   use Apsepp.Scope_Bound_Locking;

   type Lock_Identifier is (TS);

   package TS_T_F is

      use Apsepp_Testing_System_Test_Fixture,
          Apsepp_Testing_System_Test_Fixture.Restricted_Access
            .Apsepp_Testing_System_T_F_Restr;

      L_H : aliased Lock_Holder (L => Fixture_Instance_Lock'Access);

      function I_A
        return not null access Apsepp_Testing_System_T_F
        is (Fixture_Instance_Access (L_H));

   end TS_T_F;

   package Lock_Holder_Array_Package
     is new Generic_Lock_Holder_Array (Index_Type => Lock_Identifier);

   use Lock_Holder_Array_Package;

   L_H_A : Lock_Holder_Array := (TS => TS_T_F.L_H'Access);

end Apsepp_Test_Node_Class_Abstract_Test_Case_Early_Test_Case;
