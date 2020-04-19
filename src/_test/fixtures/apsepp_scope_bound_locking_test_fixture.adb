-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

-- TODO: Use 'Apsepp.Generic_Safe_Counter' instead of
-- 'Apsepp.Generic_Safe_Integer_Operations'. <2020-04-03>
with Apsepp.Generic_Safe_Integer_Operations;

package body Apsepp_Scope_Bound_Locking_Test_Fixture is

   ----------------------------------------------------------------------------

   package Safe_Integer_Operations
     is new Apsepp.Generic_Safe_Integer_Operations (Integer_Type => Integer);

   use Safe_Integer_Operations;

   ----------------------------------------------------------------------------

   Lock_Count_Var : Safe_Integer;

   ----------------------------------------------------------------------------

   overriding
   procedure Set_Up (Obj : Apsepp_Scope_Bound_Locking_T_F) is

      pragma Unreferenced (Obj);

   begin

      Lock_Count_Var := Create (0);

   end Set_Up;

   ----------------------------------------------------------------------------

   not overriding
   function Lock_Count (Obj : Apsepp_Scope_Bound_Locking_T_F) return Natural
     is (Val (Lock_Count_Var));

   ----------------------------------------------------------------------------

   overriding
   procedure On_Lock (Obj : Test_Lock) is

      pragma Unreferenced (Obj);

   begin

      Inc (Lock_Count_Var);

   end On_Lock;

   ----------------------------------------------------------------------------

end Apsepp_Scope_Bound_Locking_Test_Fixture;
