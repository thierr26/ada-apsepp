-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp_Shared_Instance_Test_Fixture is

   ----------------------------------------------------------------------------

   Lock_Count_Var     : Natural := 0;
   Unlock_Count_Var   : Natural := 0;
   CB_Calls_Count_Var : Natural := 0;

   ----------------------------------------------------------------------------

   procedure Increment_Lock_Count is

   begin

      Lock_Count_Var := Lock_Count_Var + 1;

   end Increment_Lock_Count;

   ----------------------------------------------------------------------------

   procedure Increment_Unlock_Count is

   begin

      Unlock_Count_Var := Unlock_Count_Var + 1;

   end Increment_Unlock_Count;

   ----------------------------------------------------------------------------

   procedure CB is

   begin

      CB_Calls_Count_Var := CB_Calls_Count_Var + 1;

   end CB;

   ----------------------------------------------------------------------------

   not overriding
   procedure Reset (Obj : Shared_Instance_Test_Fixture) is

      pragma Unreferenced (Obj);

   begin

      Lock_Count_Var     := 0;
      Unlock_Count_Var   := 0;
      CB_Calls_Count_Var := 0;

   end Reset;

   ----------------------------------------------------------------------------

   not overriding
   function Lock_Count (Obj : Shared_Instance_Test_Fixture) return Natural
     is (Lock_Count_Var);

   ----------------------------------------------------------------------------

   not overriding
   function Unlock_Count (Obj : Shared_Instance_Test_Fixture) return Natural
     is (Unlock_Count_Var);

   ----------------------------------------------------------------------------

   not overriding
   function CB_Calls_Count (Obj : Shared_Instance_Test_Fixture) return Natural
     is (CB_Calls_Count_Var);

   ----------------------------------------------------------------------------

end Apsepp_Shared_Instance_Test_Fixture;
