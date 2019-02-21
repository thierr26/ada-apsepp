-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp_Scope_Bound_Locks_Test_Fixture is

   ----------------------------------------------------------------------------

   N_Var : Integer := 0;

   ----------------------------------------------------------------------------

   procedure Increment_N is

   begin

      N_Var := N_Var + 1;

   end Increment_N;

   ----------------------------------------------------------------------------

   procedure Decrement_N is

   begin

      N_Var := N_Var - 1;

   end Decrement_N;

   ----------------------------------------------------------------------------

   not overriding
   procedure Reset (Obj : Scope_Bound_Locks_Test_Fixture) is

      pragma Unreferenced (Obj);

   begin

      N_Var := 0;

   end Reset;

   ----------------------------------------------------------------------------

   not overriding
   function N (Obj : Scope_Bound_Locks_Test_Fixture) return Integer
     is (N_Var);

   ----------------------------------------------------------------------------

end Apsepp_Scope_Bound_Locks_Test_Fixture;
