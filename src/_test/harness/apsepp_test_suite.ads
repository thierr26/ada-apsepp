-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Abstract_Test_Suite; use Apsepp.Abstract_Test_Suite;
private with Apsepp_Scope_Bound_Locks_Test_Case,
             Apsepp_Shared_Instance_Test_Case,
             Apsepp_Scope_Debug_Test_Case;

package Apsepp_Test_Suite is

   type Apsepp_T_S is limited new Test_Suite with private;

   overriding
   function Child_Array (Obj : Apsepp_T_S) return Child_Test_Node_Array;

private

   type Apsepp_T_S is limited new Test_Suite with null record;

   use Apsepp_Scope_Bound_Locks_Test_Case;
   use Apsepp_Shared_Instance_Test_Case;
   use Apsepp_Scope_Debug_Test_Case;

   Scope_Bound_Locks_T_C : aliased Apsepp_Scope_Bound_Locks_T_C;
   Shared_Instance_T_C   : aliased Apsepp_Shared_Instance_T_C;
   Scope_Debug_T_C       : aliased Apsepp_Scope_Debug_T_C;

end Apsepp_Test_Suite;
