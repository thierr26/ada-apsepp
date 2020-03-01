-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Test_Node_Class.Abstract_Test_Suite;
  use Apsepp.Test_Node_Class.Abstract_Test_Suite;

private with Apsepp_Scope_Bound_Locking_Test_Case;

package Apsepp_Test_Suite is

   type Apsepp_T_S is new Test_Suite with null record;

   overriding
   function Child_Array (Obj : Apsepp_T_S) return Test_Node_Array;

private

   use Apsepp_Scope_Bound_Locking_Test_Case;

   Scope_Bound_Locking_T_C : aliased Apsepp_Scope_Bound_Locking_T_C;

end Apsepp_Test_Suite;
