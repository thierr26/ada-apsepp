-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp_Test_Suite is

   ----------------------------------------------------------------------------

   overriding
   function Child_Array (Obj : Apsepp_T_S) return Test_Node_Array
     is (Test_Node_Class_E_T_C'Access,
         Scope_Bound_Locks_T_C'Access,
         Shared_Instance_T_C'Access,
         Scope_Debug_T_C'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Suite;
