-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp_Test_Suite is

   ----------------------------------------------------------------------------

   overriding
   function Child_Array (Obj : Apsepp_T_S) return Child_Test_Node_Array
     is (Scope_Bound_Locks_T_C'Access,
         Shared_Instance_T_C'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Suite;
