-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp_Test_Suite is

   ----------------------------------------------------------------------------

   overriding
   function Child_Array (Obj : Apsepp_T_S) return Test_Node_Array
     is (1 => Scope_Bound_Locking_T_C'Access);

   ----------------------------------------------------------------------------

end Apsepp_Test_Suite;
