-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Event_Class.Generic_Assert_Num_Mixin is

   ----------------------------------------------------------------------------

   overriding
   procedure Set (Obj : in out Child_W_Assert_Num; Data : Test_Event_Data) is

   begin

      Parent (Obj).Set (Data); -- Inherited procedure call.

      Obj.Assert_Num := Data.Assert_Num;

   end Set;

   ----------------------------------------------------------------------------

   overriding
   function Assert_Num (Obj : Child_W_Assert_Num) return Test_Assert_Count
     is (Obj.Assert_Num);

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Generic_Assert_Num_Mixin;
