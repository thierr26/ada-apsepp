-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic
   type Parent is new Test_Event_Base with private;
package Apsepp.Test_Event_Class.Generic_Assert_Num_Mixin is

   type Child_W_Assert_Num is new Parent with private

     with Type_Invariant'Class => Child_W_Assert_Num.Has_Assert_Num;

   overriding
   procedure Set (Obj : in out Child_W_Assert_Num; Data : Test_Event_Data);

   overriding
   function Has_Assert_Num (Obj : Child_W_Assert_Num) return Boolean
     is (True);

   overriding
   function Assert_Num (Obj : Child_W_Assert_Num) return Test_Assert_Count;

private

   type Child_W_Assert_Num is new Parent with record
      Assert_Num : Test_Assert_Count;
   end record;

end Apsepp.Test_Event_Class.Generic_Assert_Num_Mixin;
