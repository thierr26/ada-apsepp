-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic
   type Parent is abstract new Test_Event_Base with private;
package Apsepp.Test_Event_Class.Generic_R_Index_Mixin is

   type Child_W_R_Index is new Parent with private

     with Type_Invariant'Class => Child_W_R_Index.Has_R_Index;

   overriding
   procedure Set (Obj : in out Child_W_R_Index; Data : Test_Event_Data);

   overriding
   function Has_R_Index (Obj : Child_W_R_Index) return Boolean
     is (True);

   overriding
   function R_Index (Obj : Child_W_R_Index) return Test_Routine_Index;

private

   type Child_W_R_Index is new Parent with record
      R_Index : Test_Routine_Index;
   end record;

end Apsepp.Test_Event_Class.Generic_R_Index_Mixin;
