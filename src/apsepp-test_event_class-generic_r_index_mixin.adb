-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Test_Event_Class.Generic_R_Index_Mixin is

   ----------------------------------------------------------------------------

   overriding
   procedure Set (Obj : in out Child_W_R_Index; Data : Test_Event_Data) is

   begin

      Parent (Obj).Set (Data); -- Inherited procedure call.

      Obj.R_Index := Data.R_Index;

   end Set;

   ----------------------------------------------------------------------------

   overriding
   function R_Index (Obj : Child_W_R_Index) return Test_Routine_Index
     is (Obj.R_Index);

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Generic_R_Index_Mixin;
