-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Event_Class.Generic_Routine_Index_Mixin is

   ----------------------------------------------------------------------------

   overriding
   procedure Set (Obj  : in out Child_W_Routine_Index;
                  Data :        Test_Event_Data) is

   begin

      Parent (Obj).Set (Data); -- Inherited procedure call.

      Obj.Routine_Index := Data.Routine_Index;

   end Set;

   ----------------------------------------------------------------------------

   overriding
   function Routine_Index
     (Obj : Child_W_Routine_Index) return Test_Routine_Index
     is (Obj.Routine_Index);

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Generic_Routine_Index_Mixin;
