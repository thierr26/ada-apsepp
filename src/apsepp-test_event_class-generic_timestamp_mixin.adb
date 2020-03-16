-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Test_Event_Class.Generic_Timestamp_Mixin is

   ----------------------------------------------------------------------------

   overriding
   procedure Set (Obj : in out Child_W_Timestamp; Data : Test_Event_Data) is

   begin

      Parent (Obj).Set (Data); -- Inherited procedure call.

      Obj.Date := Data.Date;

   end Set;

   ----------------------------------------------------------------------------

   overriding
   function Timestamp (Obj : Child_W_Timestamp) return Time
     is (Obj.Date);

   ----------------------------------------------------------------------------

   overriding
   procedure Set_Timestamp (Obj  : in out Child_W_Timestamp;
                            Date :        Time              := Clock) is

   begin

      Obj.Date := Date;

   end Set_Timestamp;

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Generic_Timestamp_Mixin;
