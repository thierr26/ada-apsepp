-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic
   type Parent is abstract new Test_Event_Base with private;
package Apsepp.Test_Event_Class.Generic_Timestamp_Mixin is

   type Child_W_Timestamp is new Parent with private

     with Type_Invariant'Class => Child_W_Timestamp.Has_Timestamp;

   overriding
   procedure Set (Obj : in out Child_W_Timestamp; Data : Test_Event_Data);

   overriding
   function Has_Timestamp (Obj : Child_W_Timestamp) return Boolean
     is (True);

   overriding
   function Timestamp (Obj : Child_W_Timestamp) return Time;

   overriding
   procedure Set_Timestamp (Obj  : in out Child_W_Timestamp;
                            Date :        Time              := Clock);

private

   type Child_W_Timestamp is new Parent with record
      Date : Time;
   end record;

end Apsepp.Test_Event_Class.Generic_Timestamp_Mixin;
