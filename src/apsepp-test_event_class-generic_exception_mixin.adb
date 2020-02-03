-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Unchecked_Deallocation;

package body Apsepp.Test_Event_Class.Generic_Exception_Mixin is

   ----------------------------------------------------------------------------

   overriding
   procedure Set (Obj : in out Child_W_Exception; Data : Test_Event_Data) is

   begin

      Parent (Obj).Set (Data); -- Inherited procedure call.

      Child_W_Exception'Class (Obj).Free_Exception;

      Obj.E := Data.E;

   end Set;

   ----------------------------------------------------------------------------

   overriding
   procedure Clean_Up (Obj : in out Child_W_Exception) is

   begin

      Parent (Obj).Clean_Up; -- Inherited procedure call.

      Child_W_Exception'Class (Obj).Free_Exception;

   end Clean_Up;

   ----------------------------------------------------------------------------

   function Exception_Access
     (Obj : Child_W_Exception) return Exception_Occurrence_Access
     is (Obj.E);

   ----------------------------------------------------------------------------

   procedure Free_Exception (Obj : in out Child_W_Exception) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Exception_Occurrence,
         Name   => Exception_Occurrence_Access);

   begin

      Free (Obj.E);

   end Free_Exception;

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Generic_Exception_Mixin;
