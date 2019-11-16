-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Test_Event_Class.Impl is

   ----------------------------------------------------------------------------

   package body Derivation is

      -----------------------------------------------------

      overriding
      procedure Set (Obj  : in out Test_Event_FCTNA;
                     Data :        Test_Event_Data) is

      begin

         Test_Event (Obj).Set (Data); -- Inherited procedure call.

         Obj.Previous_Child_Tag := Data.Previous_Child_Tag;

      end Set;

      -----------------------------------------------------

      overriding
      function Previous_Child_Tag (Obj : Test_Event_FCTNA) return Tag is

      begin

         return Obj.Previous_Child_Tag;

      end Previous_Child_Tag;

      -----------------------------------------------------

      overriding
      procedure Set (Obj  : in out Test_Event_TRC;
                     Data :        Test_Event_Data) is

      begin

         Test_Event (Obj).Set (Data); -- Inherited procedure call.

         Obj.Last_Cancelled_R_Index := Data.R_Index;

      end Set;

      -----------------------------------------------------

      overriding
      function Last_Cancelled_R_Index (Obj : Test_Event_TRC)
        return Test_Routine_Index is

      begin

         return Obj.Last_Cancelled_R_Index;

      end Last_Cancelled_R_Index;

      -----------------------------------------------------

   end Derivation;

   ----------------------------------------------------------------------------

end Apsepp.Test_Event_Class.Impl;
