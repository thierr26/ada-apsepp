-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO;

package body Apsepp.Debug_Trace_Class.Standard is

   ----------------------------------------------------------------------------

   protected body Debug_Trace_Standard is

      -----------------------------------------------------

      procedure Set_Up (Time_Fraction_Digits : Natural) is

      begin

         Debug_Trace_Quiet_Instance.Set_Up (Time_Fraction_Digits);

      end Set_Up;

      -----------------------------------------------------

      function Item_W_Entity (Item        : String;
                              Entity_Name : String) return String
        is (Debug_Trace_Quiet_Instance.Item_W_Entity (Item,
                                                      Entity_Name));

      -----------------------------------------------------

      function E_To_String (Error : Exception_Occurrence) return String
        is (Debug_Trace_Quiet_Instance.E_To_String (Error));

      -----------------------------------------------------

      function Clock_String (Reset_Elapsed : Boolean := False) return String
        is (Debug_Trace_Quiet_Instance.Clock_String (Reset_Elapsed));

      -----------------------------------------------------

      procedure Trace (Item        : String;
                       Entity_Name : String := "") is

      begin

         Ada.Text_IO.Put_Line (if Entity_Name'Length /= 0 then
                                  Item_W_Entity (Item,
                                                 Entity_Name)
                               else
                                  Item);

      end Trace;

      -----------------------------------------------------

      procedure Trace_E (Error       : Exception_Occurrence;
                         Entity_Name : String               := "") is

      begin

         Trace (Item        => E_To_String (Error),
                Entity_Name => Entity_Name);

      end Trace_E;

      -----------------------------------------------------

      procedure Set_Time_Zone (Time_Zone : Time_Offset) is

      begin

         Debug_Trace_Quiet_Instance.Set_Time_Zone (Time_Zone);

      end Set_Time_Zone;

      -----------------------------------------------------

      procedure Set_Local_Time_Zone is

      begin

         Debug_Trace_Quiet_Instance.Set_Local_Time_Zone;

      end Set_Local_Time_Zone;

      -----------------------------------------------------

      procedure Trace_Time (Entity_Name   : String  := "";
                            Reset_Elapsed : Boolean := False) is

      begin

         Trace (Item        => Clock_String (Reset_Elapsed),
                Entity_Name => Entity_Name);

      end Trace_Time;

      -----------------------------------------------------

   end Debug_Trace_Standard;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Standard;
