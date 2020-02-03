-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO;

package body Apsepp.Debug_Trace_Class.Standard is

   ----------------------------------------------------------------------------

   protected body Debug_Trace_Standard is

      -----------------------------------------------------

      function Message_W_Entity (Message     : String;
                                 Entity_Name : String) return String
        is (Debug_Trace_Stub_Instance.Message_W_Entity (Message, Entity_Name));

      -----------------------------------------------------

      function E_To_String (E : Exception_Occurrence) return String
        is (Debug_Trace_Stub_Instance.E_To_String (E));

      -----------------------------------------------------

      function Clock_String (Reset_Elapsed : Boolean := False) return String
        is (Debug_Trace_Stub_Instance.Clock_String (Reset_Elapsed));

      -----------------------------------------------------

      procedure Trace (Message : String; Entity_Name : String := "") is

      begin

         Ada.Text_IO.Put_Line (if Entity_Name'Length /= 0 then
                                  Message_W_Entity (Message, Entity_Name)
                               else
                                  Message);

      end Trace;

      -----------------------------------------------------

      procedure Trace_E (E           : Exception_Occurrence;
                         Entity_Name : String               := "") is

      begin

         Trace (E_To_String (E), Entity_Name);

      end Trace_E;

      -----------------------------------------------------

      procedure Set_Time_Zone (Time_Zone : Time_Offset) is

      begin

         Debug_Trace_Stub_Instance.Set_Time_Zone (Time_Zone);

      end Set_Time_Zone;

      -----------------------------------------------------

      procedure Set_Local_Time_Zone is

      begin

         Debug_Trace_Stub_Instance.Set_Local_Time_Zone;

      end Set_Local_Time_Zone;

      -----------------------------------------------------

      procedure Trace_Time (Entity_Name   : String  := "";
                            Reset_Elapsed : Boolean := False) is

      begin

         Trace (Clock_String (Reset_Elapsed), Entity_Name);

      end Trace_Time;

      -----------------------------------------------------

   end Debug_Trace_Standard;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Standard;
