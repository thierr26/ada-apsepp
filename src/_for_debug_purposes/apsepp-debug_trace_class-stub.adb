-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Strings.Fixed;

package body Apsepp.Debug_Trace_Class.Stub is

   ----------------------------------------------------------------------------

   overriding
   function Clock_String
     (Obj           : Debug_Trace_Stub;
      Reset_Elapsed : Boolean          := False) return String is

      use Ada.Strings.Fixed;

      Date         : Time;
      Time_Zone    : Time_Offset;
      Elapsed_Time : Duration;
      Par_Index    : Positive;

      pragma Unreferenced (Obj);

   begin

      Reset_Date_Handler.Get (Date, Time_Zone, Elapsed_Time, Reset_Elapsed);

      return Ret : String := To_ISO_8601 (Date                  => Date,
                                          Time_Zone             => Time_Zone,
                                          Include_Time_Fraction => True)
                             & " ("
                             & Duration'Image (Elapsed_Time)
                             & ")" do

         Par_Index := Index (Ret, "(");
         Ret(Par_Index + 1) := '+';

      end return;

   end Clock_String;

   ----------------------------------------------------------------------------

   overriding
   procedure Trace_E (Obj         : in out Debug_Trace_Stub;
                      E           :        Exception_Occurrence;
                      Entity_Name :        String               := "") is

   begin

      Debug_Trace_Stub'Class (Obj).Trace
        (Debug_Trace_Stub'Class (Obj).E_To_String (E), Entity_Name);

   end Trace_E;

   ----------------------------------------------------------------------------

   overriding
   procedure Set_Time_Zone (Obj       : in out Debug_Trace_Stub;
                            Time_Zone :        Time_Offset) is

      pragma Unreferenced (Obj);

   begin

      Reset_Date_Handler.Set_Time_Zone (Time_Zone);

   end Set_Time_Zone;

   ----------------------------------------------------------------------------

   overriding
   procedure Set_Local_Time_Zone (Obj : in out Debug_Trace_Stub) is

   begin

      Debug_Trace_Stub'Class (Obj).Set_Time_Zone (UTC_Time_Offset);

   exception

      when others => -- Probably Unknown_Zone_Error.
         Debug_Trace_Stub'Class (Obj).Set_Time_Zone (0);

   end Set_Local_Time_Zone;

   ----------------------------------------------------------------------------

   overriding
   procedure Trace_Time
     (Obj           : in out Debug_Trace_Stub;
      Entity_Name   :        String           := "";
      Reset_Elapsed :        Boolean          := False) is

   begin

      Debug_Trace_Stub'Class (Obj).Trace
        (Debug_Trace_Stub'Class (Obj).Clock_String (Reset_Elapsed),
         Entity_Name);

   end Trace_Time;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Stub;
