-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Strings.Fixed,
     Ada.Calendar,
     Apsepp.Calendar,
     Apsepp.Debug_Trace_Class.Private_Protected_Clock;

package body Apsepp.Debug_Trace_Class.Quiet is

   ----------------------------------------------------------------------------

   overriding
   function Clock_String
     (Obj           : Debug_Trace_Quiet;
      Reset_Elapsed : Boolean           := False) return String is

      use Ada.Strings.Fixed,
          Calendar,
          Private_Protected_Clock;

      Date         : Ada.Calendar.Time;
      Time_Zone    : Time_Offset;
      Elapsed_Time : Duration;
      Par_Index    : Positive;

      pragma Unreferenced (Obj);

   begin

      Clock_Handler.Get (Date, Time_Zone, Elapsed_Time, Reset_Elapsed);

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
   procedure Trace_E (Obj         : in out Debug_Trace_Quiet;
                      Error       :        Exception_Occurrence;
                      Entity_Name :        String               := "") is

   begin

      Debug_Trace_Quiet'Class (Obj).Trace
        (Item        => Debug_Trace_Quiet'Class (Obj).E_To_String (Error),
         Entity_Name => Entity_Name);

   end Trace_E;

   ----------------------------------------------------------------------------

   overriding
   procedure Set_Time_Zone (Obj       : in out Debug_Trace_Quiet;
                            Time_Zone :        Time_Offset) is

      use Private_Protected_Clock;

      pragma Unreferenced (Obj);

   begin

      Clock_Handler.Set_Time_Zone (Time_Zone);

   end Set_Time_Zone;

   ----------------------------------------------------------------------------

   overriding
   procedure Set_Local_Time_Zone (Obj : in out Debug_Trace_Quiet) is

   begin

      Debug_Trace_Quiet'Class (Obj).Set_Time_Zone (UTC_Time_Offset);

   exception

      when others => -- Probably 'Unknown_Zone_Error'.
         Debug_Trace_Quiet'Class (Obj).Set_Time_Zone (0);

   end Set_Local_Time_Zone;

   ----------------------------------------------------------------------------

   overriding
   procedure Trace_Time
     (Obj           : in out Debug_Trace_Quiet;
      Entity_Name   :        String           := "";
      Reset_Elapsed :        Boolean          := False) is

      Clock_String : constant String
        := Debug_Trace_Quiet'Class (Obj).Clock_String (Reset_Elapsed);

   begin

      Debug_Trace_Quiet'Class (Obj).Trace (Item        => Clock_String,
                                           Entity_Name => Entity_Name);

   end Trace_Time;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Quiet;
