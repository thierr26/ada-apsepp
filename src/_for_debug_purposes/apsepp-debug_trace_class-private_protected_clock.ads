-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Calendar;

private package Apsepp.Debug_Trace_Class.Private_Protected_Clock is

   ----------------------------------------------------------------------------

   protected Clock_Handler is

      procedure Set_Time_Zone (Time_Zone : Time_Offset);

      procedure Get (Date         : out Ada.Calendar.Time;
                     Time_Zone    : out Time_Offset;
                     Elapsed_Time : out Duration;
                     Reset        :     Boolean);

   private

      Offset : Time_Offset := 0;

      Reset_Date : Ada.Calendar.Time := Apsepp.Calendar.Time_First;

   end Clock_Handler;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Private_Protected_Clock;
