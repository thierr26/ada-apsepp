-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Apsepp.Debug_Trace_Class.Private_Protected_Clock is

   ----------------------------------------------------------------------------

   protected body Clock_Handler is

      -----------------------------------------------------

      procedure Set_Time_Zone (Time_Zone : Time_Offset) is

      begin

         Offset := Time_Zone;

      end Set_Time_Zone;

      -----------------------------------------------------

      procedure Get (Date         : out Ada.Calendar.Time;
                     Time_Zone    : out Time_Offset;
                     Elapsed_Time : out Duration;
                     Reset        :     Boolean) is

         use type Ada.Calendar.Time;
         use Apsepp.Calendar;

         Elasped_Time_Reset_Needed : constant Boolean
           := Reset                    -- True if caller requires reset.
                or else
              Reset_Date = Time_First; -- True on first 'Get' call.

      begin

         Date      := Ada.Calendar.Clock;
         Time_Zone := Offset;

         if Elasped_Time_Reset_Needed then
            Reset_Date := Date;
         end if;

         Elapsed_Time := Date - Reset_Date;

      end Get;

      -----------------------------------------------------

   end Clock_Handler;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class.Private_Protected_Clock;
