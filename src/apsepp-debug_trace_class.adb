-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package body Apsepp.Debug_Trace_Class is

   ----------------------------------------------------------------------------

   protected body Reset_Date_Handler is

      -----------------------------------------------------

      procedure Get (Date         : out Time;
                     Time_Zone    : out Time_Offset;
                     Elapsed_Time : out Duration;
                     Reset        :     Boolean) is

      begin

         Date      := Clock;
         Time_Zone := Offset;

         if Reset or else Reset_Date = Time_First then
            Reset_Date := Date;
         end if;

         Elapsed_Time := Date - Reset_Date;

      end Get;

      -----------------------------------------------------

      procedure Set_Time_Zone (Time_Zone : Time_Offset) is

      begin

         Offset := Time_Zone;

      end Set_Time_Zone;

      -----------------------------------------------------

   end Reset_Date_Handler;

   ----------------------------------------------------------------------------

end Apsepp.Debug_Trace_Class;
