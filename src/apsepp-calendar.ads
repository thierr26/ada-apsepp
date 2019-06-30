-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;

package Apsepp.Calendar is

   Time_First : constant Time := Time_Of (Year    => Year_Number'First,
                                          Month   => Month_Number'First,
                                          Day     => Day_Number'First,
                                          Seconds => Day_Duration'First);

   function To_ISO_8601
     (Date                  : Time;
      Time_Zone             : Time_Offset := 0;
      Include_Time_Fraction : Boolean     := False) return String;

end Apsepp.Calendar;
