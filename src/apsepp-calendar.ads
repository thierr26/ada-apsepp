-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;

package Apsepp.Calendar is

   Time_First : constant Time := Time_Of (Year    => Year_Number'First,
                                          Month   => Month_Number'First,
                                          Day     => Day_Number'First,
                                          Seconds => Day_Duration'First);

   function Unknown_Time_zone return Boolean;

   function Default_Time_Offset return Time_Offset
     is (if Unknown_Time_zone then
            0
         else
            UTC_Time_Offset);

   -- TODOC: Example of returned values:
   --
   -- - "2020-02-25T16:57:18.92+01:00" (non-null Time_Offset,
   --   Time_Fraction_Digits = 2);
   --
   -- - "2020-02-25T16:57:18+01:00" (non-null time offset,
   --   Time_Fraction_Digits = 0);
   --
   -- - "2020-02-25T16:57:18.92Z" (null time offset, Time_Fraction_Digits = 2);
   --
   -- - "2020-02-25T16:57:18Z" (null time offset, Time_Fraction_Digits = 0).
   function To_ISO_8601
     (Date                 : Time;
      Time_Zone            : Time_Offset := Default_Time_Offset;
      Time_Fraction_Digits : Natural     := 0) return String
     with Post => To_ISO_8601'Result'Length <= 26 + Time_Fraction_Digits;

end Apsepp.Calendar;
