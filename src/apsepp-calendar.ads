-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
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

   -- TODOC: Returned value can look like on of the following:
   --
   -- - "2020-02-25T16:57:18.92+01:00" (non-null time offset, time fraction
   --   included (28 characters long));
   --
   -- - "2020-02-25T16:57:18+01:00" (non-null time offset, time fraction not
   --   included (25 characters long));
   --
   -- - "2020-02-25T16:57:18.92Z" (null time offset, time fraction included (23
   --   characters long));
   --
   -- - "2020-02-25T16:57:18Z" (null time offset, time fraction not included
   --   (20 characters long)).
   --
   -- The time fraction is always 3 characters long.
   -- REF: ARM9.6.1 86/2. <2020-02-25>
   function To_ISO_8601
     (Date                  : Time;
      Time_Zone             : Time_Offset := Default_Time_Offset;
      Include_Time_Fraction : Boolean     := False) return String
     with Post => To_ISO_8601'Result'Length in 20 | 23 | 25 | 28;

end Apsepp.Calendar;
