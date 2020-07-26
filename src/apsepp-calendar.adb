-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Calendar.Formatting,
     Apsepp.Text_IO.Generic_Fixed_IO;

package body Apsepp.Calendar is

   ----------------------------------------------------------------------------

   Unknown_Time_Zone_Cached_Avail : Boolean := False;
   Unknown_Time_Zone_Cached       : Boolean;

   function Unknown_Time_zone return Boolean is

   begin

      if not Unknown_Time_Zone_Cached_Avail then

         declare
            UTC_Time_Offset_Ret : Time_Offset;
            pragma Unreferenced (UTC_Time_Offset_Ret);
         begin
            UTC_Time_Offset_Ret      := UTC_Time_Offset;
            Unknown_Time_Zone_Cached := False;
         exception
            when Unknown_Zone_Error => Unknown_Time_Zone_Cached := True;
         end;

         Unknown_Time_Zone_Cached_Avail := True;

      end if;

      return Unknown_Time_Zone_Cached;

   end Unknown_Time_zone;

   ----------------------------------------------------------------------------

   function To_ISO_8601
     (Date                 : Time;
      Time_Zone            : Time_Offset := Default_Time_Offset;
      Time_Fraction_Digits : Natural     := 0) return String is

      use Formatting;

      Negative_Offset : constant Boolean     := Time_Zone < 0;

      Abs_Offset      : constant Time_Offset := (if Negative_Offset then
                                                    -Time_Zone
                                                 else
                                                    Time_Zone);

      -----------------------------------------------------

      function Abs_Offset_Image return String is

         Img : constant String          := Image (Duration (Abs_Offset) * 60);
         Ret :          String (1 .. 6) := (1      => (if Negative_Offset then
                                                          '-'
                                                       else
                                                          '+'),
                                            others => <>);

      begin

         Ret(2 .. 6) := Img(Img'First .. Img'First + 4);
         return Ret;

      end Abs_Offset_Image;

      -----------------------------------------------------

      No_Time_Fraction : constant Boolean := Time_Fraction_Digits = 0;

      -- The time fraction that Image outputs when parameter
      -- 'Include_Time_Fraction' is True is with 2 digits.
      -- REF: ARM 9.6.1(82/2). <2020-07-25>
      Standard_Time_Fraction : constant Boolean := Time_Fraction_Digits = 2;

      package Day_Duration_IO
        is new Text_IO.Generic_Fixed_IO (Fixed_Point_Type => Day_Duration);

      Img : String := Image (Date                  => Date,
                             Include_Time_Fraction => Standard_Time_Fraction,
                             Time_Zone             => Time_Zone)
                        &
                      (
                        if No_Time_Fraction or Standard_Time_Fraction then
                           ""
                        else
                           Day_Duration_IO.Fractional_Image
                             (Seconds (Date),
                              Time_Fraction_Digits)
                      )
                        &
                      (
                        if Time_Zone = 0 then
                           "Z"
                        else
                           Abs_Offset_Image
                      );

   begin

      Img(11) := 'T';
      return Img;

   end To_ISO_8601;

   ----------------------------------------------------------------------------

end Apsepp.Calendar;
