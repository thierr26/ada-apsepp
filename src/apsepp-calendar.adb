-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Calendar.Formatting;

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
     (Date                  : Time;
      Time_Zone             : Time_Offset := Default_Time_Offset;
      Include_Time_Fraction : Boolean     := False) return String is

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

      Img : String := Image (Date, Include_Time_Fraction, Time_Zone)
                      & (if Time_Zone = 0 then
                            "Z"
                         else
                            Abs_Offset_Image);

   begin

      Img(11) := 'T';
      return Img;

   end To_ISO_8601;

   ----------------------------------------------------------------------------

end Apsepp.Calendar;
