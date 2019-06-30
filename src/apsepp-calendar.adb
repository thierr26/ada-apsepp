-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Calendar.Formatting;

package body Apsepp.Calendar is

   ----------------------------------------------------------------------------

   function To_ISO_8601
     (Date                  : Time;
      Time_Zone             : Time_Offset := 0;
      Include_Time_Fraction : Boolean     := False) return String is

      use Formatting;

      Negative_Offset : Boolean     := Time_Zone < 0;

      Abs_Offset      : Time_Offset := (if Negative_Offset then
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
