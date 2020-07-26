-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Strings.Fixed,
     Ada.IO_Exceptions,
     Apsepp.Generic_Array_Slice_Copy;

package body Apsepp.Text_IO.Generic_Fixed_IO is

   ----------------------------------------------------------------------------

   function Image (X         : Fixed_Point_Type;
                   Fore      : Field            := 0;
                   Aft       : Positive_Field   := Default_Aft;
                   Plus_Kind : Plus_Sign_Kind   := None) return String is

      Buf           : String (1 .. Default_Fore + Aft + 1);
      Non_Blank_Idx : Natural                              := 0;

      function Slice_Copy is new Apsepp.Generic_Array_Slice_Copy
        (Index_Type   => Positive,
         Element_Type => Character,
         Array_Type   => String,
         Diff_Type    => Integer);

      -----------------------------------------------------

      function Fore_Too_Small_Message return String
        is ("'Fore' parameter value ("
              &
            Ada.Strings.Fixed.Trim (Field'Image (Fore), Ada.Strings.Left)
              &
            ") too small for "
              &
            Ada.Strings.Fixed.Trim (Fixed_Point_Type'Image (X),
                                    Ada.Strings.Left));

      -----------------------------------------------------

   begin

      Fixed_Point_Type_IO.Put (To   => Buf,
                               Item => X,
                               Aft  => Aft);

      Non_Blank_Idx := Ada.Strings.Fixed.Index_Non_Blank (Buf);

      if Buf (Non_Blank_Idx) /= Ada.Characters.Latin_1.Minus_Sign then

         case Plus_Kind is
            when Plus   =>
               Non_Blank_Idx      := Non_Blank_Idx - 1;
               Buf(Non_Blank_Idx) := Ada.Characters.Latin_1.Plus_Sign;
            when others =>
               null;
         end case;

      end if;

      if Fore = 0 then
         return Slice_Copy (Buf, Non_Blank_Idx, Buf'Last); -- Early return.
      elsif Fore = Default_Fore then
         return Buf; -- Early return.
      elsif Fore < Default_Fore then
         declare
            First : constant Positive
              := Buf'First + Default_Fore - Fore;
         begin
            return Slice_Copy
                     (Buf,
                      (if Buf(First - 1) = Ada.Characters.Latin_1.Space then
                          First
                       else
                          raise Ada.IO_Exceptions.Layout_Error
                            with Fore_Too_Small_Message),
                      Buf'Last); -- Early return.
         end;
      else
         declare
            Padding : constant String (1 .. Fore - Default_Fore)
              := (others => Ada.Characters.Latin_1.Space);
         begin
            return Padding & Buf; -- Early return.
         end;
      end if;

   end Image;

   ----------------------------------------------------------------------------

   function Fractional_Image
     (X   : Fixed_Point_Type;
      Aft : Positive_Field   := Default_Aft) return String is

      Buf : String (1 .. Default_Fore + Aft + 1);

   begin

      Fixed_Point_Type_IO.Put (To   => Buf,
                               Item => X,
                               Aft  => Aft);

      return Buf(Default_Fore + 1 .. Buf'Last);

   end Fractional_Image;

   ----------------------------------------------------------------------------

end Apsepp.Text_IO.Generic_Fixed_IO;
