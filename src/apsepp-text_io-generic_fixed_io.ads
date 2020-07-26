-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Characters.Latin_1;

generic

   type Fixed_Point_Type is delta <>;

package Apsepp.Text_IO.Generic_Fixed_IO is

   type Plus_Sign_Kind is (Plus, Space, None);

   Default_Fore : constant Positive_Field;
   Default_Aft  : constant Positive_Field;

   -- TODOC: Fore = 0 implies no left padding. <2020-07-26>
   function Image (X         : Fixed_Point_Type;
                   Fore      : Field            := 0;
                   Aft       : Positive_Field   := Default_Aft;
                   Plus_Kind : Plus_Sign_Kind   := None) return String
     with Post => (Fore = 0 or else Image'Result'Length = Fore + Aft + 1)
                    and then
                  Image'Result'First = 1;

   -- TODOC: Includes decimal point. <2020-07-26>
   function Fractional_Image
     (X   : Fixed_Point_Type;
      Aft : Positive_Field   := Default_Aft) return String
     with Post => Fractional_Image'Result'Length = Aft + 1
                    and then
                  Fractional_Image'Result'First = Fixed_Point_Type'Fore + 1
                    and then
                  (
                    Fractional_Image'Result (Fractional_Image'Result'First)
                      =
                    Ada.Characters.Latin_1.Full_Stop
                  );

private

   package Fixed_Point_Type_IO
     is new Ada.Text_IO.Fixed_IO (Num => Fixed_Point_Type);

   Default_Fore : constant Positive_Field := Fixed_Point_Type_IO.Default_Fore;
   Default_Aft  : constant Positive_Field := Fixed_Point_Type_IO.Default_Aft;

end Apsepp.Text_IO.Generic_Fixed_IO;
