-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package body Generic_Char_Count_String is

   ----------------------------------------------------------------------------

   function S return Char_Count_String is

      Char_Count : Char_Count_String := (others => '0');

   begin

      Count := Count + 1;

      Char_Count(Char_Count'First) := Char;
      Char_Count(Char_Count'Last - Suffix'Length + 1
                   ..
                 Char_Count'Last) := Suffix;

      declare
         Image   : constant String  := Natural'Image (Count);
         Padding : constant Integer
           := Char_Count_String_Len - Suffix'Length - Image'Length;
      begin
         Char_Count(Char_Count'First + Padding + 1
                      ..
                    Char_Count'Last - Suffix'Length)
           := Image(Image'First + 1
                      ..
                    Image'Last);
      end;

      return Char_Count;

   end S;

   ----------------------------------------------------------------------------

end Generic_Char_Count_String;
