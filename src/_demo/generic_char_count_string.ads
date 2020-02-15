-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

generic

   Char : Character;

   Count_Width : Positive := 3;

   Suffix : String := "";

package Generic_Char_Count_String is

   Char_Count_String_Len : constant Positive
     := 1 + Count_Width + Suffix'Length;

   subtype Char_Count_String is String (1 .. Char_Count_String_Len);

   function S return Char_Count_String;

private

   Count : Natural := 0;

end Generic_Char_Count_String;
