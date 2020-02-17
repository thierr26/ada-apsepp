-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Tags;         use Ada.Tags;
with Ada.Containers;   use Ada.Containers;

with Ada.Strings.Hash;
  use Ada.Strings;

package Apsepp.Tags is

   function Total_Expanded_Name (T : Tag) return String
     is ("." & (if T = No_Tag then
                   ""
                else
                   Expanded_Name (T)));

   function "<" (Left, Right : Tag) return Boolean
     is (Total_Expanded_Name (Left) < Total_Expanded_Name (Right));

   function "<=" (Left, Right : Tag) return Boolean
     is (Total_Expanded_Name (Left) <= Total_Expanded_Name (Right));

   function ">" (Left, Right : Tag) return Boolean
     is (Total_Expanded_Name (Left) > Total_Expanded_Name (Right));

   function ">=" (Left, Right : Tag) return Boolean
     is (Total_Expanded_Name (Left) >= Total_Expanded_Name (Right));

   function Tag_Hash (T : Tag) return Hash_Type
     is (Hash (Total_Expanded_Name (T)));

end Apsepp.Tags;
