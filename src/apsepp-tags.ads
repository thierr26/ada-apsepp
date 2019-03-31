-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Tags;       use Ada.Tags;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Hash;

package Apsepp.Tags is

   function Tag_Hash (T : Tag) return Hash_Type
     is (Ada.Strings.Hash ("." & (if T = No_Tag then
                                     ""
                                  else
                                     Expanded_Name (T))));

end Apsepp.Tags;
