-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO;

package Apsepp.Text_IO is

   subtype Field is Ada.Text_IO.Field;
   subtype Positive_Field is Field range 1 .. Field'Last;

end Apsepp.Text_IO;
