-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Ada.Text_IO;

package body Apsepp.Output is

   ----------------------------------------------------------------------------

   procedure Put_Line (S : String) is

   begin

      Ada.Text_IO.Put_Line (S);

   end Put_Line;

   ----------------------------------------------------------------------------

end Apsepp.Output;
