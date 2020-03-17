-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO;

package body Apsepp.Output_Class.Standard is

   ----------------------------------------------------------------------------

   protected body Output_Standard is

      -----------------------------------------------------

      procedure Put_Line (Item : String) is

      begin

         Ada.Text_IO.Put_Line (Item);

      end Put_Line;

      -----------------------------------------------------

   end Output_Standard;

   ----------------------------------------------------------------------------

end Apsepp.Output_Class.Standard;
