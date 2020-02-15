-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Ada.Text_IO;

package body Output_Exclam_Impl is

   ----------------------------------------------------------------------------

   protected body Output_Exclam is

      -----------------------------------------------------

      procedure Put_Line (S : String) is

         S_Exclam : constant String := (if S'Length = 0 then
                                           S
                                        else
                                           S(S'First .. S'Last - 1) & '!');

      begin

         Ada.Text_IO.Put_Line (S_Exclam);

      end Put_Line;

      -----------------------------------------------------

   end Output_Exclam;

   ----------------------------------------------------------------------------

end Output_Exclam_Impl;
