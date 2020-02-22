-- Copyright (C) 2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Output_Class;

package Output_Exclam_Impl is

   ----------------------------------------------------------------------------

   protected type Output_Exclam is new Apsepp.Output_Class.Output_Interfa with

      overriding
      procedure Put_Line (Item : String);

   end Output_Exclam;

   ----------------------------------------------------------------------------

end Output_Exclam_Impl;
