-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

package Apsepp.Output_Class.Standard is

   ----------------------------------------------------------------------------

   protected type Output_Standard is new Output_Interfa with

      overriding
      procedure Put_Line (S : String);

   end Output_Standard;

   ----------------------------------------------------------------------------

end Apsepp.Output_Class.Standard;
