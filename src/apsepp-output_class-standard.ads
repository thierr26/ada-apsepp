-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Output_Class.Interfa; use Apsepp.Output_Class.Interfa;

package Apsepp.Output_Class.Standard is

   type Output_Standard is limited new Output_Interfa with private;

   overriding
   procedure Put_Line (Obj : Output_Standard; S : String);

private

   type Output_Standard is limited new Output_Interfa with null record;

end Apsepp.Output_Class.Standard;
