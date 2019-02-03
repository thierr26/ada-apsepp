-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Output_Class.Interfa; use Apsepp.Output_Class.Interfa;

package Apsepp.Output_Class.Quiet is

   type Output_Quiet is limited new Output_Interfa with private;

private

   type Output_Quiet is limited new Output_Interfa with null record;

end Apsepp.Output_Class.Quiet;
