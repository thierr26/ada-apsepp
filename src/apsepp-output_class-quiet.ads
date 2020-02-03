-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

package Apsepp.Output_Class.Quiet is

   type Output_Quiet is limited new Output_Interfa with private;

private

   type Output_Quiet is limited new Output_Interfa with null record;

end Apsepp.Output_Class.Quiet;
