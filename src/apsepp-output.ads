-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Output_Class.Interfa; use Apsepp.Output_Class.Interfa;

package Apsepp.Output is

   type Output_Access is access all Output_Interfa'Class;

   function Instance return Output_Access;

   function Output return Output_Access renames Instance;

private

   Instance_Access : Output_Access;

end Apsepp.Output;
