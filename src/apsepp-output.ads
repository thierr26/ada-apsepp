-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance;
with Apsepp.Output_Class.Interfa; use Apsepp.Output_Class.Interfa;

package Apsepp.Output is

   package Shared_Instance
     is new Apsepp.Generic_Shared_Instance (Output_Interfa);

   use Shared_Instance;

   function Output return Instance_Type_Access renames Instance;

end Apsepp.Output;
