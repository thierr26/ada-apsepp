-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance;
with Apsepp.Output_Class;            use Apsepp.Output_Class;

package Apsepp.Output is

   package Shared_Instance is new Generic_Shared_Instance (Output_Interfa);

   subtype Output_Access is Shared_Instance.Instance_Type_Access;

   function Output return Output_Access renames Shared_Instance.Instance;

end Apsepp.Output;
