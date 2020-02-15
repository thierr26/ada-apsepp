-- Copyright (C) 2019-2020 Thierry Rascle <thierr26@free.fr>
-- MIT license. For more information, please refer to the LICENSE file.

with Apsepp.Generic_Shared_Instance;

with Apsepp.Scope_Bound_Locking; use Apsepp.Scope_Bound_Locking;
with Apsepp.Output_Class;        use Apsepp.Output_Class;

package Apsepp.Output is

   package Output_Shared_Instance is new Generic_Shared_Instance
     (Instance_Ancestor_Type => Output_Interfa,
      Lock_Type              => Lock);

   function Output return access Output_Interfa'Class
     renames Output_Shared_Instance.Instance_Access;

end Apsepp.Output;
