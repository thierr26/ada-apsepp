-- Copyright (C) 2019 Thierry Rascle <thierr26@free.fr>
-- MIT license. Please refer to the LICENSE file.

generic
   type Instance_Type is limited new Instance_Ancestor_Type with private;
   with function Create return Instance_Type;
   with procedure CB is null;
   Just_Pretend : Boolean := False;
package Apsepp.Generic_Shared_Instance.Creator is

   function Has_Actually_Created return Boolean;

end Apsepp.Generic_Shared_Instance.Creator;
